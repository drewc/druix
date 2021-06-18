(define-module (druix packages scheme gambit-c)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (guix git-download)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module ((druix versions gambit-c) #:prefix dv:)
  #:use-module (gnu packages scheme))


#;(use-modules (guix packages)
             (guix download)
             (guix build utils)
             (gnu packages scheme)
             (gnu packages tls)
             (gnu packages rsync)
             (gnu packages version-control)
             (gnu packages compression))

(define-public gambit-c-bootstrap
  (package
    (inherit gambit-c)
    (name "gambit-c-bootstrap")
    (version "4.9.3")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Let's copy over the configured source JIT
         (add-before 'build 'copy-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (copy-recursively "." out))
             #t))
         ;; and make bootstrap
         (replace 'build
           (lambda* (#:key #:allow-other-keys)
             (invoke "make" "bootstrap")
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (copy-recursively "." out))
             #t)))))))
(define (gambit-c-configure-flags v)
 (let* ((ver (v:druix-version v))
        (vver (string-append "v" ver))
        (pver (string-append "PACKAGE_VERSION=" vver))
        (pstr (string-append "PACKAGE_STRING=Gambit " vver)))
   (cons* pver pstr
          '("--enable-single-host"
            "--enable-targets=js,python,ruby,php"
            "--enable-c-opt=-O1"
            "--enable-gcc-opts"
            "--enable-shared"
            "--enable-absolute-shared-libs" "--enable-openssl"))))

(define (make-gambit-c-bootstrap v)
  (package
    (inherit gambit-c-bootstrap)
    (name "gambit-c-unstable-bootstrap")
    (version (v:druix-version v))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo v))
             (commit (v:commit v))))
       (file-name (git-file-name "gambit-c-unstable-bootstrap" (v:commit v)))
       (sha256 (base32 (v:sha256 v)))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (delete 'install)
         (replace 'build
           (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
             (let ((bootroot (assoc-ref inputs "boot"))
                   (bsh (string-append
                         (assoc-ref (or native-inputs inputs) "bash")
                         "/bin/bash"))
                   (out (assoc-ref outputs "out")))

               ;; Copy over the "release" bootrap
               (invoke "chmod" "-R" "u+rw" "./")
               (copy-recursively bootroot "./boot")
               (invoke "chmod" "-R" "u+rw" "./boot")
               ;; Replace the conf and make with our versions
               (copy-recursively "config.guess" "./boot/config.guess")
               (copy-recursively "config.sub" "./boot/config.sub")
               (copy-recursively "gsc/makefile.in" "./boot/gsc/makefile.in")

               ;; make sure we rebuild gsc
               (delete-file-recursively "./boot/gsc/gsc")
               (invoke "echo" "Reconf for boot/configure\n\n\n\n")
               ;; Copy over the new `gsc` files to build with.
               (for-each (lambda (scm)
                           (copy-recursively
                            scm (string-append"./boot/" scm)))
                         (find-files "./gsc" "\\.scm$"))

               ;; For make use a shell as the makefile relies on certain things.
               (invoke bsh "-c" (string-append "export CONFIG_SHELL=" bsh
                                                "; cd boot && ./configure && \
      for i in lib gsi gsc ; do (cd $i ; echo 'making' $i ; find . ; make ) ; done \n"))
               (copy-recursively "./boot" out)

               #t))))
       #:configure-flags '(,@(gambit-c-configure-flags v))))
    (native-inputs `(("boot", gambit-c-bootstrap)
                     ("openssl" ,openssl)
                     ("bash" , bash)))))
(define (stamp.h v)
  (with-output-to-string
    (lambda ()
      (display "/* Automatically generated */

#ifndef ___STAMP_VERSION
#define ___STAMP_VERSION ")
      (write (string-append "v" (v:druix-version v)))
      (display "
#endif

#ifndef ___STAMP_YMD
#define ___STAMP_YMD ")
      (write (v:ymd v))
      (display "
#endif

#ifndef ___STAMP_HMS
#define ___STAMP_HMS ")
      (write (v:hms v))
      (display "
#endif
"))))

(define* (make-gambit-c-package
          v #:optional (bootstrap (make-gambit-c-bootstrap v)))
  (package
    (inherit gambit-c)
    (name "gambit-c-unstable")
    (version (v:druix-version v))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo v))
             (commit (v:commit v))))
       (file-name (git-file-name name (v:commit v)))
       (sha256 (base32 (v:sha256 v)))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-stamp.h
                    (lambda _
                      (substitute* "include/makefile.in"
                        (("echo > stamp.h;")
                          "echo \"Actually, non, we make one for guix!\"; cat stamp.h;"))
         
                        (invoke "chmod" "-R" "u+rw" "./include")
                      (with-output-to-file "include/stamp.h"
                        (lambda () (display ,(stamp.h v))))
                      (invoke "echo" "Made an include/stamp.h")
                      (invoke "cat" "include/stamp.h")))
         (replace 'build
           (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
                      (let ((bootroot (assoc-ref (or native-inputs inputs) "gambit-c-bootstrap")))
                      (invoke "chmod" "-R" "u+rw" "./")
                      (copy-recursively bootroot "boot/")
                      (invoke "chmod" "-R" "u+rw" "./")
                      (invoke "cp" "boot/gsc/gsc" "gsc-boot")
                      (invoke "make" "bootclean")
                      (invoke "sh" "-c" "make stamp ; make from-scratch && make modules")
                      #true))))

       #:configure-flags '(,@(gambit-c-configure-flags v))))
    #;(inputs `(("gcc-toolchain" ,gcc-toolchain)
              ("linux-headers" ,linux-libre-headers)))
    (native-inputs `(("gambit-c-bootstrap", bootstrap)
                     ("openssl" ,openssl)))))

(define gambit-c-versions dv:versions)
(define gambit-c-unstable-version (car gambit-c-versions))

#;(define-public gambit-c-unstable-bootstrap
  (make-gambit-c-bootstrap gambit-c-unstable-version))

(define-public gambit-c-packages (map make-gambit-c-package gambit-c-versions))

(define-public gambit-c-unstable (car gambit-c-packages))

(define (exsym pkg)
  (string->symbol (string-append "gambit-unstable-" (package-version pkg))))

(define (modsym pkg)
  (define sym (exsym pkg))
  (module-define! (current-module) sym pkg)
  (eval `(export ,sym) (interaction-environment)))

(for-each modsym gambit-c-packages)
