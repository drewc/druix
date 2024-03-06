(define-module (druix packages scheme gambit-c)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages commencement)
  #:use-module (guix git-download)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module ((druix versions gambit-c) #:prefix dv:)
  #:use-module (gnu packages scheme))

(define-public gambit-c
  (package
    (name "gambit-c")
    (version "4.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://gambitscheme.org/latest/gambit-v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "025x8zi9176qwww4d3pk8aj9ab1fpqyxqz26q3v394k6bfk49yqr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; According to the ./configure script, this makes the build slower and
       ;; use >= 1 GB memory, but makes Gambit much faster.
       '("--enable-single-host")))
    (home-page "http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page")
    (synopsis "Efficient Scheme interpreter and compiler")
    (description
     "Gambit consists of two main programs: gsi, the Gambit Scheme
interpreter, and gsc, the Gambit Scheme compiler.  The interpreter contains
the complete execution and debugging environment.  The compiler is the
interpreter extended with the capability of generating executable files.  The
compiler can produce standalone executables or compiled modules which can be
loaded at run time.  Interpreted code and compiled code can be freely
mixed.")
    ;; Dual license.
    (license (list lgpl2.1+ asl2.0))))

(define-public gambit-c-bootstrap
  (package
    (inherit gambit-c)
    (name "gambit-c-bootstrap")
    (version "4.9.4")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Let's copy over the configured source JIT
         (add-before 'build 'copy-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/" ,name "/" ,version)))
               (copy-recursively "." share))
             #t))
         ;; and make bootstrap
         (replace 'build
           (lambda* (#:key #:allow-other-keys)
             (invoke "make" "bootstrap")
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/" ,name "/" ,version)))
               (copy-recursively "." share))
             #t)))))))
(define (gambit-c-configure-flags v)
 (let* ((ver (v:druix-version v))
        (vver (string-append "v" ver))
        (pver (string-append "PACKAGE_VERSION=" vver))
        (pstr (string-append "PACKAGE_STRING=Gambit " vver)))
   (cons* pver pstr
          '("--enable-single-host" "--enable-openssl" "LIBS=-lpthread"))))

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
             (let ((bootroot
                    (string-append
                     (assoc-ref inputs "gambit-c-bootstrap")
                     "/share/gambit-c-bootstrap/4.9.4"))
                   (bsh (string-append
                         (assoc-ref (or native-inputs inputs) "bash")
                         "/bin/bash"))
                   (out (string-append
                         (assoc-ref outputs "out")
                         "/share/",name  "/" ,version)))

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
    (native-inputs `(("gambit-c-bootstrap", gambit-c-bootstrap)
                     ("openssl:static" ,openssl "static")
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
          v #:optional (bootstrap gambit-c-unstable-bootstrap))
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
             (let ((bootroot
                    (string-append
                     (assoc-ref (or native-inputs inputs)
                                "gambit-c-unstable-bootstrap")
                     "/share/gambit-c-unstable-bootstrap/" ,version)))
               (invoke "chmod" "-R" "u+rw" "./")
               (copy-recursively bootroot "boot/")
               (invoke "chmod" "-R" "u+rw" "./")
               (invoke "cp" "boot/gsc/gsc" "gsc-boot")
               (invoke "make" "bootclean")
               (invoke "sh" "-c" "make stamp ; make from-scratch && make modules")
               #true))))

       #:configure-flags '(,@(gambit-c-configure-flags v))))
    (native-inputs `(("gambit-c-unstable-bootstrap", bootstrap)
                     ("openssl:static" ,openssl "static")
                     ("openssl" ,openssl)
                     ("texinfo" ,texinfo)
                     ("texi2html" ,texi2html)))))

(define gambit-c-unstable-version dv:latest)

(define-public gambit-c-unstable-bootstrap
  (make-gambit-c-bootstrap gambit-c-unstable-version))

#;(define-public gambit-c-packages (map make-gambit-c-package gambit-c-versions))

(define-public gambit-c-unstable (make-gambit-c-package gambit-c-unstable-version))

#;(define (exsym pkg)
  (string->symbol (string-append "gambit-unstable-" (package-version pkg))))

#;(define (modsym pkg)
  (define sym (exsym pkg))
  (module-define! (current-module) sym pkg)
  (eval `(export ,sym) (interaction-environment)))

#;(for-each modsym gambit-c-packages)
