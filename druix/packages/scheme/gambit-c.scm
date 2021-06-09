(define-module (druix packages scheme gambit-c)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (guix git-download)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module ((druix versions gambit-c) #:prefix dv:)
  #:use-module (gnu packages scheme)
  #:use-module (guix build utils))


(use-modules (guix packages)
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
        (pver (string-append "PACKAGE_VERSION=v" vver))
        (pstr (string-append "PACKAGE_STRING=Gambit v" vver)))
  ;; (cons* pver pstr
          '("--enable-single-host"
                      "--enable-targets=js,python,ruby,php"
                      "--enable-c-opt=-O1"
                      "--enable-gcc-opts"
                      "--enable-shared"
                      "--enable-absolute-shared-libs"
                      "--enable-openssl")))
 ;;)

(define (make-gambit-c-bootstrap v)
  (package
    (inherit gambit-c-bootstrap)
    (name "gambit-c-bootstrap")
    (version (v:druix-version v))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo v))
             (commit (v:commit v))))
       (file-name (git-file-name "gambit-c-bootstrap" (v:commit v)))
       (sha256 (base32 (v:sha256 v)))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (delete 'install)
          (add-before 'configure 'sub-new-version
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("^PACKAGE_VERSION=.*$")
                ,(string-append "PACKAGE_VERSION=\"v" (v:druix-version v)"\"\n"))
               (("^PACKAGE_STRING=.*$")
                ,(string-append "PACKAGE_STRING=\"Gambit v" (v:druix-version v)"\"\n")))
             (invoke "grep" "-i" "^PACKAGE.*" "configure")))
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
               (substitute* "./boot/configure"
               (("^PACKAGE_VERSION=.*$")
                ,(string-append "PACKAGE_VERSION=\"v" (v:druix-version v)"\"\n"))
               (("^PACKAGE_STRING=.*$")
                ,(string-append "PACKAGE_STRING=\"Gambit v" (v:druix-version v)"\"\n")))
               (invoke "grep" "-i" "^PACKAGE.*" "./boot/configure")

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
               (copy-recursively "./boot/gsc/gsc" (string-append out "/bin/gsc"))

               #t))))
       #:configure-flags '(,@(gambit-c-configure-flags v))))
    (native-inputs `(("boot", gambit-c-bootstrap)
                     ("openssl" ,openssl)
                     ("bash" , bash)))))

(define-public gambit-c-unstable-bootsource
  (package
    (inherit gambit-c)
    (name "gambit-c-bootstrap")
    (version "4.9.3-bootsource")
    (arguments
     '(; #:out-of-source? #t
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'copy-source
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (copy-recursively "." (string-append out "/boot")))
                      #true))
                  ;(delete 'patc    h-source-shebangs)
                  ;(delete 'check)
                  (replace 'build
                    (lambda* (#:key #:allow-other-keys)
                      (invoke "make" "bootstrap")
                      #true)))))))

(define* (make-gambit-c-package
          v
          #:optional (bootstrap (make-gambit-c-bootstrap v)))
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
         (add-before 'configure 'sub-new-version
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("^PACKAGE_VERSION=.*$")
                ,(string-append "PACKAGE_VERSION=\"v" (v:druix-version v)"\"\n"))
               (("^PACKAGE_STRING=.*$")
                ,(string-append "PACKAGE_STRING=\"Gambit v" (v:druix-version v)"\"\n")))
             (invoke "grep" "-i" "^PACKAGE.*" "configure")))
         (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bs (dirname
                                  (string-append (dirname (which "gsi"))
                                                 "../")))
                            (gsc-boot (which "gsc")))
                        (invoke "cat" "makefile")
                      (copy-recursively
                              (string-append bs "/boot/") "boot/")
                      (invoke "chmod" "-R" "u+rw" "./")
                      (invoke "cp" gsc-boot "boot/gsc-boot")
                      (invoke "sh" "-c" "export CONFIG_SHELL=`which bash` ;
 cp config.guess boot; cp config.sub boot; cd boot && \
 rm -f gsc/makefile && \
 cp ../gsc/makefile.in ../gsc/*.scm gsc && ./configure && \
 for i in lib gsi gsc ; do (cd $i ; echo 'making' $i ; find . ; make ) ; done
  ")
                      (invoke "cp" "boot/gsc/gsc" "gsc-boot")
                      (invoke "ls" "gsc-boot" "-l")
                      (invoke "./gsc-boot" "-v")
                      (invoke "make" "bootclean")
                      (invoke "make" "all")
                      #true)))
         )

       '(,@(gambit-c-configure-flags v))))
   (native-inputs `(("gambit-c-bootstrap", gambit-c-unstable-bootsource)
                    ("openssl" ,openssl)
                    ("bash" , bash)))))

(define gambit-c-main-sha1
  "1d5b01330881b3e26345dbaabfd35bbdfae36330")
(define gambit-c-main-hash "17f1zyvs0qazqbqczbsspqrz2vzsabg8kbz2xf1z5x6xxxvkqimc")

(define-public gambit-c-unstable-working
  (let ((v (car dv:versions)))
  (package
    (inherit gambit-c)
    (name "gambit-c-unstable")
    (version "1-unstable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo v))
             (commit (v:commit v))))
       (file-name (git-file-name name (v:commit v)))
       (sha256 (base32 (v:sha256 v)))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'sub-new-version
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "configure"
                        (("^PACKAGE_VERSION=.*$")
                         ,(string-append "PACKAGE_VERSION=\"v" (v:druix-version v)"\"\n"))
                        (("^PACKAGE_STRING=.*$")
                         ,(string-append "PACKAGE_STRING=\"Gambit v" (v:druix-version v)"\"\n")))
                      (invoke "grep" "-i" "^PACKAGE.*" "configure")))
                  ;(delete 'patch-source-shebangs)
                  (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bs (dirname
                                  (string-append (dirname (which "gsi"))
                                                 "../")))
                            (gsc-boot (which "gsc")))
                        (invoke "cat" "makefile")
                      (invoke "rsync" "-rt"
                              (string-append bs "/boot/") "boot/")
                      (invoke "chmod" "-R" "u+rw" "./")
                      (invoke "cp" gsc-boot "boot/gsc-boot")
                      (invoke "sh" "-c" "export CONFIG_SHELL=`which bash` ;
 cp config.guess boot; cp config.sub boot; cd boot && \
 rm -f gsc/makefile && \
 cp ../gsc/makefile.in ../gsc/*.scm gsc && ./configure && \
 for i in lib gsi gsc ; do (cd $i ; echo 'making' $i ; find . ; make ) ; done;
  ")
                      (invoke "cp" "boot/gsc/gsc" "gsc-boot")
                      (invoke "make" "bootclean")
                      (invoke "sh" "-c" "make stamp ; make from-scratch && make modules")
                                        ;; (invoke "make" "all")
                      #true))))
        #:configure-flags '(,@(gambit-c-configure-flags v))))
   (native-inputs `(("unzip" ,unzip)
                    ("rsync" ,rsync)
                    ("boot", gambit-c-unstable-bootsource)
                    ("openssl" ,openssl))))))

(define gambit-c-versions dv:versions)
(define gambit-c-unstable-version (car gambit-c-versions))

(define-public gambit-c-unstable-bootstrap
  (make-gambit-c-bootstrap gambit-c-unstable-version))

(define-public gambit-c-packages (map make-gambit-c-package gambit-c-versions))

(define-public gambit-c-unstable (car gambit-c-packages))
