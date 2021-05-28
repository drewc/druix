(define-module (druix packages scheme gambit-c-unstable)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
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

(define-public gambit-c-unstable-bootsource
  (package
    (inherit gambit-c)
    (name "gambit-c-unstable")
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

(define gambit-c-main-sha1
  "1d5b01330881b3e26345dbaabfd35bbdfae36330")
(define gambit-c-main-hash "17f1zyvs0qazqbqczbsspqrz2vzsabg8kbz2xf1z5x6xxxvkqimc")
(define (gambc-configure-flags)
  '("--enable-single-host"
    "--enable-targets=js"
    "--enable-c-opt=-O1"
    "--enable-gcc-opts"
    "--enable-shared"
    "--enable-abolute-shared-libs"
    "--enable-openssl"
    ))

(define-public gambit-c-unstable
  (package
    (inherit gambit-c)
    (name "gambit-c-unstable")
    (version "unstable")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gambit/gambit.git")
             (commit gambit-c-main-sha1)))
       (file-name (git-file-name name gambit-c-main-sha1))
       (sha256 (base32 gambit-c-main-hash))))
    (arguments
     `(; #:out-of-source? #t
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'sub-new-version
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "configure"
                        (("^PACKAGE_VERSION=.*$")
                         (string-append "PACKAGE_VERSION=\"v.4.9.3-" "1d5b01330" "\"\n"))
                        (("^PACKAGE_STRING=.*$")
                         (string-append "PACKAGE_STRING=\"Gambit v.4.9.3-" "1d5b01330" "\"\n")))
                      ))
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
 for i in lib gsi gsc ; do (cd $i ; echo 'making' $i ; find . ; make ) ; done
  ")
                      (invoke "cp" "boot/gsc/gsc" "gsc-boot")
                      (invoke "ls" "gsc-boot" "-l")
                      (invoke "./gsc-boot" "-v")
                      (invoke "make" "bootclean")
                      (invoke "make" "all")
                      #true))))
       #:configure-flags '("--enable-single-host"
                           "--enable-targets=js"
                           "--enable-c-opt=-O1"
    "--enable-gcc-opts"
    "--enable-shared"
    "--enable-abolute-shared-libs"
    "--enable-openssl"

                           )
      ))
   (native-inputs `(("unzip" ,unzip)
                    ("rsync" ,rsync)
                    ("boot", gambit-c-unstable-bootsource)
                    ("openssl" ,openssl)))))
