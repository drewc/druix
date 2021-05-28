(define-module (druix packages scheme gerbil-unstable)
  #:use-module (druix packages scheme gambit-c-unstable)
  #:use-module (druix packages scheme gambit-c-unstable)
  #:use-module ((druix versions gerbil-unstable) #:prefix dvg:)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:use-module (guix derivations)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages databases)
  #:use-module (guix store))

(define unstable-version (car dvg:versions))

(define-public gerbil-unstable
  (let* ((v (v:druix-version unstable-version))
         (c (v:commit unstable-version))
         (s (v:sha256 unstable-version))
         (git-uri (v:repo unstable-version))
         (name "gerbil-unstable")
         (pv (string-append "PACKAGE_VERSION='v" v "'"))
         (configure-flags
          `(,pv "--enable-libxml" "--enable-libyaml" "--enable-zlib"
                "--enable-sqlite" "--enable-mysql" "--enable-lmdb"
                "--enable-leveldb")))
    (package
      (name name)
      (version v)
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference (url git-uri)
                         (commit c)))
         (sha256 (base32 s))
         (file-name (git-file-name name s))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'check)
                  ;(add-before 'build 'make-writable
           (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (invoke "chmod" "-R" "777" ".")
                        ;; (invoke "make configure")
                        ;; (invoke "make stage0")
                        ;; (invoke "make stage1")
                        (setenv "PATH" (string-append (getcwd) "/bin:"
                                                    (getenv "PATH")))
                        ;;    build.nix:
                        ;;  export GERBIL_GXC=$PWD/bin/gxc
                        (setenv "GERBIL_GXC" (string-append
                                              (getcwd) "/bin/gxc"))

                        ;; build.nix:    export GERBIL_BASE=$PWD
                        (setenv "GERBIL_BASE" (getcwd))
                        ;; build.nix:    export GERBIL_HOME=$PWD
                        (setenv "GERBIL_HOME" (getcwd))
                        ;; build.nix:    export GERBIL_PATH=$PWD/lib
                        (setenv "GERBIL_PATH" (getcwd))


                        (invoke "make"))))
           (add-before 'patch-generated-file-shebangs 'fake-gx
               (lambda _
                 (setenv "PATH" (string-append (getcwd) "/bin:"
                                               (getenv "PATH")))
                 (invoke "touch" "bin/gxi")
                 (invoke "chmod" "755" "bin/gxi"    )
                 (invoke "touch" "bin/gxc")
                 (invoke "chmod" "755" "bin/gxc"))))
         #:configure-flags '(,@configure-flags)))

      (inputs `(("gambit-c-unstable" ,gambit-c-unstable)
                ))
      (native-inputs `(("gambit-c-unstable" ,gambit-c-unstable)
                       ("openssl" ,openssl)
                       ("lmdb" ,lmdb)
                       ("leveldb" ,leveldb)
                       ("sqlite" ,sqlite)
                       ("mysql" ,mysql)
                       ("libyaml" ,libyaml)
                       ("libxml2" ,libxml2)
                       ("zlib" ,zlib)))
      (home-page "https://cons.io")
      (license (list l:lgpl2.1+ l:asl2.0))
      (synopsis "A meta-dialect of Scheme with post-modern features")
      (description "Gerbil Scheme

Gerbil is an opinionated dialect of Scheme designed for Systems Programming, with a state of the art macro and module system on top of the Gambit runtime.

The macro system is based on quote-syntax, and provides the full meta-syntactic tower with a native implementation of syntax-case. It also provides a full-blown module system, similar to PLT Scheme's (sorry, Racket) modules. The main difference from Racket is that Gerbil modules are single instantiation, supporting high performance ahead of time compilation and compiled macros."))))
