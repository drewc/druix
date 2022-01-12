(define-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gambit-c)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages linux)
  #:use-module (druix utils)
  #:use-module ((druix versions gerbil) #:prefix dvg:)
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
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages databases)
  #:use-module (guix store))


(define-public gerbil
  (package
    (name "gerbil")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vyzo/gerbil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xzi9mhrmzcajhlz5qcnz4yjlljvbkbm9426iifgjn47ac0965zw"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (invoke "chmod" "755" "-R" ".")
             ;; Otherwise fails when editing an r--r--r-- file.
             (invoke "gsi-script" "configure"
                     "--prefix" (assoc-ref outputs "out")
                     "--with-gambit" (assoc-ref inputs "gambit-c"))))
         (add-before 'patch-generated-file-shebangs 'fix-gxi-shebangs
           (lambda _
             ;; Some .ss files refer to gxi using /usr/bin/env gxi
             ;; and 'patch-generated-file-shebangs can't fix that
             ;; because gxi has not been compiled yet.
             ;; We know where gxi is going to end up so we
             ;; Doctor Who our fix here before the problem
             ;; happens towards the end of the build.sh script.
             (let ((abs-srcdir (getcwd)))
               (for-each
                (lambda (f)
                   (substitute* f
                     (("#!/usr/bin/env gxi")
                      (string-append "#!" abs-srcdir "/../bin/gxi"))))
                 '("./gerbil/gxc"
                   "./lang/build.ss"
                   "./misc/http-perf/build.ss"
                   "./misc/rpc-perf/build.ss"
                   "./misc/scripts/docsnarf.ss"
                   "./misc/scripts/docstub.ss"
                   "./misc/scripts/docsyms.ss"
                   "./r7rs-large/build.ss"
                   "./release.ss"
                   "./std/build.ss"
                   "./std/run-tests.ss"
                   "./std/web/fastcgi-test.ss"
                   "./std/web/rack-test.ss"
                   "./tools/build.ss"
                   "./tutorial/httpd/build.ss"
                   "./tutorial/kvstore/build.ss"
                   "./tutorial/lang/build.ss"
                   "./tutorial/proxy/build-static.ss"
                   "./tutorial/proxy/build.ss")))
             #t))
         (replace
          'build
          (lambda*
           (#:key inputs #:allow-other-keys)
           (setenv "HOME" (getcwd))
             (invoke
              ;; The build script needs a tty or it'll crash on an ioctl
              ;; trying to find the width of the terminal it's running on.
              ;; Calling in script prevents that.
              "script"
              "-qefc"
              "./build.sh")))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib")))
               (mkdir-p bin)
               (mkdir-p lib)
               (copy-recursively "../bin" bin)
               (copy-recursively "../lib" lib)))))))
    (native-inputs
     `(("coreutils" ,coreutils)
       ("util-linux" ,util-linux)))
    (propagated-inputs
     `(("gambit-c" ,gambit-c)
       ("zlib" ,zlib)
       ("openssl" ,openssl)
       ("sqlite" ,sqlite)))
    (build-system gnu-build-system)
    (synopsis "Meta-dialect of Scheme with post-modern features")
    (description "Gerbil is an opinionated dialect of Scheme designed for Systems
Programming, with a state of the art macro and module system on top of the Gambit
runtime.  The macro system is based on quote-syntax, and provides the full meta-syntactic
tower with a native implementation of syntax-case.  It also provides a full-blown module
system, similar to PLT Scheme's (sorry, Racket) modules.  The main difference from Racket
is that Gerbil modules are single instantiation, supporting high performance ahead of
time compilation and compiled macros.")
    (home-page "https://cons.io")
    (license `(,l:lgpl2.1 ,l:asl2.0))))

(define (make-gerbil-unstable-configure-form flags)
  `(lambda* (#:key build target native-inputs inputs outputs
             (configure-flags '()) out-of-source?
             #:allow-other-keys)
     (let* ((gambc (with-directory-excursion
                    (string-append (dirname (which "gsc"))
                                   "/../")
                    (getcwd)))
            (with-g (string-append "--with-gambit=" gambc))
            (conflags (list with-g ,@flags))
            (conf (assoc-ref %standard-phases 'configure)))
       (conf #:build build #:target target #:native-inputs native-inputs
             #:inputs inputs #:outputs outputs
             #:configure-flags conflags))))

(define gerbil-unstable-before-build-form
  '(lambda _
     (invoke "chmod" "-R" "777" ".")
     (setenv "PATH" (string-append (getcwd) "/bin:" (getenv "PATH")))
     (setenv "GERBIL_GXC" (string-append (getcwd) "/bin/gxc"))
     (setenv "GERBIL_BASE" (getcwd))
     (setenv "GERBIL_HOME" (getcwd))
     (setenv "GERBIL_PATH" (getcwd))
     #t))

;;; Gerbil uses itself to compile itself. Because we patch #!/usr/bin/env, and
;;; some of what are called "generated-file"'s rely on a shebang that works, we
;;; fake them here.

(define gerbil-unstable-fake-/bin
  '(lambda _
     (setenv "PATH"
             (string-append (getcwd) "/bin:" (getenv "PATH")))
     (for-each (lambda (exe)
                 (invoke "touch" exe) (invoke "chmod" "755" exe))
               '("bin/gxi" "bin/gxi-script" "bin/gxc"))
  #t))

(define* (make-gerbil-package version #:optional (name "gerbil-unstable"))
   (let* ((v (v:druix-version version))
         (c (v:commit version))
         (s (v:sha256 version))
         (git-uri (v:repo version))
         (pv (string-append "PACKAGE_VERSION=v" v ""))
         (configure-flags
          `(,pv "--enable-libxml" "--enable-libyaml" "--enable-zlib"
                "--enable-sqlite" "--enable-mysql" "--enable-lmdb"
                "--enable-leveldb")))
    (package
      (inherit gerbil)
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
           (replace 'configure
             ,(make-gerbil-unstable-configure-form configure-flags))
          (add-before 'build 'set-build-environment
            ,gerbil-unstable-before-build-form)
          (add-before 'patch-generated-file-shebangs 'fake-bin
            ,gerbil-unstable-fake-/bin))))
      (propagated-inputs `(("gambit-c-unstable" ,gambit-c-unstable)
                #;("gcc-toolchain" ,gcc-toolchain)
                #;("linux-headers" ,linux-libre-headers)))
      (native-inputs `(("gambit-c-unstable" ,gambit-c-unstable)
                       ("openssl" ,openssl)
                       ("lmdb" ,lmdb)
                       ("leveldb" ,leveldb)
                       ("sqlite" ,sqlite)
                       ("mysql" ,mysql)
                       ("libyaml" ,libyaml)
                       ("libxml2" ,libxml2)
                       ("zlib" ,zlib))  ))))


(define-public gerbil-packages
  (map make-gerbil-package (@ (druix versions gerbil) versions)))
(for-each export-package gerbil-packages)

(define-public gerbil-unstable (car gerbil-packages))
