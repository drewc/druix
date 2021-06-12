(define-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gambit-c)
  #:use-module (druix utils)
  #:use-module (gnu packages scheme)
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages databases)
  #:use-module (guix store))

#;(define unstable-version (car dvg:versions))

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
      (inputs `(("gambit-c-unstable" ,gambit-c-unstable)))
      (native-inputs `(("gambit-c-unstable" ,gambit-c-unstable)
                       ("openssl" ,openssl)
                       ("lmdb" ,lmdb)
                       ("leveldb" ,leveldb)
                       ("sqlite" ,sqlite)
                       ("mysql" ,mysql)
                       ("libyaml" ,libyaml)
                       ("libxml2" ,libxml2)
                       ("zlib" ,zlib))))))


(define-public gerbil-packages
  (map make-gerbil-package (@ (druix versions gerbil) versions)))
(for-each export-package gerbil-packages)

(define-public gerbil-unstable (car gerbil-packages))
