(define-module (druix packages glow-contacts)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil gerbil-ftw)
  #:use-module (druix packages scheme gerbil smug-gerbil)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil gerbil-libp2p)
  #:use-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix packages scheme gerbil gerbil-persist)
  #:use-module (druix packages scheme gerbil gerbil-ethereum)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages tls)
  #:use-module (druix packages glow-lang)
  #:use-module (druix utils gerbil-packages))

(define-public gloui-spa
  (package
    (name "gloui-spa")
    (version "0.5.0")
    (synopsis "gloUI, a UI for glow, which is a programming language used to make decentralized applications.")
    (description synopsis)
    (home-page "https://github.com/drewc/gloui")
    (license (@ (guix licenses) expat))
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri "https://github.com/drewc/gloui/releases/download/v0.5.0/gloui-SPA-0.5.0.tar.gz")
       (sha256
        (base32 "0v98qrpqdx89g96c95brr1j9pxi70qin8wjskdamsskm1rl0f9lw"))))
   (build-system copy-build-system)
   (arguments
  `(#:phases
     (modify-phases %standard-phases
       (delete 'reset-gzip-timestamps))))))

(define shebang
  '(lambda _
     (display "#!/bin/sh\n")
     (display "ORIG_GERBIL_LOADPATH=\"$GERBIL_LOADPATH\"\n")
     (display "ORIG_GERBIL_PATH=\"$GERBIL_PATH\"\n")
     (display "unset GERBIL_HOME\n")
     (display "GERBIL_LOADPATH=") (write (gerbil-loadpath outputs)) (newline)
     (display "GERBIL_PATH=\"$HOME/.cache/glow/gerbil\"\n")
     (display "export GERBIL_PATH GERBIL_LOADPATH GLOW_SOURCE ORIG_GERBIL_PATH ORIG_GERBIL_LOADPATH\n")
     (display "exec ") (display gerbil) (display "/bin/gxi ") (display out)
     (display "/lib/gerbil/lib/mukn/glow-contacts/main.ss \"$@\"")))

(define-public glow-contacts
  (gxpkg/clan
   (@ (druix versions glow-contacts) latest)
   "Glow Contacts for Decentralized Applications (DApps)"
   "https://gitlab.com/drewc/glow-contacts" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("gerbil-persist" ,gerbil-persist)
     ("gerbil-libp2p" ,gerbil-libp2p)
     ("gerbil-ethereum" ,gerbil-ethereum)
     ("smug-gerbil" ,smug-gerbil)
     ("gerbil-crypto" ,gerbil-crypto)
     ("gerbil-ftw" ,gerbil-ftw)
     ("glow-lang", glow-lang))
   #:clan '(#:software-name "Glow Contacts"
            #:gerbil-package "mukn/glow-contacts"
            #:version-path "version")
   #:arguments
  `(#:phases
     (modify-phases %standard-phases
      ;; (add-before 'copy-source 'patch-glow-install-path)
       (add-after 'build
           'shebang-glow
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out"))
                 (gerbil (assoc-ref inputs "gerbil")))
             (install-file
              "main.ss" (string-append out "/lib/gerbil/lib/mukn/glow-contacts/") )
             #;(make-file-writable "glow-contacts")
             (with-output-to-file "glow-contacts"
               ,shebang)
             (chmod "glow-contacts" #o755)
             (install-file "glow-contacts" (string-append out "/bin"))
             (invoke "echo" "Made a shebang") (newline)
             (invoke "cat" "glow-contacts") (newline)
             #t)))))))
