(define-module (druix packages glow-lang)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil smug-gerbil)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil gerbil-libp2p)
  #:use-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix packages scheme gerbil gerbil-persist)
  #:use-module (druix packages scheme gerbil gerbil-ethereum)
  #:use-module (gnu packages tls)
  #:use-module (druix utils gerbil-packages))

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
     (display "/lib/gerbil/lib/mukn/glow/main.ss \"$@\"")))

(define-public glow-lang
  (gxpkg/clan
   (@ (druix versions glow-lang) latest)
   "Glow: language for safe Decentralized Applications (DApps)"
   "https://github.com/fare/gerbil-ethereum" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("gerbil-persist" ,gerbil-persist)
     ("gerbil-libp2p" ,gerbil-libp2p)
     ("gerbil-ethereum" ,gerbil-ethereum)
     ("smug-gerbil" ,smug-gerbil)
     ("gerbil-crypto" ,gerbil-crypto))
   #:clan '(#:software-name "Glow"
            #:gerbil-package "mukn/glow"
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
              "main.ss" (string-append out "/lib/gerbil/lib/mukn/glow/") )
             (make-file-writable "glow")
             (with-output-to-file "glow"
               ,shebang)
             (chmod "glow" #o755)
             (install-file "glow" (string-append out "/bin"))
             (invoke "echo" "Made a shebang") (newline)
             (invoke "cat" "glow") (newline)
             #t)))))))
