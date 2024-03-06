(define-module (druix packages scheme gerbil gerbil-ethereum)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix packages scheme gerbil gerbil-persist)
  #:use-module (druix utils gerbil-packages))

(define shebang
  '(lambda _
     (display "#!/bin/sh\n#|\n")
     (display "ORIG_GERBIL_LOADPATH=\"$GERBIL_LOADPATH\"\n")
     (display "ORIG_GERBIL_PATH=\"$GERBIL_PATH\"\n")
     (display "unset GERBIL_HOME\n")
     (display "GERBIL_LOADPATH=") (write (gerbil-loadpath outputs)) (newline)
     (display "GERBIL_PATH=\"$HOME/.cache/gerbil-ethereum/gerbil\"\n")
     (display "export GERBIL_PATH GERBIL_LOADPATH GLOW_SOURCE ORIG_GERBIL_PATH ORIG_GERBIL_LOADPATH\n")
     (display "exec ") (display gerbil) (display "/bin/gxi \"$0\" \"$@\"\n|#\n")
     (display "(import :mukn/ethereum/scripts/run-ethereum-test-net :clan/multicall)\n")
     (display "(apply call-entry-point (cdr (command-line)))\n")))

(define-public gerbil-ethereum
  (gxpkg/clan
   (@ (druix versions gerbil-ethereum) latest)
   "Gerbil Ethereum: Ethereument data and activities"
   "https://github.com/fare/gerbil-ethereum" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("gerbil-persist" ,gerbil-persist)
     ("gerbil-crypto" ,gerbil-crypto))
   #:clan '(#:software-name "Gerbil-ethereum"
            #:gerbil-package "mukn/ethereum"
            #:version-path "version")
   #:arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-after 'build
           'shebang-run-ethereum-test-net
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out"))
                 (gerbil (assoc-ref inputs "gerbil")))
             (copy-recursively
              "./scripts"
              (string-append out "/lib/gerbil/lib/mukn/ethereum/scripts"))
             (with-output-to-file "run-ethereum-test-net"
               ,shebang)
             (chmod "run-ethereum-test-net" #o755)
             (install-file "run-ethereum-test-net" (string-append out "/bin"))
             (invoke "echo" "Made a shebang") (newline)
             (invoke "cat" "run-ethereum-test-net") (newline)
             #t)))))))
