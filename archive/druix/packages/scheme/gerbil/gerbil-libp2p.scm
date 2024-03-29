(define-module (druix packages scheme gerbil gerbil-libp2p)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils gerbil-packages)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix l:))

(define-public gerbil-libp2p
  (gxpkg
   (@ (druix versions gerbil-libp2p) latest)
   "Gerbil libp2p: use libp2p from Gerbil"
   "https://github.com/vyzo/gerbil-libp2p.git"
   l:expat
   `(("gerbil" ,gerbil-unstable)
     ("openssl" ,openssl))))
