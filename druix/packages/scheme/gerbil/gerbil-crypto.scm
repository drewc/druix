(define-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages pkg-config)
  #:use-module (druix utils gerbil-packages))

(define-public gerbil-crypto
  (gxpkg/clan
   (@ (druix versions gerbil-crypto) latest)
   "Gerbil Crypto: Extra Cryptographic Primitives for Gerbil"
   "https://github.com/fare/gerbil-crypto" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("libsecp256k1", libsecp256k1)
     ("pkg-config", pkg-config))
   #:clan '(#:software-name "Gerbil-crypto"
            #:gerbil-package "clan/crypto"
            #:version-path "version")))
