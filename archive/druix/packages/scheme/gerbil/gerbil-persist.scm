(define-module (druix packages scheme gerbil gerbil-persist)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix utils gerbil-packages))

(define-public gerbil-persist
  (gxpkg/clan
   (@ (druix versions gerbil-persist) latest)
   "Gerbil Persist: Persistent data and activities"
   "https://github.com/fare/gerbil-persist" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("gerbil-crypto" ,gerbil-crypto))
   #:clan '(#:software-name "Gerbil-persist"
            #:gerbil-package "clan/persist"
            #:version-path "version")))
