(define-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils gerbil-packages)
  #:use-module (druix packages scheme gerbil gerbil-utils))

(define-public gerbil-poo
  (gxpkg/clan
   (@ (druix versions gerbil-poo) latest)
    "Gerbil POO: Prototype Object Orientation for Gerbil Scheme"
    "https://github.com/fare/gerbil-poo" (@ (guix licenses) asl2.0)
    `(("gerbil" ,gerbil-unstable)
      ("gerbil-utils" ,gerbil-utils))
    #:clan '(#:software-name "Gerbil-POO"
            #:gerbil-package "clan/poo"
            #:version-path "version")))
