(define-module (druix packages scheme gerbil gerbil-ftw)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils gerbil-packages)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module ((guix licenses) #:prefix l:))

(define-public gerbil-ftw
  (gxpkg
   (@ (druix versions gerbil-ftw) latest)
   "For The Web!: A lot of gerbil scheme web app thingies"
   "https://github.com/drewc/ftw"
   l:expat
   `(("gerbil", gerbil-unstable)
     ("gerbil-utils", gerbil-utils))))
