(define-module (druix packages scheme gerbil smug-gerbil)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils gerbil-packages)
  #:use-module ((guix licenses) #:prefix l:))

(define-public smug-gerbil
  (gxpkg
   (@ (druix versions smug-gerbil) latest)
   "Super Monadic Über Go-into : Parsers for Gerbil Scheme"
   "https://github.com/drewc/smug-gerbil"
   (l:fsf-free "https://mit-license.org/")
   `(("gerbil" ,gerbil-unstable))))
