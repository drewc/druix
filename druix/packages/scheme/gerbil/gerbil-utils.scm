(define-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils gerbil-packages)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix l:))

(define-public gerbil-utils
  (gxpkg/clan
   (@ (druix versions gerbil-utils) latest)
   "Gerbil Clan: Community curated Collection of Common Utilities"
   "https://github.com/fare/gerbil-utils" l:lgpl2.1
   `(("gerbil" ,gerbil-unstable)
     ("openssl" ,openssl))
   #:clan '(#:software-name "Gerbil-utils"
            #:gerbil-package "clan"
            #:version-path "version")))
