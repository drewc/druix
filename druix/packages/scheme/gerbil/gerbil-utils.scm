(define-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix utils)
  #:use-module (druix build-system gerbil)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix git-download))

(define-public gerbil-utils
  (let ((v (car (@ (druix versions gerbil-utils) versions))))

  (package
    (name "gerbil-utils")
    (synopsis "Gerbil Clan: Community curated Collection of Common Utilities")
    (license l:lgpl2.1)
    (description synopsis)
    (home-page "https://github.com/fare/gerbil-utils")
    (version (v:druix-version v))
    (build-system gerbil-build-system)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo v))
             (commit (v:commit v))))
       (file-name (git-file-name name (v:commit v)))
       (sha256 (base32 (v:sha256 v)))))
    (inputs `(("gerbil" ,gerbil-unstable))))))
