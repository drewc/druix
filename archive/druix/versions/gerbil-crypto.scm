(define-module (druix versions gerbil-crypto) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil-crypto"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      16
      #:ymd
      20210510
      #:hms
      215637
      #:sha256
      "1k4bidi4anqwg0l58qzyqzgcwynhbas2s592fm7pga4akisbqzlz"
      #:repo
      "https://github.com/fare/gerbil-crypto.git"
      #:commit
      "4c7c4a852b1a13af91ba2b7435218cd9dd8b8b6e")
))

(define latest (car versions))
