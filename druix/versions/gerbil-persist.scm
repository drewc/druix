(define-module (druix versions gerbil-persist) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil-persist"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      19
      #:ymd
      20210315
      #:hms
      154413
      #:sha256
      "0brjphzlwbyp6i4pn438xdmvima0b602w8wm9712ir1kx2ijamaa"
      #:repo
      "https://github.com/fare/gerbil-persist.git"
      #:commit
      "75d4c45b77faebf0e15a63fca0e1debf21cff3eb")
))

(define latest (car versions))
