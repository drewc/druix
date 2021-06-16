(define-module (druix versions gerbil-utils) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git>
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      318
      #:ymd
      20210510
      #:hms
      202318
      #:sha256
      "1mpzs3c7l86s68qdvx08rkdi0n0p3903vbkjw9s8hkqwbnjrp8xh"
      #:repo
      "https://github.com/fare/gerbil-utils.git"
      #:commit
      "806ee9a1e324987f7ac3c927068a26a3c107a090")
))
