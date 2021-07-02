(define-module (druix versions gerbil-ethereum) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil-ethereum"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      285
      #:ymd
      20210618
      #:hms
      103357
      #:sha256
      "0x9fbgm7pwrqdnn68sa2xynb1gfahzi1c6hqbmhgnbyh9dw807i0"
      #:repo
      "https://github.com/fare/gerbil-ethereum.git"
      #:commit
      "bb6dc2c6390cb12078edbaf762519ff938ac4579")

    (make <druix-version-git>
      #:name
      "gerbil-ethereum"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      283
      #:ymd
      20210603
      #:hms
      13312
      #:sha256
      "0h6fbn8q8iqyq0ynky2nbl7qz126nv9lmayjv234g0vbc8vh9ylz"
      #:repo
      "https://github.com/fare/gerbil-ethereum.git"
      #:commit
      "8e6c1036bdde0d495484bd0d96526405cc1d51a3")

    (make <druix-version-git>
      #:name
      "gerbil-ethereum"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      285
      #:ymd
      20210618
      #:hms
      103357
      #:sha256
      "0x9fbgm7pwrqdnn68sa2xynb1gfahzi1c6hqbmhgnbyh9dw807i0"
      #:repo
      "https://github.com/fare/gerbil-ethereum.git"
      #:commit
      "bb6dc2c6390cb12078edbaf762519ff938ac4579")
))

(define latest (car versions))
