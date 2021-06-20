(define-module (druix versions go-ethereum) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-ethereum"
      #:major
      1
      #:minor
      10
      #:patch
      4
      #:revision
      #f
      #:ymd
      20210617
      #:hms
      103517
      #:sha256
      "00lllm4mcbf4ngmy2jsvsp97lzlkdqzyb7v3kq84npg2xn44h68d"
      #:repo
      "https://github.com/ethereum/go-ethereum.git"
      #:commit
      "aa637fd38a379db6da98df0d520fb1c5139a18ce")
))

(define latest (car versions))
