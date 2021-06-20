(define-module (druix versions go-github-com-hashicorp-golang-lru) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-hashicorp-golang-lru"
      #:major
      0
      #:minor
      5
      #:patch
      4
      #:revision
      2
      #:ymd
      20210104
      #:hms
      140557
      #:sha256
      "0rvnxv3vphzmh59p6ddir11rzp7lrn6rizhmyx2rm9vvqhnav6y3"
      #:repo
      "https://github.com/hashicorp/golang-lru.git"
      #:commit
      "80c98217689d6df152309d574ccc682b21dc802c")
))

(define latest (car versions))
