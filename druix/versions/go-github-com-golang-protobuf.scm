(define-module (druix versions go-github-com-golang-protobuf) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-golang-protobuf"
      #:major
      1
      #:minor
      5
      #:patch
      2
      #:revision
      #f
      #:ymd
      20210329
      #:hms
      182059
      #:sha256
      "1mh5fyim42dn821nsd3afnmgscrzzhn3h8rag635d2jnr23r1zhk"
      #:repo
      "https://github.com/golang/protobuf.git"
      #:commit
      "ae97035608a719c7a1c1c41bed0ae0744bdb0c6f")
))

(define latest (car versions))
