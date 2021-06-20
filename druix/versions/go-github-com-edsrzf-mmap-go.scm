(define-module (druix versions go-github-com-edsrzf-mmap-go) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-edsrzf-mmap-go"
      #:major
      1
      #:minor
      0
      #:patch
      0
      #:revision
      2
      #:ymd
      20190108
      #:hms
      65903
      #:sha256
      "0vxww5l4wc449f6r1hwyg8k88bf5i8q8s6sjq9jd24yqpva35c84"
      #:repo
      "https://github.com/edsrzf/mmap-go.git"
      #:commit
      "904c4ced31cdffe19e971afa0b3d319ff06d9c72")
))

(define latest (car versions))
