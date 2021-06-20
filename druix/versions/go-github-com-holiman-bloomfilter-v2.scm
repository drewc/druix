(define-module (druix versions go-github-com-holiman-bloomfilter-v2) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-holiman-bloomfilter-v2"
      #:major
      2
      #:minor
      0
      #:patch
      3
      #:revision
      1
      #:ymd
      20210107
      #:hms
      80224
      #:sha256
      "0a1q0f09vvy1wxsixga9djgv5b8in77gjmmh041g4zy741hxza9v"
      #:repo
      "https://github.com/holiman/bloomfilter.git"
      #:commit
      "59a86729025f265874bc744ca32f2535341b0291")
))

(define latest (car versions))
