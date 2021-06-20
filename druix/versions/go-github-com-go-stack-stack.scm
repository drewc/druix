(define-module (druix versions go-github-com-go-stack-stack) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-go-stack-stack"
      #:major
      1
      #:minor
      8
      #:patch
      0
      #:revision
      #f
      #:ymd
      20180826
      #:hms
      134848
      #:sha256
      "0wk25751ryyvxclyp8jdk5c3ar0cmfr8lrjb66qbg4808x66b96v"
      #:repo
      "https://github.com/go-stack/stack.git"
      #:commit
      "2fee6af1a9795aafbe0253a0cfbdf668e1fb8a9a")
))

(define latest (car versions))
