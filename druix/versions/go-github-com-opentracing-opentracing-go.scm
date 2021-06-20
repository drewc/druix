(define-module (druix versions go-github-com-opentracing-opentracing-go) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-opentracing-opentracing-go"
      #:major
      1
      #:minor
      2
      #:patch
      0
      #:revision
      2
      #:ymd
      20210205
      #:hms
      174328
      #:sha256
      "0pmwf6885cvjxyjvkd3rcqfsbnky8zr1d4ia8k6j69gb9jlkmq6m"
      #:repo
      "https://github.com/opentracing/opentracing-go.git"
      #:commit
      "3088eee7e4d26010ac9a301e977fd2721dbebcbe")
))

(define latest (car versions))
