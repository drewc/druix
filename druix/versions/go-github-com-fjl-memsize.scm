(define-module (druix versions go-github-com-fjl-memsize) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-fjl-memsize"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      47
      #:ymd
      20190710
      #:hms
      130421
      #:sha256
      "0q1ixp3g77slfr3xqag74j42h35ilir1xl0qhxm1hiw2gj49jqfk"
      #:repo
      "https://github.com/fjl/memsize.git"
      #:commit
      "bcb5799ab5e5bd6a0fbd8ba513f95a50d9eac048")
))

(define latest (car versions))
