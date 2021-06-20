(define-module (druix versions go-github-com-dop251-goja) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-dop251-goja"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      290
      #:ymd
      20210614
      #:hms
      154742
      #:sha256
      "14f6n4b5h2r5hzhqk5x1rqsqr067y4rfsqmnciphgcncdq1l6n72"
      #:repo
      "https://github.com/dop251/goja.git"
      #:commit
      "14a1ffa82844308972f4374871f2bbc8be62f0ef")
))

(define latest (car versions))
