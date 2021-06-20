(define-module (druix versions go-github-com-dlclark-regexp2) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-dlclark-regexp2"
      #:major
      1
      #:minor
      4
      #:patch
      0
      #:revision
      5
      #:ymd
      20201116
      #:hms
      162257
      #:sha256
      "04fv2yzh9jvlzjv9ns94m5kcmkrli6krwjrmvkh40divhxj4rlsl"
      #:repo
      "https://github.com/dlclark/regexp2.git"
      #:commit
      "a2a8dda75c91d84f7daa4154f32385f65b101bf2")
))

(define latest (car versions))
