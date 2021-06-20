(define-module (druix versions go-github-com-deckarep-golang-set) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-deckarep-golang-set"
      #:major
      1
      #:minor
      7
      #:patch
      1
      #:revision
      2
      #:ymd
      20201129
      #:hms
      21324
      #:sha256
      "1nxbc85qidkhcy9m6x280w9z943184195afcp42w5xz0m2b26w01"
      #:repo
      "https://github.com/deckarep/golang-set.git"
      #:commit
      "03b572015f8e9d51f230e69f81cc8441db837606")
))

(define latest (car versions))
