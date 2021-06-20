(define-module (druix versions go-github-com-gballet-go-libpcsclite) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-gballet-go-libpcsclite"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      24
      #:ymd
      20191108
      #:hms
      122812
      #:sha256
      "0jn77a8hcb4kfz81xmpl0lxz0sg1gnika0kxdj79rmdkgcrcmw3m"
      #:repo
      "https://github.com/gballet/go-libpcsclite.git"
      #:commit
      "4678299bea08415f0ca8bd71da9610625cc86e86")
))

(define latest (car versions))
