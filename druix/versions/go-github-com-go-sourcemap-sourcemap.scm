(define-module (druix versions go-github-com-go-sourcemap-sourcemap) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-go-sourcemap-sourcemap"
      #:major
      2
      #:minor
      1
      #:patch
      3
      #:revision
      1
      #:ymd
      20201028
      #:hms
      152341
      #:sha256
      "0ifnfxp4016df6hzn2pbs7nm4wj7jwk3yw4hqybp23qhcfg7l9cr"
      #:repo
      "https://github.com/go-sourcemap/sourcemap.git"
      #:commit
      "eed1c205cd5e6a344123e253f59c02395ae80864")
))

(define latest (car versions))
