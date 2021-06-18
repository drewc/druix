(define-module (druix versions gerbil-libp2p) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil-libp2p"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      87
      #:ymd
      20210208
      #:hms
      80232
      #:sha256
      "0m85n9rjpvvszkkyrp16bxp8nkjss1nb976v2kr52xljknj122ms"
      #:repo
      "https://github.com/vyzo/gerbil-libp2p.git"
      #:commit
      "04d187dcb9247b5c632c28b62c647255ae14b40e")
))

(define latest (car versions))
