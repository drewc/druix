(define-module (druix versions go-github-com-VictoriaMetrics-fastcache) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-VictoriaMetrics-fastcache"
      #:major
      1
      #:minor
      6
      #:patch
      0
      #:revision
      #f
      #:ymd
      20210531
      #:hms
      61147
      #:sha256
      "1ivx700ijnkyjbgw4rq4446d4dnb4sg7sh569rsvnm5cm0fkvyhz"
      #:repo
      "https://github.com/VictoriaMetrics/fastcache.git"
      #:commit
      "5ffb40c1da4c7e2b8483e73629e66f4f1f94bff2")
))

(define latest (car versions))
