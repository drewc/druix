(define-module (druix versions gerbil-ftw) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil-ftw"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      24
      #:ymd
      20210630
      #:hms
      120015
      #:sha256
      "1yibpwk3556zkd55yp0iv4sh4yk22dglhgaxj4b4r9q8i0a2fhdm"
      #:repo
      "https://github.com/drewc/ftw.git"
      #:commit
      "22fb47f3832a254c459a8294a43c3a6fae7687e0")
))

(define latest (car versions))
