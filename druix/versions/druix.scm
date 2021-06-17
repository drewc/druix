(define-module (druix versions druix) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "druix"
      #:major
      0
      #:minor
      1
      #:patch
      #f
      #:revision
      #f
      #:ymd
      20210617
      #:hms
      204527
      #:sha256
      "078hlj23viqi51l422v9vskwfrjx8n6aqinvlczra0fw1fhsmaka"
      #:repo
      "https://github.com/drewc/druix.git"
      #:commit
      "0043ff3c04bd21e229b3c1e190b1922a3db17708")

    (make <druix-version-git>
      #:name
      "druix"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      22
      #:ymd
      20210616
      #:hms
      32910
      #:sha256
      "01f6yq173id1bf0pr08pz5sgwhr3l3xhalsz2m56pm6gl99ig1wx"
      #:repo
      "https://github.com/drewc/druix.git"
      #:commit
      "21e58ac74b90dc51d7c9e98b69edc835215228f0")
))

(define latest (car versions))
