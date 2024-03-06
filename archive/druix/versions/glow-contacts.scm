(define-module (druix versions glow-contacts) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "glow-contacts"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      41
      #:ymd
      20210630
      #:hms
      213825
      #:sha256
      "1xirm163bbhisrgxd9zf9s99sl407wqjvl246i6a6lv8p9s88pmn"
      #:repo
      "https://gitlab.com/mukn/glow-contacts.git"
      #:commit
      "5c69c97f76d925ce7a408dc1f50fd06fbc7f5638")

    (make <druix-version-git>
      #:name
      "glow-contacts"
      #:major
      0
      #:minor
      0
      #:patch
      0
      #:revision
      2
      #:ymd
      20210630
      #:hms
      120232
      #:sha256
      "00w5jfm8gl46rcfm3bw2brhfx9awp882lkxbdhzc27b8w5d09i2p"
      #:repo
      "https://gitlab.com/drewc/glow-contacts.git"
      #:commit
      "d30a98667ddb79db3cbd9ba3f07754ac6b718453")
))

(define latest (car versions))
