(define-module (druix versions gerbil-smug) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git>
      #:major
      0
      #:minor
      4
      #:patch
      20
      #:revision
      #f
      #:ymd
      20201212
      #:hms
      174641
      #:sha256
      "13fdijd71m3fzp9fw9xp6ddgr38q1ly6wnr53salp725w6i4wqid"
      #:repo
      "https://github.com/drewc/smug-gerbil.git"
      #:commit
      "cf23a47d0891aa9e697719309d04dd25dd1d840b")
))
