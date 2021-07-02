(define-module (druix versions glow-lang) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "glow-lang"
      #:major
      0
      #:minor
      1
      #:patch
      0
      #:revision
      182
      #:ymd
      20210701
      #:hms
      214947
      #:sha256
      "0mfjr3rb3q029048xh4c6swin0aaj7gy966rrr40lpgndbl44jsw"
      #:repo
      "https://gitlab.com/mukn/glow.git"
      #:commit
      "ed0bd72d5aa15994d3c9608e209ac2c81815e0e2")

    (make <druix-version-git>
      #:name
      "glow-lang"
      #:major
      0
      #:minor
      1
      #:patch
      0
      #:revision
      159
      #:ymd
      20210618
      #:hms
      90230
      #:sha256
      "0hgvjywb1icmbkxppb85irxdncy0zgw9l385z3vxh91ckj2bg2fh"
      #:repo
      "https://gitlab.com/mukn/glow.git"
      #:commit
      "eeeccdc55df81b21af427da6c9999a68286c468a")
))

(define latest (car versions))
