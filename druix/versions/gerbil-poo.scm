(define-module (druix versions gerbil-poo) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git>
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      97
      #:ymd
      20210521
      #:hms
      182954
      #:sha256
      "04fgyjbn4mjqa8zhsck55bxbzbl1wpm1yzakabrsvgk2cx8rgibb"
      #:repo
      "https://github.com/fare/gerbil-poo.git"
      #:commit
      "8ab28efe1dd828a0a83195d81164588a0fb404a0")
))
