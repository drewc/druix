(define-module (druix versions gambit-c) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git> #:major 4 #:minor 9 #:patch 3 #:revision 1427 #:sha256 "0rw595mh2a4b9v03mb5rfmkhmkg0fg4clzzr7b95n4c8w3wj19mk" #:repo "https://github.com/gambit/gambit.git" #:commit "46618e760ae2c8577c704d0e0b07fe5695c9dcb9")))
