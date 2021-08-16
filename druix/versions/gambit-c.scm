(define-module (druix versions gambit-c) #:use-module (druix versions)
  #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
   (make <druix-version-git> #:major 4 #:minor 9 #:patch 3 #:revision 1427
         #:ymd 20210606 #:hms 132415
         #:sha256 "0rw595mh2a4b9v03mb5rfmkhmkg0fg4clzzr7b95n4c8w3wj19mk"
         #:repo "https://github.com/gambit/gambit.git"
         #:commit "46618e760ae2c8577c704d0e0b07fe5695c9dcb9")
   (make <druix-version-git>
     #:major 4 #:minor 9 #:patch 3 #:revision 420
     #:sha256 "17f1zyvs0qazqbqczbsspqrz2vzsabg8kbz2xf1z5x6xxxvkqimc"
     #:repo "https://github.com/gambit/gambit.git"
     #:commit "1d5b01330881b3e26345dbaabfd35bbdfae36330")))

(define latest (car versions))
