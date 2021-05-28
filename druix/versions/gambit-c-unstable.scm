(define-module (druix versions gambit-c-unstable)
  #:use-module (druix versions)
  #:use-module (oop goops)
  #:export (versions))

(define versions
  (list
   (make <druix-version-git>
    #:major 4 #:minor 3 #:patch 9 #:revision 0
    #:sha256 "17f1zyvs0qazqbqczbsspqrz2vzsabg8kbz2xf1z5x6xxxvkqimc"
    #:repo "https://github.com/gambit/gambit.git"
    #:commit "1d5b01330881b3e26345dbaabfd35bbdfae36330")))
