(define-module (druix versions gambit-c) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      4
      #:minor
      9
      #:patch
      4
      #:revision
      93
      #:ymd
      20220920
      #:hms
      162911
      #:sha256
      "0wc8dx3ny3zpdfbgfrb29gk2s4yg1wrkwyv2hblb5d3pa2l13zw9"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "176cff999b1e0bb8bf7f84ea2a636921529a0f72")

    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      63
      #:ymd
      20220801
      #:hms
      95721
      #:sha256
      "1c6ycdxhdsbdn3v3fy31ygzyllm77wrq9xffj9fxsi6ygy7aykqp"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "8e114a367c9ad81c4faa9f544a4a37f9fc9bd0e3")

    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      4
      #:minor
      9
      #:patch
      4
      #:revision
      93
      #:ymd
      20220920
      #:hms
      162911
      #:sha256
      "0wc8dx3ny3zpdfbgfrb29gk2s4yg1wrkwyv2hblb5d3pa2l13zw9"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "176cff999b1e0bb8bf7f84ea2a636921529a0f72")

    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      4
      #:minor
      9
      #:patch
      4
      #:revision
      90
      #:ymd
      20220910
      #:hms
      130555
      #:sha256
      "1idfw1ah2df1p3dan0gkgq54bfigay6lvq192m6p3fmv91j40kh1"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "758cd4ecdaee936f26bc6bd5ec39dcbd8c06bfc3")

    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      4
      #:minor
      9
      #:patch
      4
      #:revision
      4
      #:ymd
      20220109
      #:hms
      32410
      #:sha256
      "0nacpfflqi8a84hl9671jhpzx54as1h00ympq9m95v5pvdf2b504"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "7b2062fbf994bbf23f0198b0a438a8d4ec4e9c4a")

    (make <druix-version-git>
      #:name
      "gambit-c"
      #:major
      4
      #:minor
      9
      #:patch
      3
      #:revision
      1540
      #:ymd
      20211108
      #:hms
      175240
      #:sha256
      "1rwli8mjhb69v30x3l65qy1hi0w15bki3hbn701jfx17fdrqnf4m"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "ee1795ec16af8c0de005dc3b746803e874b01475")

    (make <druix-version-git>
      #:name
      "unnamed"
      #:major
      4
      #:minor
      9
      #:patch
      3
      #:revision
      1427
      #:ymd
      20210606
      #:hms
      132415
      #:sha256
      "0rw595mh2a4b9v03mb5rfmkhmkg0fg4clzzr7b95n4c8w3wj19mk"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "46618e760ae2c8577c704d0e0b07fe5695c9dcb9")

    (make <druix-version-git>
      #:name
      "unnamed"
      #:major
      4
      #:minor
      9
      #:patch
      3
      #:revision
      420
      #:ymd
      19700101
      #:hms
      0
      #:sha256
      "17f1zyvs0qazqbqczbsspqrz2vzsabg8kbz2xf1z5x6xxxvkqimc"
      #:repo
      "https://github.com/gambit/gambit.git"
      #:commit
      "1d5b01330881b3e26345dbaabfd35bbdfae36330")
))

(define latest (car versions))
