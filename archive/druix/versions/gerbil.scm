(define-module (druix versions gerbil) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      0
      #:patch
      #f
      #:revision
      66
      #:ymd
      20220926
      #:hms
      181637
      #:sha256
      "1712ncdrlnrl77pcln2s0cd97ad7ns1x3p3yygdgh4d99061p5hp"
      #:repo
      "ssh://git@github.com/vyzo/gerbil.git"
      #:commit
      "605e6fd6003ae8a188ecdbc0406ff682c77f9252")

    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      17
      #:patch
      #f
      #:revision
      275
      #:ymd
      20220110
      #:hms
      205617
      #:sha256
      "0c0nspm659ybgmqlppdv7sxzll4hwkvcp9qmcsip6d0kz0p8r9c3"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "7014e282dad51978c67f2746ac3e8553c73e09a3")

    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      16
      #:patch
      #f
      #:revision
      243
      #:ymd
      20211029
      #:hms
      83016
      #:sha256
      "1jjm271bfpqski0l1z7sjqkdba135j595f6fnkvjym9yxwznxndy"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "87c6279bfaf0b9e8d5d418415687f703b5be75dd")

    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      16
      #:patch
      #f
      #:revision
      192
      #:ymd
      20210608
      #:hms
      122759
      #:sha256
      "0jm331giq0m73l66xc9mjzzbvmg4jxqkmcxc68fjywrcxa07xx31"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "fa9537be0848e54d2c68165503b9cc48babb9334")

    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      16
      #:patch
      #f
      #:revision
      188
      #:ymd
      19700101
      #:hms
      0
      #:sha256
      "1br42b7slyas4cjs0hhh85s9s0inag3d85jva8ym4di0k756v327"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "237627a4bf573e372ed3cd539a35c0f8477b8879")

    (make <druix-version-git>
      #:name
      "gerbil"
      #:major
      0
      #:minor
      16
      #:patch
      #f
      #:revision
      187
      #:ymd
      19700101
      #:hms
      0
      #:sha256
      "0yqsjyk1gzfnvp4rvs8q06v7vcdgbnpw9bpa03f36zkzp466gdyl"
      #:repo
      "https://github.com/vyzo/gerbil.git"
      #:commit
      "7e8b4baaf563b7cd804b3b653d4823b9762f5c87")
))

(define latest (car versions))
