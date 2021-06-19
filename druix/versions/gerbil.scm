(define-module (druix versions gerbil) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
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
