(define-module (druix versions gerbil-unstable) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git> #:major 0 #:minor 16 #:patch #f #:revision 188 #:sha256 "1br42b7slyas4cjs0hhh85s9s0inag3d85jva8ym4di0k756v327" #:repo "https://github.com/vyzo/gerbil.git" #:commit "237627a4bf573e372ed3cd539a35c0f8477b8879")
    (make <druix-version-git> #:major 0 #:minor 16 #:patch #f #:revision 187 #:sha256 "0yqsjyk1gzfnvp4rvs8q06v7vcdgbnpw9bpa03f36zkzp466gdyl" #:repo "https://github.com/vyzo/gerbil.git" #:commit "7e8b4baaf563b7cd804b3b653d4823b9762f5c87")))
