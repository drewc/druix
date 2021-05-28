(define-module (druix versions gerbil-unstable) #:use-module (druix versions) #:use-module (oop goops) #:export (versions))
(define versions 
  (list 
    (make <druix-version-git> #:major 0 #:minor 16 #:patch #f #:revision 187 #:sha256 "0yqsjyk1gzfnvp4rvs8q06v7vcdgbnpw9bpa03f36zkzp466gdyl" #:repo "https://github.com/vyzo/gerbil.git" #:commit "7e8b4baaf563b7cd804b3b653d4823b9762f5c87")))
