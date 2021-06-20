(define-module (druix versions go-github-com-graph-gophers-graphql-go) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-graph-gophers-graphql-go"
      #:major
      1
      #:minor
      1
      #:patch
      0
      #:revision
      6
      #:ymd
      20210610
      #:hms
      195333
      #:sha256
      "047ff3mcnafaacxyi6xz3xg011ilhnwj70p2hizszvhalm6hwk8m"
      #:repo
      "https://github.com/graph-gophers/graphql-go.git"
      #:commit
      "af5bb93e114f0cd4cc095dd8eae0b67070ae8f20")
))

(define latest (car versions))
