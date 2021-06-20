(define-module (druix versions go-github-com-gorilla-websocket) #:use-module (druix versions) #:use-module (oop goops) #:export (versions latest))
(define versions 
  (list 
    (make <druix-version-git>
      #:name
      "go-github-com-gorilla-websocket"
      #:major
      1
      #:minor
      4
      #:patch
      2
      #:revision
      4
      #:ymd
      20210424
      #:hms
      162022
      #:sha256
      "0z2cwsyyfbqaxvnik45c8gqkjn8n54w8c5h8zzl5jhq9chbfycr1"
      #:repo
      "https://github.com/gorilla/websocket.git"
      #:commit
      "e8629af678b7fe13f35dff5e197de93b4148a909")
))

(define latest (car versions))
