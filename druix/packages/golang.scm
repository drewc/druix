(define-module (druix packages golang)
  #:use-module (druix utils)
  #:use-module (druix utils go-packages)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix git-download)
  #:re-export
  (go
   go-github-com-davecgh-go-spew
   go-github-com-dlclark-regexp2
   go-golang-org-x-text
   go-gopkg-in-yaml-v2
   go-golang-org-x-sys
   go-github-com-fatih-color
   go-github-com-golang-protobuf-proto
   go-github-com-golang-snappy
   go-github-com-google-uuid
   )
  ;; public

;; go-github-com-VictoriaMetrics-fastcache
;; go-github-com-deckarep-go
;; go-github-com-dop251-goja
)

;; go-github-com-VictoriaMetrics-fastcache
 (define-public-go-package
  "github.com/VictoriaMetrics/fastcache"
  "Fast thread-safe inmemory cache for big number of entries in Go.")

;; go-github-com-deckarep-golang-set
 (define-public-go-package
  "github.com/deckarep/golang-set"
  "A simple set type for the Go language."
  #:description "A simple set type for the Go language. Trusted by Docker, 1Password, Ethereum and Hashicorp.")

;;
(define-public-go-package
  "github.com/go-sourcemap/sourcemap"
  "Source maps consumer for Golang"
  #:phases '(modify-phases %standard-phases
              (delete 'check)))

;; go-github-com-dop251-goja
(define-public-go-package
  "github.com/dop251/goja"
  "ECMAScript/JavaScript engine in pure Go"
   #:inputs `(("perl", (@ (gnu packages perl) perl))
              ("go-github-com-dlclark-regexp2"
               ,go-github-com-dlclark-regexp2)
              ("go-github-com-go-sourcemap-sourcemap"
               , go-github-com-go-sourcemap-sourcemap)
              ("go-golang-org-x-text" ,go-golang-org-x-text)
              ("gopkg.in/yaml.v2" ,go-gopkg-in-yaml-v2))
    #:phases '(modify-phases %standard-phases (delete 'check)))

;; go-github-com-edsrzf-mmap-go
(define-public-go-package
  "github.com/edsrzf/mmap-go" "A portable mmap package for Go"
  #:inputs `(("golang.org/x/sys" ,go-golang-org-x-sys)))

;; go-github-com-fjl-memsize
(define-public-go-package "github.com/fjl/memsize" "memsize computes the size of your object graph")


;; go-github-com-gballet-go-libpcsclite
(define-public-go-package "github.com/gballet/go-libpcsclite" "A golang implementation of the libpcpsclite client")


;; go-github-com-go-stack-stack
(define-public-go-package "github.com/go-stack/stack" "Package stack implements utilities to capture, manipulate, and format call stacks.")

;; ;; go-github-com-golang-protobuf
;; (define-public-go-package "github.com/golang/protobuf"
;;   "Go support for Protocol Buffers")

(define-public go-github-com-golang-protobuf-protoc-gen-go-descriptor
  (package
    (name "go-github-com-golang-protobuf-protoc-gen-go-descriptor")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/golang/protobuf")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15am4s4646qy6iv0g3kkqq52rzykqjhm4bf08dk0fy2r58knpsyl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/golang/protobuf/protoc-gen-go/descriptor"
       #:unpack-path "github.com/golang/protobuf"
       ;; Requires unpackaged golang.org/x/sync/errgroup
       #:tests? #f))
    (synopsis "Go support for Protocol Buffers")
    (description "This package provides Go support for the Protocol Buffers
data serialization format.")
    (home-page "https://github.com/golang/protobuf")
    (license l:bsd-3)))


;; go-github-com-gorilla-websocket
(define-public-go-package "github.com/gorilla/websocket"
  "A fast, well-tested and widely used WebSocket implementation for Go.")

;; go-github-com-opentracing-opentracing-go
(define-public-go-package "github.com/opentracing/opentracing-go" "OpenTracing API for Go"
  #:phases '(modify-phases %standard-phases (delete 'check)))

;; go-github-com-graph-gophers-graphql-go
(define-public-go-package "github.com/graph-gophers/graphql-go"
  "GraphQL server with a focus on ease of use"
  #:inputs `(("go-github-com-opentracing-opentracing-go"
             ,go-github-com-opentracing-opentracing-go))
  #:phases '(modify-phases %standard-phases (delete 'check)))



;; go-github-com-hashicorp-golang-lru
(define-public-go-package "github.com/hashicorp/golang-lru" "Golang LRU cache")


;; go-github-com-holiman-bloomfilter-v2
(define-public-go-package "github.com/holiman/bloomfilter/v2" "syn"
  #:unpack-path "github.com/holiman/bloomfilter")
