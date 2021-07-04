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
   go-golang.org-x-sync-errgroup
   go-github-com-burntsushi-toml))


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
(define-public-go-package "github.com/holiman/bloomfilter/v2"
  "Bloom filter in go"
  #:description
  "Face-meltingly fast, thread-safe, marshalable, unionable, probability- and optimal-size-calculating Bloom filter in go"
  #:unpack-path "github.com/holiman/bloomfilter")



;; go-github-com-holiman-uint256
(define-public-go-package "github.com/holiman/uint256" "Fixed size 256-bit math library for Go")


;; go-github-com-huin-goupnp
(define-public-go-package "github.com/huin/goupnp" "UPnP client library for Go"
  #:inputs `(("go-golang.org-x-sync-errgroup", go-golang.org-x-sync-errgroup)))


;; ;; go-github-com-influxdata-influxdb-v2-kit-platform
;; (define-public-go-package "github.com/influxdata/influxdb/v2/kit/platform" "Scalable datastore for metrics, events, and real-time analytics")



;; ;; go-github-com-influxdata-influxdb-client
;; (define-public-go-package "github.com/influxdata/influxdb/client"
;;   "Scalable datastore for metrics, events, and real-time analytics"
;;   #:inputs `(("go-github-com-influxdata-influxdb-v2-kit-platform"
;;              ,go-github-com-influxdata-influxdb-v2-kit-platform)))

(define-public go-google-golang-org-protobuf
  (package
    (name "go-google-golang-org-protobuf")
    (version "1.27.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/protobuf")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0aszb7cv8fq1m8akgd4kjyg5q7g5z9fdqnry6057ygq9r8r2yif2"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "google.golang.org/protobuf"))
    (propagated-inputs
      `(("go-github-com-google-go-cmp"
         ,go-github-com-google-go-cmp)
        ("go-github-com-golang-protobuf"
         ,go-github-com-golang-protobuf)))
    (home-page "https://google.golang.org/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
      "This project hosts the Go implementation for
@url{https://developers.google.com/protocol-buffers,protocol buffers}, which is a
language-neutral, platform-neutral, extensible mechanism for serializing
structured data.  The protocol buffer language is a language for specifying the
schema for structured data.  This schema is compiled into language specific
bindings.  This project provides both a tool to generate Go code for the
protocol buffer language, and also the runtime implementation to handle
serialization of messages in Go.  See the
@url{https://developers.google.com/protocol-buffers/docs/overview,protocol buffer developer guide}
for more information about protocol buffers themselves.")
    (license l:bsd-3)))

(define-public go-google-golang-org-grpc
  (package
    (name "go-google-golang-org-grpc")
    (version "1.39.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/grpc/grpc-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "02wbl21kjx118ranbhqbpzxnq31mayi2w60vdrm2zn3rqqf3jszy"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "google.golang.org/grpc"))
    (propagated-inputs
      `(("go-google-golang-org-protobuf"
         ,go-google-golang-org-protobuf)
        ("go-google-golang-org-genproto"
         ,go-google-golang-org-genproto)
        ("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ("go-golang-org-x-oauth2"
         ,go-golang-org-x-oauth2)
        ("go-golang-org-x-net" ,go-golang-org-x-net)
        ("go-github-com-google-uuid"
         ,go-github-com-google-uuid)
        ("go-github-com-google-go-cmp"
         ,go-github-com-google-go-cmp)
        ("go-github-com-golang-protobuf"
         ,go-github-com-golang-protobuf)
        ("go-github-com-golang-glog"
         ,go-github-com-golang-glog)
        ("go-github-com-envoyproxy-go-control-plane"
         ,go-github-com-envoyproxy-go-control-plane)
        ("go-github-com-cncf-udpa-go"
         ,go-github-com-cncf-udpa-go)))
    (home-page "https://google.golang.org/grpc")
    (synopsis "gRPC-Go")
(description
      "Package grpc implements an RPC system called gRPC.")
    (license l:asl2.0)))
(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20210701191553-46259e63a0a9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/googleapis/go-genproto")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0z60xi1qc0cqzia5rpilb0v2qnkzbx50sfp6cjc9zmdip9jdysya"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "google.golang.org/genproto"))
    (propagated-inputs
      `(("go-google-golang-org-protobuf"
         ,go-google-golang-org-protobuf)
        ("go-google-golang-org-grpc"
         ,go-google-golang-org-grpc)
        ("go-golang-org-x-tools" ,go-golang-org-x-tools)
        ("go-golang-org-x-text" ,go-golang-org-x-text)
        ("go-golang-org-x-lint" ,go-golang-org-x-lint)
        ("go-github-com-golang-protobuf"
         ,go-github-com-golang-protobuf)))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
      "This repository contains the generated Go packages for common protocol buffer
types, and the generated @url{http://grpc.io,gRPC} code necessary for interacting with Google's gRPC
APIs.")
    (license l:asl2.0)))

(define-public go-github-com-pierrec-lz4-v4
  (package
    (name "go-github-com-pierrec-lz4-v4")
    (version "4.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pierrec/lz4")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1ffsm2ny2v6nasc3zww5f2bl0jqi2jmiyqm8x2cvyzrd0clay1wl"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/pierrec/lz4"))
    (home-page "https://github.com/pierrec/lz4")
    (synopsis "lz4 : LZ4 compression in pure Go")
    (description
      "Package lz4 implements reading and writing lz4 compressed data.
")
    (license l:bsd-3)))
(define-public go-github-com-klauspost-compress
  (package
    (name "go-github-com-klauspost-compress")
    (version "1.13.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/klauspost/compress")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0ydnf9rizlhm8rilh14674qqx272sbwbkjx06xn9pqvy6mmn2r3r"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/klauspost/compress"))
    (propagated-inputs
      `(("go-github-com-golang-snappy"
         ,go-github-com-golang-snappy)))
    (home-page
      "https://github.com/klauspost/compress")
    (synopsis "compress")
    (description
      "This package provides various compression algorithms.")
    (license l:bsd-3)))

(define-public go-github-com-google-go-cmp
  (package
    (name "go-github-com-google-go-cmp")
    (version "0.5.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/go-cmp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0lrb0pacv5iy3m6fn1qb3nv7zwimfhpzqq8f6hwpwx88cx3g6p1s"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/google/go-cmp"))
    (propagated-inputs
      `(("go-golang-org-x-xerrors"
         ,go-golang-org-x-xerrors)))
    (home-page "https://github.com/google/go-cmp")
    (synopsis "Package for equality of Go values")
    (description
      "This package is intended to be a more powerful and safer alternative to
@code{reflect.DeepEqual} for comparing whether two values are semantically equal.")
    (license l:bsd-3)))

(define-public go-github-com-google-flatbuffers
  (package
    (name "go-github-com-google-flatbuffers")
    (version "2.0.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/flatbuffers")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1zbf6bdpps8369r1ql00irxrp58jnalycc8jcapb8iqg654vlfz8"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/google/flatbuffers"))
    (home-page
      "https://github.com/google/flatbuffers")
    (synopsis "FlatBuffers")
    (description
      "@strong{FlatBuffers} is a cross platform serialization library architected for
maximum memory efficiency.  It allows you to directly access serialized data without parsing/unpacking it first, while still having great forwards/backwards compatibility.")
    (license l:asl2.0)))

(define-public go-github-com-golang-protobuf
  (package
    (name "go-github-com-golang-protobuf")
    (version "1.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/protobuf")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1mh5fyim42dn821nsd3afnmgscrzzhn3h8rag635d2jnr23r1zhk"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/golang/protobuf"))
    (propagated-inputs
      `(("go-google-golang-org-protobuf"
         ,go-google-golang-org-protobuf)
        ("go-github-com-google-go-cmp"
         ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/golang/protobuf")
    (synopsis "Go support for Protocol Buffers")
    (description
      "This module
(@url{https://pkg.go.dev/mod/github.com/golang/protobuf,(code github.com/golang/protobuf)})
contains Go bindings for protocol buffers.")
    (license l:bsd-3)))

(define-public go-github-com-golang-glog
  (package
    (name "go-github-com-golang-glog")
    (version "0.0.0-20210429001901-424d2337a529")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/glog")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1hnbln6h61ik9z92dspybmb0cxpddm1c4dzx5g64jr8cdvqcgpji"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/golang/glog"))
    (home-page "https://github.com/golang/glog")
    (synopsis "glog")
    (description
      "Package glog implements logging analogous to the Google-internal C++ INFO/ERROR/V setup.
It provides functions Info, Warning, Error, Fatal, plus formatting variants such as
Infof.  It also provides V-style logging controlled by the -v and -vmodule=file=2 flags.
")
    (license l:asl2.0)))

(define-public go-github-com-envoyproxy-go-control-plane
  (package
    (name "go-github-com-envoyproxy-go-control-plane")
    (version "0.9.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/envoyproxy/go-control-plane")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1cb3s6x2jx93cdljb9jwp2wc2iljv91h7706gq1amkkwdhdxhcdv"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "github.com/envoyproxy/go-control-plane"))
    (propagated-inputs
      `(("go-google-golang-org-protobuf"
         ,go-google-golang-org-protobuf)
        ("go-google-golang-org-grpc"
         ,go-google-golang-org-grpc)
        ("go-google-golang-org-genproto"
         ,go-google-golang-org-genproto)
        ("go-go-opentelemetry-io-proto-otlp"
         ,go-go-opentelemetry-io-proto-otlp)
        ("go-github-com-stretchr-testify"
         ,go-github-com-stretchr-testify)
        ("go-github-com-prometheus-client-model"
         ,go-github-com-prometheus-client-model)
        ("go-github-com-google-go-cmp"
         ,go-github-com-google-go-cmp)
        ("go-github-com-golang-protobuf"
         ,go-github-com-golang-protobuf)
        ("go-github-com-envoyproxy-protoc-gen-validate"
         ,go-github-com-envoyproxy-protoc-gen-validate)
        ("go-github-com-cncf-xds-go"
         ,go-github-com-cncf-xds-go)
        ("go-github-com-census-instrumentation-opencensus-proto"
         ,go-github-com-census-instrumentation-opencensus-proto)))
    (home-page
      "https://github.com/envoyproxy/go-control-plane")
    (synopsis "control-plane")
    (description
      "This repository contains a Go-based implementation of an API server that
implements the discovery service APIs defined in
@url{https://github.com/envoyproxy/data-plane-api,data-plane-api}.")
    (license l:asl2.0)))

(define-public go-github-com-apache-arrow-go-arrow
  (package
    (name "go-github-com-apache-arrow-go-arrow")
    (version "0.0.0-20210702113516-d7a8b468ab64")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/apache/arrow")
                          (commit (go-version->git-ref version))))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      "1s6dyykpirszf879msr7zqc319cd3ylz2hf4yh83p672l4d1m49c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/apache/arrow"))
    (propagated-inputs
     `(("go-google-golang-org-protobuf"
        ,go-google-golang-org-protobuf)
       ("go-google-golang-org-grpc"
        ,go-google-golang-org-grpc)
       ("go-google-golang-org-genproto"
        ,go-google-golang-org-genproto)
       ("go-golang-org-x-xerrors"
        ,go-golang-org-x-xerrors)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-sync" ,go-golang-org-x-sync)
       ("go-github-com-stretchr-testify"
        ,go-github-com-stretchr-testify)
       ("go-github-com-pmezard-go-difflib"
        ,go-github-com-pmezard-go-difflib)
       ("go-github-com-pierrec-lz4-v4"
        ,go-github-com-pierrec-lz4-v4)
       ("go-github-com-klauspost-compress"
        ,go-github-com-klauspost-compress)
       ("go-github-com-google-go-cmp"
        ,go-github-com-google-go-cmp)
       ("go-github-com-google-flatbuffers"
        ,go-github-com-google-flatbuffers)
       ("go-github-com-golang-protobuf"
        ,go-github-com-golang-protobuf)
       ("go-github-com-davecgh-go-spew"
        ,go-github-com-davecgh-go-spew)))
    (home-page "https://github.com/apache/arrow")
    (synopsis
     "Package arrow provides an implementation of Apache Arrow.\n")
    (description
     "Package arrow provides an implementation of Apache Arrow.\n")
    (license l:asl2.0)))

#;(define-public-go-package "github.com/apache/arrow"
  "Apache Arrow go"
  #:inputs `(("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
             ("go-golang.org-x-sync-errgroup", go-golang.org-x-sync-errgroup))
  #:phases '(modify-phases %standard-phases
              (replace 'build
                (lambda* _
                  (with-throw-handler
    #t
    (lambda _
      (invoke "pwd")
      (invoke "go" "install"
              "-v" ; print the name of packages as they are compiled
              "-x" ; print each command as it is invoked
              ;; Respectively, strip the symbol table and debug
              ;; information, and the DWARF symbol table.
              "-ldflags=-s -w" "github.com/apache/arrow/go/arrow"))
    (lambda (key . args)
      (display (string-append "Building '" "' failed.\n"
                              "Here are the results of `go env`:\n")) (invoke "go" "env")))))))

;; go-github-com-influxdata-flux
(define-public-go-package "github.com/influxdata/flux"
  "Flux is a lightweight scripting language for querying databases"
  #:inputs `(("go-github-com-apache-arrow-go-arrow"
              ,go-github-com-apache-arrow-go-arrow)))



;; go-github-com-influxdata-influxdb
(define-public-go-package "github.com/influxdata/influxdb" "Scalable datastore for metrics, events, and real-time analytics"
  #:inputs `(("go-github-com-burntsushi-toml"
              ,go-github-com-burntsushi-toml)
             ("go-github-com-influxdata-flux"
              ,go-github-com-influxdata-flux)))
