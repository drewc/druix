(define-module (druix packages go-ethereum)
  #:use-module (druix packages golang)
  #:use-module (druix utils)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix git-download))


(define latest-version (@ (druix versions go-ethereum) latest))
#;(define-public go-ethereum
  (package
    (name "go-ethereum") (version (v:druix-version latest-version))
    (synopsis "Official Golang implementation of the Ethereum protocol.")
    (home-page "https://geth.ethereum.org/") (description synopsis)
    (license l:gpl3) (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'perms
                    (lambda _ (invoke "chmod" "-R" "755" "./")))
                  (delete 'configure))
       #:make-flags '("geth")))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo latest-version))
             (commit (v:commit latest-version))))
       (sha256 (base32 (v:sha256 latest-version)))
       (file-name (git-file-name "go-ethereum" (v:commit latest-version)))))
    (inp/uts `(("go", go)))))
(define-public go-ethereum
  (package
    (name "go-ethereum") (version (v:druix-version latest-version))
    (synopsis "Official Golang implementation of the Ethereum protocol.")
    (home-page "https://geth.ethereum.org/") (description synopsis)
    (license l:gpl3) (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ethereum/go-ethereum/cmd/geth"
       #:unpack-path "github.com/ethereum/go-ethereum"
      ; #:phases
      #; (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             "Build the package named by IMPORT-PATH."
             (with-throw-handler
                 #t
               (lambda _
                 (invoke "go" "install"
                        "-v"   ; print the name of packages as they are compiled
                        "-x"   ; print each command as it is invoked
                        ;; Respectively, strip the symbol table and debug
                        ;; information, and the DWARF symbol table.
                        "-ldflags=-s -w"
                        "github.com/ethereum/go-ethereum/cmd/geth"))
               (lambda (key . args)
                 (display (string-append "Building '" import-path "' failed.\n"
                                         "Here are the results of `go env`:\n"))
                 (invoke "go" "env"))))))))
    (inputs `(("go-github-com-VictoriaMetrics-fastcache"
               , go-github-com-VictoriaMetrics-fastcache)
              ("go-github-com-davecgh-go-spew"
               ,go-github-com-davecgh-go-spew)
              ("go-github-com-deckarep-golang-set"
                ,go-github-com-deckarep-golang-set)
              ("go-github-com-dop251-goja" ,go-github-com-dop251-goja)
              ("go-github-com-dlclark-regexp2"
               ,go-github-com-dlclark-regexp2)
              ("go-github-com-edsrzf-mmap-go" ,go-github-com-edsrzf-mmap-go)
              ("go-github-com-fatih-color" ,go-github-com-fatih-color)
              ("go-github-com-fjl-memsize" ,go-github-com-fjl-memsize)
              ("go-github-com-gballet-go-libpcsclite"
               ,go-github-com-gballet-go-libpcsclite)
              ("go-github-com-go-sourcemap-sourcemap"
               , go-github-com-go-sourcemap-sourcemap)
              ("go-github-com-go-stack-stack"
               ,go-github-com-go-stack-stack)
              ("go-github-com-golang-protobuf-proto"
               ,go-github-com-golang-protobuf-proto)
              ("go-github-com-golang-protobuf-protoc-gen-go-descriptor"
               ,go-github-com-golang-protobuf-protoc-gen-go-descriptor)
              ("go-github-com-golang-snappy", go-github-com-golang-snappy)
              ("go-github-com-google-uuid" ,go-github-com-google-uuid)
              ("go-github-com-gorilla-websocket"
               ,go-github-com-gorilla-websocket)
              ("go-github-com-graph-gophers-graphql-go"
               ,go-github-com-graph-gophers-graphql-go)
              ("go-github-com-hashicorp-golang-lru"
               ,go-github-com-hashicorp-golang-lru)
              ("go-github-com-holiman-bloomfilter-v2"
               ,go-github-com-holiman-bloomfilter-v2)
              ))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (v:repo latest-version))
             (commit (v:commit latest-version))))
       (sha256 (base32 (v:sha256 latest-version)))
       (file-name (git-file-name "go-ethereum" (v:commit latest-version)))))))
