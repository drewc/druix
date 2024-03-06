(define-module (druix packages go-ethereum)
  #:use-module (druix utils)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages golang)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix git-download))

(define version "1.10.6")

(define-public go-ethereum-gopath
  (package
    (name "go-ethereum-gopath")
    (version version)
    (source (origin
              (method url-fetch/tarbomb)
              (uri "https://github.com/drewc/druix/releases/download/v0.0.0-geth-libs/go-ethereum-libs-1.10.6.tar.gz")
              (sha256
               (base32
                "0ks6bshmlmmqw9dpcfhp7d136dvaqnp42yqh84xmgk1s5ynrpryk"))))
    (build-system copy-build-system)
     (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'reset-gzip-timestamps))))
    (synopsis " Build Libs for Official Golang implementation of the Ethereum protocol.")
    (home-page "https://geth.ethereum.org/")
    (description synopsis)
    (license l:gpl3)))

(define-public go-ethereum
  (package
    (name "go-ethereum") (version version)
    (synopsis "Official Golang implementation of the Ethereum protocol.")
    (home-page "https://geth.ethereum.org/") (description synopsis)
    (license l:gpl3) (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gopath (assoc-ref inputs "gopath")))
               (invoke "echo" gopath)
               (with-directory-excursion "go-ethereum-1.10.6"
                 (setenv "GOPATH" gopath)
                 (setenv "GOCACHE" "/tmp/go-cache")
                 (invoke "make" "geth")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (invoke "echo" bin)
               (with-directory-excursion "go-ethereum-1.10.6"
                 (install-file "./build/bin/geth" bin)))
             #t)))))
    (inputs `(("gopath" ,go-ethereum-gopath)
              ("go" ,go)))
    (source (origin
              (method url-fetch/tarbomb)
              (uri "https://github.com/ethereum/go-ethereum/archive/refs/tags/v1.10.6.tar.gz")
              (sha256
               (base32
                "0cpnln5ycv7sybw0b826mdxla15az997pb3vldqmzzrlbjdg091p"))))))
