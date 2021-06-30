(define-module (druix packages sql sqlite)
  #:use-module (gnu packages)
  #:use-module ((gnu packages sqlite) #:prefix old:)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages readline)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define-public sqlite
  (package
    (inherit old:sqlite)
    (version "3.36.0")
    (source (origin
              (method url-fetch)
              (uri (let ((numeric-version
                          (match (string-split version #\.)
                            ((first-digit other-digits ...)
                             (string-append first-digit
                                            (string-pad-right
                                             (string-concatenate
                                              (map (cut string-pad <> 2 #\0)
                                                   other-digits))
                                             6 #\0))))))
                     (string-append "https://sqlite.org/2021/sqlite-autoconf-"
                                    numeric-version ".tar.gz")))
              (sha256
               (base32 "1qxwkfvd185dfcqbakrzikrsw6ffr5jp1gl3dch9dsdyjvmw745x"))))))
