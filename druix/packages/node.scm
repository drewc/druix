(define-module (druix packages node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system node)
  #:use-module (gnu packages)
  #:use-module ((gnu packages node) #:prefix node:)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define-public node
  (package
   (inherit node:node-lts)
   (version "18.12.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://nodejs.org/dist/v" version
                                "/node-v" version ".tar.xz"))
            (sha256
             (base32 "0bpl0klb60wnhh53xdzgxa0dajjlrnk6dmnfcdvikswr54gg19vk"))
            (modules '((guix build utils)))
            (snippet
             `(begin
                ;; Remove bundled software, where possible
                (for-each delete-file-recursively
                          '("deps/cares"
                            "deps/icu-small"
                            "deps/nghttp2"
                          ;;  "deps/openssl"
                            "deps/zlib"))
                (substitute* "Makefile"
                             ;; Remove references to bundled software.
                             (("deps/uv/uv.gyp") "")
                             (("deps/zlib/zlib.gyp") ""))
                #t))))

   (arguments
    (substitute-keyword-arguments
     (package-arguments node:node-lts)
     ((#:phases phases)
      `(modify-phases ,phases
                      (delete 'replace-llhttp-sources)
                      (replace 'delete-problematic-tests
                               (lambda* (#:key inputs #:allow-other-keys)
                                 ;; FIXME: These tests fail in the build container, but they don't
                                 ;; seem to be indicative of real problems in practice.
                                 (for-each delete-file
                                           '("test/parallel/test-release-npm.js"
                                             "test/parallel/test-stdin-from-file-spawn.js"
                                             "test/parallel/test-cluster-primary-error.js"
                                             "test/parallel/test-cluster-primary-kill.js"
                                             ))

                                 ;; These require a DNS resolver.
                                 (for-each delete-file
                                           '("test/parallel/test-dns.js"
                                             "test/parallel/test-net-socket-connect-without-cb.js"
                                             "test/parallel/test-tcp-wrap-listen.js"
                                             "test/parallel/test-dns-lookupService-promises.js"))
                                 #t))))))))

(define-public node-16
  (package
    (inherit node:node-lts)
    (version "16.17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nodejs.org/dist/v" version
                                  "/node-v" version ".tar.xz"))
              (sha256
               (base32 "0iang6lqx006ki45wxd9zpscbh59bib67j8vja2nji2yd22w4a0x"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; Remove bundled software, where possible
                  (for-each delete-file-recursively
                            '("deps/cares"
                              "deps/icu-small"
                              "deps/nghttp2"
                              "deps/openssl"
                              "deps/zlib"))
                  (substitute* "Makefile"
                    ;; Remove references to bundled software.
                    (("deps/uv/uv.gyp") "")
                    (("deps/zlib/zlib.gyp") ""))
                  #t))))

    (arguments
     (substitute-keyword-arguments (package-arguments node:node-lts)
       ((#:phases phases)
    `(modify-phases ,phases
       (delete 'replace-llhttp-sources)
       (replace 'delete-problematic-tests
         (lambda* (#:key inputs #:allow-other-keys)
           ;; FIXME: These tests fail in the build container, but they don't
           ;; seem to be indicative of real problems in practice.
            (for-each delete-file
                      '("test/parallel/test-release-npm.js"
                        "test/parallel/test-stdin-from-file-spawn.js"
                        "test/parallel/test-cluster-primary-error.js"
                        "test/parallel/test-cluster-primary-kill.js"
                        ))

            ;; These require a DNS resolver.
            (for-each delete-file
                      '("test/parallel/test-dns.js"
                        "test/parallel/test-dns-lookupService-promises.js"))

           ;; ;; These tests require networking.
           ;; (for-each delete-file
           ;;           '("test/parallel/test-https-agent-unref-socket.js"
           ;;             "test/parallel/test-corepack-yarn-install.js"))

           ;; ;; This test is timing-sensitive, and fails sporadically on
           ;; ;; slow, busy, or even very fast machines.
           ;; (delete-file "test/parallel/test-fs-utimes.js")

           ;; ;; FIXME: This test fails randomly:
           ;; ;; https://github.com/nodejs/node/issues/31213
           ;; (delete-file "test/parallel/test-net-listen-after-destroying-stdin.js")

           ;; ;; FIXME: These tests fail on armhf-linux:
           ;; ;; https://github.com/nodejs/node/issues/31970
           ;; ,@(if (target-arm32?)
           ;;       '((for-each delete-file
           ;;                   '("test/parallel/test-zlib.js"
           ;;                     "test/parallel/test-zlib-brotli.js"
           ;;                     "test/parallel/test-zlib-brotli-flush.js"
           ;;                     "test/parallel/test-zlib-brotli-from-brotli.js"
           ;;                     "test/parallel/test-zlib-brotli-from-string.js"
           ;;                     "test/parallel/test-zlib-convenience-methods.js"
           ;;                     "test/parallel/test-zlib-random-byte-pipes.js"
           ;;                     "test/parallel/test-zlib-write-after-flush.js")))
           ;;       '())

           ;; ;; These tests have an expiry date: they depend on the validity of
           ;; ;; TLS certificates that are bundled with the source.  We want this
           ;; ;; package to be reproducible forever, so remove those.
           ;; ;; TODO: Regenerate certs instead.
           ;; (for-each delete-file
           ;;           '("test/parallel/test-tls-passphrase.js"
           ;;             "test/parallel/test-tls-server-verify.js")))
           #t
           ))

       ;; (add-after 'delete-problematic-tests 'replace-llhttp-sources
       ;;   (lambda* (#:key inputs #:allow-other-keys)
       ;;     ;; Replace pre-generated llhttp sources
       ;;     (let ((llhttp (assoc-ref inputs "llhttp")))
       ;;       (copy-file (string-append llhttp "/src/llhttp.c")
       ;;                  "deps/llhttp/src/llhttp.c")
       ;;       (copy-file (string-append llhttp "/src/api.c")
       ;;                  "deps/llhttp/src/api.c")
       ;;       (copy-file (string-append llhttp "/src/http.c")
       ;;                  "deps/llhttp/src/http.c")
       ;;       (copy-file (string-append llhttp "/include/llhttp.h")
       ;;                  "deps/llhttp/include/llhttp.h"))))

       )))


    )))
