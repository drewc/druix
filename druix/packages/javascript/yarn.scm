(define-module (druix packages javascript yarn)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix licenses)
  #:use-module (gnu packages node)
  #:use-module (ice-9 pretty-print))

(define yarn-version "1.22.5")

(define-public yarn
  (package
   (name "yarn")
   (version yarn-version)
   (source (origin
             (method url-fetch/tarbomb)
             (uri (string-append "https://github.com/yarnpkg/yarn/releases/download/v"
                                 version
                                 "/yarn-v"
                                 version
                                 ".tar.gz"))

             (sha256
              (base32
              "0hf15amsn54cbsy4pvqcq2c03d1l11w3799p19sympz4j93gnr66"))))
   (build-system trivial-build-system)
   (outputs '("out"))
   (inputs `(("node" ,node-lts)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
   		  (use-modules (guix build utils))
                  (let* ((out (assoc-ref %outputs "out"))
			 (bin (string-append  out "/bin"))
			 (lib (string-append  out "/lib"))
                         (node-modules (string-append lib "/node_modules"))
                         (yarn (string-append node-modules "/yarn"))
                         (input-dir (string-append (assoc-ref %build-inputs "source") "/yarn-v" ,version)))

		    (mkdir-p yarn)
		    (mkdir-p bin)
   		    (copy-recursively (string-append input-dir "/") yarn)
		    (symlink (string-append yarn "/bin/yarn") (string-append bin "/yarn"))
		    (symlink (string-append yarn "/bin/yarnpkg") (string-append bin "/yarnpkg"))
		    (delete-file (string-append yarn "/bin/yarn.cmd"))
		    (delete-file (string-append yarn "/bin/yarnpkg.cmd"))
   		    ))))
   (home-page "https://yarnpkg.com/")
   (synopsis "Dependency management tool for JavaScript")
   (description "Fast, reliable, and secure dependency management tool
for JavaScript.  Acts as a drop-in replacement for NodeJS's npm.")
   (license bsd-2)))
