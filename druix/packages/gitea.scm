(define-module (druix packages gitea)
    #:use-module ((guix licenses) #:prefix license:)
    #:use-module (guix packages)
    #:use-module (guix download)
    #:use-module (guix gexp)
    #:use-module (guix build-system go)
    #:use-module (gnu packages bash)
    #:use-module (gnu packages node)
    #:use-module (gnu packages version-control))

(define-public gitea
  (package
    (name "gitea")
    (version "1.16.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/go-gitea/gitea/releases"
                                  "/download/v" version
                                  "/gitea-src-" version ".tar.gz"))
              (sha256
               (base32 "1q9hbg6fwi9gq8dwa9hi1giqz10h9y0xi0h03gjz0jci5i8xh4rg"))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'unpatch-example-shebangs
           ;; If we don't do this then git repos created with this version of
           ;; gitea will use the build environment's bash for the different
           ;; git repo hooks.
           (lambda _
             (substitute*
               (find-files "src/integrations/gitea-repositories-meta"
                           "(\\.sample|gitea|(post|pre)-receive|update)")
               (("#!/gnu/store/.*/bin/bash") "#!/bin/bash")
               (("#!/gnu/store/.*/bin/sh") "#!/bin/sh"))))
         (add-before 'build 'prepare-build
           (lambda _
             (setenv "TAGS" "bindata sqlite sqlite_unlock_notify")
             (setenv "GITEA_WORK_DIR" "/var/lib/gitea")))
         (replace 'build
           (lambda _
             (with-directory-excursion "src"
               (invoke "make" "build")
               (invoke "make" "generate-manpage"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (unsetenv "GITEA_WORK_DIR")
               (with-directory-excursion "src"
                 (invoke "make" "test-backend")
                 ;; Gitea requires git with lfs support to run tests.
                 ;(invoke "make" "test-sqlite")
                 (invoke "make" "test-sqlite-migration")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "src"
                 (invoke "make" "install")
                 (install-file "man/man1/gitea.1.gz"
                               (string-append out "/share/man/man1"))))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/gitea")))
               (wrap-program bin
                 `("PATH" ":" prefix
                   (,(dirname (search-input-file inputs "/bin/git")))))))))))
    ;(native-inputs
    ; (list node-lts))
    (inputs
     (list bash-minimal
           git))
    (home-page "https://gitea.io/")
    (synopsis "Self-hosted git service")
    (description "Gitea is an open-source forge software package for hosting
software development version control using Git as well as other collaborative
features like bug tracking, wikis and code review.")
    (properties
      '((release-monitoring-url . "https://github.com/go-gitea/gitea/releases")))
    (license license:expat)))
