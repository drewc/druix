(define-module (druix utils go-packages)
  #:use-module (druix utils)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:export
  (ensure-go-druix-versions
   make-go-package
   define-public-go-package))

(define (git-repo<-import-path ip)
  (string-append "https://" ip ".git"))

 (define (package-name<-import-path ip)
   (list->string
    (cons* #\g #\o #\-
           (map (lambda (c) (if (member c '(#\. #\/)) #\- c))
                (string->list ip)))))
(define (home-page<-import-path ip)
  (string-append "https://" ip ))

(define* (ensure-go-druix-versions ip #:key (unpack-path ip) . args)
  (apply (@ (druix versions) ensure-druix-versions)
         (string->symbol (package-name<-import-path ip))
         #:repo (git-repo<-import-path unpack-path)
         args))

(define* (make-go-package
          ip synopsis
          #:key
          (home-page (home-page<-import-path ip))
          (description synopsis)
          (license l:expat)
          (inputs '())
          (phases '%standard-phases)
          (unpack-path #f))
  (let* ((name (package-name<-import-path ip))
         (latest-version
          (module-ref (resolve-module `(druix versions ,(string->symbol name)))
                      'latest)))
    (package
      (name name) (version (v:druix-version latest-version))
      (synopsis synopsis) (home-page home-page) (description description)
      (license license) (build-system go-build-system)
      (arguments `(#:import-path ,ip #:phases ,phases
                   ,@(if (not unpack-path) '()
                         (list #:unpack-path unpack-path))))
      (inputs inputs)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (v:repo latest-version))
               (commit (v:commit latest-version))))
         (sha256 (base32 (v:sha256 latest-version)))
         (file-name (git-file-name name version)))))))

(define-macro (define-public-go-package ip synop . args)
    `(define-public ,(string->symbol (package-name<-import-path ip)) (apply make-go-package ,ip ,synop (list ,@args))))
