(define-module (druix utils gerbil-packages)
  #:use-module (druix utils)
  #:use-module (druix build-system gerbil)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:export (gxpkg gxpkg/clan clan-package?))

(define* (gxpkg druix-version synopsis home-page license inputs
                     #:key
                     (description synopsis)
                     (name (v:name druix-version))
                     #:allow-other-keys)
  (let ((v druix-version))
    (package
      (name name) (synopsis synopsis) (license license)
      (home-page home-page) (description description)
      (build-system gerbil-build-system) (inputs inputs)
      (version (v:druix-version v))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (v:repo v))
               (commit (v:commit v))))
         (file-name (git-file-name name (v:commit v)))
         (sha256 (base32 (v:sha256 v)))))
      )))

(define (clan-package? pkg)
  (apply (lambda* (#:key (clan #f) #:allow-other-keys) clan)
   (package-arguments pkg)))

(define* (string<-clan-version name version imports date)
  (with-output-to-string
    (lambda ()
      (write (cons* 'import ':clan/versioning imports))
      (newline) (write `(register-software ,name ,version))
      (display " ;; ") (display date)
      (newline))))

(define (imports<-inputs inputs)
  (define (import-symbol clan)
    (apply (lambda* (#:key gerbil-package version-path #:allow-other-keys)
             (string->symbol (string-append ":" gerbil-package "/" version-path)))
           clan))
  (map import-symbol (filter identity (map clan-package?
                                           (map cadr inputs)))))

(define (package-with-clan-version pkg date)
  (define clan (clan-package? pkg))
  (define imports (imports<-inputs (package-inputs pkg)))
  (define* (%vstr #:key software-name #:allow-other-keys)
    (string<-clan-version
     software-name
     (string-append "v" (package-version pkg))
     imports date))
  (define vpath
    (apply
     (lambda* (#:key version-path #:allow-other-keys)
       (string-append version-path ".ss"))
     clan))
  (define vstr (apply %vstr clan))
  (define phases
    `(modify-phases %standard-phases
       (add-before 'copy-source 'write-clan-version
         (lambda _
           (with-output-to-file ,vpath
             (lambda () (display ,vstr)))
                 (invoke "echo" "Made" ,vpath "from" ,vstr)))))

  (package (inherit pkg)
           (arguments (cons* #:phases phases (package-arguments pkg)))))







(define* (gxpkg/clan druix-version synopsis home-page license inputs
                     #:key clan #:allow-other-keys #:rest args)

  (let ((parent
         (apply gxpkg druix-version synopsis home-page license inputs args)))
   (package-with-clan-version
     (package
       (inherit parent)
       (arguments `(#:clan ,clan)))
     ((@ (druix versions) ymd) druix-version)

        ))
)
