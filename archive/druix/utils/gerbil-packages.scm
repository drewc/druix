(define-module (druix utils gerbil-packages)
  #:use-module (druix utils)
  #:use-module (druix utils plists)
  #:use-module (druix build-system gerbil)
  #:use-module ((druix versions) #:prefix v:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (ice-9 optargs)
  #:use-module (gnu packages base)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (druix build gerbil-build-system)
  #:export (gxpkg gxpkg/clan clan-package?))

(define* (gxpkg druix-version synopsis home-page license inputs
                     #:key
                     (description synopsis)
                     (name (v:name druix-version))
                     (arguments '())
                     #:allow-other-keys)
  (let ((v druix-version))
    (package
      (name name) (synopsis synopsis) (license license)
      (home-page home-page) (description description)
      (build-system gerbil-build-system) (inputs inputs)
      (version (v:druix-version v))
      (arguments arguments)
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
  (define args (package-arguments pkg))
  (define imports (imports<-inputs (package-inputs pkg)))

  (define* vstr
    (string<-clan-version
     (plist-get clan #:software-name)
     (string-append "v" (package-version pkg))
     imports date))
  (define vpath
    (string-append (plist-get clan #:version-path) ".ss"))

  (define our-phases
    (let-keywords
     (package-arguments pkg) #t
     ((phases '%standard-phases))
     `(begin
        (let ((old-phases ,phases))
          (modify-phases old-phases
            (add-before 'copy-source 'write-clan-version
              (lambda _
                (with-output-to-file ,vpath
                  (lambda () (display ,vstr)))
                (invoke "echo" "Made" ,vpath "from: \n" ,vstr)
                #t)))))))

  (define parent-args (plist-delete args #:phases))

  (package
    (inherit pkg)
    (arguments (cons* #:phases our-phases parent-args))))

(define* (gxpkg/clan druix-version synopsis home-page license inputs
                     #:key clan #:allow-other-keys #:rest args)

  (let ((parent
         (apply gxpkg druix-version synopsis home-page license inputs args)))
   (package-with-clan-version
     (package
       (inherit parent)
       (arguments `(#:clan ,clan ,@(package-arguments parent))))
     ((@ (druix versions) ymd) druix-version))))
