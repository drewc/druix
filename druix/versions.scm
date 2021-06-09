(define-module (druix versions)
  #:use-module (oop goops)
  #:use-module (druix utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (guix build utils)
  #:export
  (<druix-version>
   major minor patch revision sha256

   <druix-version-git>
   repo commit

   alist<-parse-druix-version
   druix-version<-git-repo

   druix-version form<-druix-version
   make-define-module-form-for-versions

   ensure-druix-versions

   update-druix-version update-druix-versions
   find-druix-versions

   string<-druix-versions
   new-versions-file-values<-druix-package-name))

;;; (use-modules (oop goops))

(define-class <druix-version> ()
  (major #:accessor major #:init-value 0 #:init-keyword #:major)
  (minor #:accessor minor #:init-value 0  #:init-keyword #:minor)
  (patch #:accessor patch #:init-value #f #:init-keyword #:patch)
  (revision #:accessor revision #:init-value #f #:init-keyword #:revision)
  (sha256 #:accessor sha256 #:init-value #f #:init-keyword #:sha256))
(define-generic druix-version)

(define-method (druix-version (v <druix-version>))
  (string-append
   (number->string (major v)) "."
   (number->string (minor v))
   (let ((p (patch v)))
     (if p (string-append "." (number->string p)) ""))
   (let ((rev (revision v)))
     (if rev (string-append "-" (number->string rev)) ""))))

(define-generic make-form<-druix-version)
(define-method (make-form<-druix-version (v <druix-version>))
  (define slots
    (filter (lambda (sd)
              (slot-bound? v (slot-definition-name sd)))
            (filter slot-definition-init-keyword
                    (class-slots (class-of v)))))

  `(make ,(class-name (class-of v))
     ,@(let sdv ((sds slots))
    (if (null? sds) sds
        (let ((sd (car sds)))
          (cons* (slot-definition-init-keyword sd)
                 (slot-ref v (slot-definition-name sd))
                 (sdv (cdr sds))))))))

(define-class <druix-version-git> (<druix-version>)
  (repo #:accessor repo #:init-keyword #:repo)
  (commit #:accessor commit #:init-keyword #:commit))

(define-method (druix-version (v <druix-version-git>))
  (define c (string-copy (commit v) 0 8))
  (string-append (next-method) "-g" c))

(define (make-define-module-form-for-versions name)
  `(define-module
     (druix versions ,(if (string? name) (string->symbol name) name))
     #:use-module (druix versions)
     #:use-module (oop goops)
     #:export (versions)))


;; => Either a new version or #f if it does not need updating
(define-generic update-druix-version)

(define-method (update-druix-version (v <druix-version>))
  ;; For now just say it does not need updating
  #f)

(define-method (update-druix-version (v <druix-version-git>))
  (let* ((src (git-clone-repo (repo v) "--depth=1"))
         (src-commit (git-repo-current-commit src)))
    (if (string=? src-commit (commit v)) #f
        (make (class-of v)
          #:major (major v) #:minor (minor v) #:patch (patch v)
          #:revision (1+ (revision v))
          #:repo (repo v)
          #:commit src-commit
          #:sha256 (sha256<-directory src)))))

(define (update-druix-versions pkg-name)
  (define versions (eval `(@ (druix versions ,pkg-name) versions)
                         (interaction-environment)))
  (define main-version (car versions))
  (define update? (update-druix-version main-version))

  (if (not update?) #f
      (cons update? versions)))

(define (string<-druix-versions vs)
  (with-output-to-string
    (lambda ()
      (display "(define versions \n  (list \n")
      (let ((one #t))
      (map (lambda (form)
             (if (not one) (newline) (set! one #f))
             (display "    ")
             (write form))
           (map make-form<-druix-version vs))
      (display "))")
      (newline)))))

(define (new-versions-file-values<-druix-package-name pkg-name)
  ;; (values new-versions? pathname contents-as-string)
  (define path (%search-load-path
                (string-append "druix/versions/" (symbol->string pkg-name))))
  (define new-versions? (update-druix-versions pkg-name))
  (define cnts
    (if new-versions?
        (with-output-to-string
          (lambda ()
            (write (make-define-module-form-for-versions pkg-name))
            (newline)
            (display (string<-druix-versions new-versions?))))
        (call-with-input-file path get-string-all)))
  (values (if (not (eq? new-versions? #f)) #t #f)
          path
          cnts))

(define (alist<-parse-druix-version str)
  (define version '())
  (define semantic '(major minor patch))
  (define (vnum s start)
    (string-match "^[v|\\.]([0-9]+)" s start))
  (define (rev s start)
    (string-match "-([0-9]+)-" s start))
  (let vnums ((t semantic)
              (start 0))
    (define m?
      (if (eq? #t t)
          ;; not on revision and failed
          #f
          (if (null? t)
              ;; done semantic, onto revision
              (rev str start)
              ;; semantic version
              (vnum str start))))
    (if (not m?)
        (if (not (null? t))
            ;;; try for revision which is always there.
            (vnums '() start)
            (if (null? version) #f (reverse version)))
        ;;; there was a match! put it in versions and continue
        (let ((n (eval-string (match:substring m? 1)))
              (s (match:end m?)))
          (set! version
                (cons* (cons (if (null? t) 'revision (car t)) n)
                       version))
          (vnums (or (null? t) (cdr t)) s)))))

(define (druix-version<-git-repo klass repo-or-uri . uri-args)
  (define grepo (apply ensure-git-repo repo-or-uri uri-args))
  (define gcommit (git-repo-current-commit grepo))
  (define gdesc (git-repo-describe--tags grepo))
  (define valist (alist<-parse-druix-version gdesc))
  (define gsha256 (sha256<-directory grepo))
  (define vrepo
    (with-directory-excursion grepo
      ($cmd "git" "remote" "get-url" "origin")))
  (make klass
    #:major (assoc-ref valist 'major)
    #:minor (assoc-ref valist 'minor)
    #:patch (assoc-ref valist 'patch)
    #:revision (assoc-ref valist 'revision)
    #:repo vrepo
    #:commit gcommit
    #:sha256 gsha256))

(define (get-druix-versions-path pkg-name)
  (%search-load-path
   (string-append "druix/versions/" (symbol->string pkg-name))))

(define (druix-versions-folder)
  (string-append (dirname (%search-load-path "druix/versions"))
                 "/versions"))

(define (find-druix-versions pkg-name)
  (define dvp (get-druix-versions-path pkg-name))
  (if (not dvp) #f
      (eval `(@ (druix versions ,pkg-name) versions)
            (interaction-environment))))

(define (ensure-druix-versions name klass . args)
  (define obj (apply make klass args))
  (ensure-druix-versions-from-object name obj))

(define (create-druix-versions-file name versions)
  ;; => string
  (with-output-to-string
    (lambda ()
      (write (make-define-module-form-for-versions name))
      (newline)
      (display (string<-druix-versions versions)))))

(define (write-druix-versions-file name versions)
  (define vfldr (druix-versions-folder))
  (define fname (string-append vfldr "/" (symbol->string name) ".scm"))
  (with-output-to-file fname
    (lambda () (display (create-druix-versions-file name versions))))
  fname)

(define-generic ensure-druix-versions-from-object)
(define-method (ensure-druix-versions-from-object
                name (obj <druix-version-git>))
  (define vpath (get-druix-versions-path name))
  (define vfldr (druix-versions-folder))
  (if (not vpath)
      (let* ((repo (ensure-git-repo (repo obj)))
             (versions (list (druix-version<-git-repo (class-of obj) repo))))
        (write-druix-versions-file name versions))
      (let ((versions (update-druix-versions name)))
        (if (not versions) vpath
            (write-druix-versions-file name versions)))))
