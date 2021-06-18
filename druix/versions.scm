(define-module (druix versions)
  #:use-module (oop goops)
  #:use-module (druix utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)
  #:use-module (guix build utils)
  #:use-module (ice-9 pretty-print)
  #:export
  (<druix-version>
   name major minor patch revision ymd hms sha256

   <druix-version-git>
   repo commit

   alist<-parse-druix-version
   druix-version<-git-repo

   druix-version
   form<-druix-version
   form<-define-versions-module
   string<-druix-module-definition

   ensure-druix-versions

   update-druix-version update-druix-versions
   find-druix-versions

   string<-druix-versions
   new-versions-file-values<-druix-package-name))

;;; (use-modules (oop goops))

(define-class <druix-version> ()
  (name #:accessor name #:init-value "unnamed" #:init-keyword #:name)
  (major #:accessor major #:init-value 0 #:init-keyword #:major)
  (minor #:accessor minor #:init-value 0  #:init-keyword #:minor)
  (patch #:accessor patch #:init-value #f #:init-keyword #:patch)
  (revision #:accessor revision #:init-value #f #:init-keyword #:revision)
  (ymd #:accessor ymd #:init-value 19700101 #:init-keyword #:ymd)
  (hms #:accessor hms #:init-value 000000 #:init-keyword #:hms)
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

(define-generic form<-druix-version)
(define-method (form<-druix-version (v <druix-version>))
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

(define (form<-define-versions-module name)
  `(define-module
     (druix versions ,(if (string? name) (string->symbol name) name))
     #:use-module (druix versions)
     #:use-module (oop goops)
     #:export (versions latest)))

(define (string<-druix-versions vs)
  (with-output-to-string
    (lambda ()
      (display "(define versions \n  (list \n")
      (let ((one #t))
      (map (lambda (form)
             (if (not one) (newline) (set! one #f))
             (display "    ")
             (pretty-print form))
           (map form<-druix-version vs))
      (display "))")
      (newline)))))

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
  (define gbranch (with-directory-excursion grepo
      ($cmd "git" "branch" "--show-current")))
  (define vrev
    (string->number
     (with-directory-excursion grepo
      ($cmd "git" "rev-list" gbranch "--count"))))
  (define gcommit (git-repo-current-commit grepo))

  (define gdesc (git-repo-describe--tags grepo))
  (define valist (catch #t (lambda ()
                             (alist<-parse-druix-version gdesc))
                       (lambda _ `((revision . ,vrev)))))

  (define gsha256 (sha256<-directory grepo))
  (define vrepo
    (with-directory-excursion grepo
      ($cmd "git" "remote" "get-url" "origin")))
  (define gymd
    (with-directory-excursion grepo
      ($cmd "sh" "-c" "TZ=UTC git show --quiet --date='format-local:%Y%m%d' --format=%cd")))
  (define ghms
    (with-directory-excursion grepo
      ($cmd "sh" "-c" "TZ=UTC git show --quiet --date='format-local:%H%M%S' --format=%cd")))

  (make klass
    #:major (assoc-ref valist 'major)
    #:minor (assoc-ref valist 'minor)
    #:patch (assoc-ref valist 'patch)
    #:revision (assoc-ref valist 'revision)
    #:ymd (string->number gymd)
    #:hms (string->number ghms)
    #:repo vrepo
    #:commit gcommit
    #:sha256 gsha256))

(define (string<-druix-module-definition name versions)
  ;; => string
  (with-output-to-string
    (lambda ()
      (write (form<-define-versions-module name))
      (newline)
      (display (string<-druix-versions versions))
      (newline)
      (write '(define latest (car versions)))
      (newline))))

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

(define (write-druix-versions-file name versions)
  (define vfldr (druix-versions-folder))
  (define fname (string-append vfldr "/" (symbol->string name) ".scm"))
  (with-output-to-file fname
    (lambda () (display (string<-druix-module-definition name versions))))
  fname)

(define-generic ensure-druix-versions-from-object)
(define-method (ensure-druix-versions-from-object
                sym (obj <druix-version-git>))
  (define vpath (get-druix-versions-path sym))
  (define vfldr (druix-versions-folder))
  (let* ((repo (ensure-git-repo (repo obj)))
         (oldvs (if vpath (find-druix-versions sym) '()))
         (newv (let ((nv (druix-version<-git-repo (class-of obj) repo)))
                 (set! (name nv) (name obj))
                 (if (not (major nv))
                     (set! (major nv) (major obj)))
                 (if (not (minor nv))
                     (set! (minor nv) (minor obj)))
                 (if (not (patch nv))
                     (set! (patch nv) (patch obj)))
                 nv))
         (restvs (if (null? oldvs) oldvs
                     (if (equal? (commit (car oldvs)) (commit newv))
                         (cdr oldvs)
                         oldvs)))
         (versions (cons newv restvs)))
    (write-druix-versions-file sym versions)
    (let ((m (resolve-module `(druix versions ,sym))))
     (and m (reload-module m)))
    versions))

(define* (ensure-druix-versions
          sname #:optional (klass <druix-version-git>)
          #:key (name (if (string? sname) sname (symbol->string sname)))
          #:allow-other-keys #:rest args)
  (ensure-druix-versions-from-object
   sname (apply make klass #:name name args)))
