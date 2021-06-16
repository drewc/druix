(define-module (druix build gerbil-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases gerbil-build))

(define %lib-prefix "/lib/gerbil")
(define %pkg-prefix "/share/gerbil")

(define (empty-directory? path)
  (define d (opendir path))
  (eof-object? (begin (readdir d) (readdir d) (readdir d))))

(define (path-exists? path)
  (access? path F_OK))

(define (gerbil-loadpath inputs)
  (define paths
    (filter path-exists?
            (map (lambda (i)
                       (string-append (cdr i)
                                      "/lib/gerbil/lib"))
                     inputs)))
  (string-join
           (append
            (let ((v (getenv "GERBIL_LOADPATH")))
              (if v (list v) '()))
            paths)
           ":"))

(define (setenv-GERBIL_LOADPATH inputs)
  (setenv "GERBIL_LOADPATH"
          (gerbil-loadpath inputs)))

(define (gxpkg-name outputs)
  (let* ((out (assoc-ref outputs "out")))
    (package-name->name+version
     (strip-store-file-name out))))

(define (gxpkg-source-directory outputs)
  (let ((out (assoc-ref outputs "out")))
    (string-append out %pkg-prefix "/" (gxpkg-name outputs))))

(define (gxpkg-lib-directory outputs)
  (let ((out (assoc-ref outputs "out")))
    (string-append out %lib-prefix)))

(define* (copy-source #:key outputs #:allow-other-keys)
  (copy-recursively "." (gxpkg-source-directory outputs))
  #t)

(define* (configure #:key inputs outputs #:allow-other-keys)
  (let ((gpath (gxpkg-lib-directory outputs)))
    (setenv "GERBIL_BUILD_CORES" (getenv "NIX_BUILD_CORES"))
    (setenv "GERBIL_PATH" gpath)
    (setenv-GERBIL_LOADPATH inputs)
    (setenv "GAMBOPT" "i8,f8,-8,t8")
    (invoke "echo" "GERBIL_LOADPATH=" (getenv "GERBIL_LOADPATH"))
    #t))



(define* (build #:key outputs (build-script "./build.ss") #:allow-other-keys)
  (with-directory-excursion (gxpkg-source-directory outputs)
    (invoke build-script))

  (let ((out (assoc-ref outputs "out"))
        (bin (string-append (gxpkg-lib-directory outputs)
                                              "/bin")))
    (if (not (empty-directory? bin))
      (invoke "cp" "-as" bin (string-append out "/bin"))
      (invoke "rmdir" bin)))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (replace 'configure configure)
    (add-before 'configure 'copy-source copy-source)
    ;; (delete 'build)
    (delete 'check)
    (replace 'build build)
   ;; (replace 'check check)
    (delete 'install)
   ; (replace 'strip strip)
    (delete 'strip)
    ))

(define* (gerbil-build
          #:key inputs
          (phases %standard-phases)
          #:allow-other-keys #:rest args)
  (apply gnu:gnu-build
         #:inputs inputs
         #:phases phases
         args))
