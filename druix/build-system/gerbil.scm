(define-module (druix build-system gerbil)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (ice-9 match)
  #:use-module (guix search-paths)
  #:export (gerbil-build
            gerbil-build-system))

(define %gerbil-build-modules
  '((druix build gerbil-build-system)
    (guix build utils)))

(define %gerbil-build-system-modules
  (append %gerbil-build-modules
          %gnu-build-system-modules))

(define* (gerbil-build
          store name inputs
                  #:key
                  source outputs
                  (tests? #t)
                  (phases '(@ (druix build gerbil-build-system)
                              %standard-phases))
                  (system (%current-system))
                  (search-paths '())
                  (imported-modules %gerbil-build-system-modules)
                  (guile #f)
                  (modules %gerbil-build-modules) #:allow-other-keys)
  (define builder
    `(begin
       (use-modules ,@modules)
       (gerbil-build #:name ,name
                     #:system ,system
                     #:source ,(match (assoc-ref inputs "source")
                                   (((? derivation? source))
                                    (derivation->output-path source))
                                   ((source) source)
                                   (source source))
                     #:phases ,phases
                     #:outputs %outputs
                     #:inputs %build-inputs
                     #:search-paths ',(map search-path-specification->sexp
                                             search-paths))))
  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define* (lower name #:key source inputs outputs native-inputs system target
                #:allow-other-keys #:rest arguments)
  (define private-keywords
    '(#:target #:inputs #:native-inputs ))
  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,@(standard-packages)))
    (build-inputs native-inputs)
    (outputs outputs)
    (build gerbil-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define gerbil-build-system
  (build-system
    (name 'gerbil)
    (description "The build system for Gerbil packages")
    (lower lower)))
