(define-module (druix services nfs-client)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (nfs-client-service-type))
(use-modules (guix modules))   ;for 'source-module-closure'
(use-modules (gnu packages linux))   ;for 'util-linux'

(define (host<-file-system fs)
  (let* ((source (file-system-device fs))
         (idx (string-rindex source #\:))
         (host-part (string-take source idx))
         ;; Strip [] from around host if present
         (host (match (string-split host-part (string->char-set "[]"))
                 (("" h "") h)
                 ((h) h))))
    host))

(define (nfs-mount-daemon file-system cname)
  #~(begin
      (use-modules (ice-9 format))
      (define target #$(file-system-mount-point file-system))
      (define host #$(host<-file-system file-system))
      (define (mountpoint)
        (system* #$(file-append util-linux "/bin/mountpoint")
                 target))

      (define (herd-me)
        (system* #$(file-append shepherd "/bin/herd")
                 "restart" (symbol->string '#$cname)))

      (define timein 5)         ; the number of seconds to wait between attempts
      (define (sleep-me)
        (sleep
         (cond
          ((< timein 20) (let ((ti timein)) (set! timein (* 2 timein)) ti))
          ((< timein 25) (set! timein (+ 1 timein)) timein)
          ((= 25 timein) (set! timein 30) timein)
          ((< timein 120) (let ((ti timein)) (set! timein (* 2 timein)) ti))
          ((>= timein 120) timein))))


      (define (log . args) (format #t "~a: ~{~a~}\n" target args))

      (define (host-exists? host)
        (define (host?) (getaddrinfo host))
        (with-exception-handler
            (const #f)
          host?
          #:unwind? #t))

      (while #t
         (if (eq? 5 timein)
             (log "Starting Investigation for " target " and " '#$cname
                  "\n mounted?" (mountpoint) "  "
                  (host-exists? host) " <--Host exists?")
             (log "Still trying after " timein " seconds"))
        (cond
         ((zero? (mountpoint))
          (sleep-me))
          ((host-exists? host)
           (log "host "host" exists. Running herd start " '#$cname)
           (herd-me) (sleep-me))
         (#t
;;; We will respawn, but we are only allowed so many before it
;;; disables the service it seems. Let's sleep a bit.
          (sleep 10)
          (exit 0))))))

(define (nfs-client-shepherd-services file-systems)
  (define (shep-serv fs)
    (let* ((fss (nfs-file-system-shepherd-service fs))
           (cname (and fss (shepherd-service-canonical-name fss)))
           (mounts (and cname (nfs-mount-shepherd-service fs cname))))
      (if fss (list mounts fss) '())))
                                        ;
  (apply append (map shep-serv file-systems)))

(define (nfs-file-system-shepherd-service file-system)
  (let* ((ss (@@ (gnu services base) file-system-shepherd-service))
         (fsss (ss file-system))
         (target  (file-system-mount-point file-system))
         (create? (file-system-create-mount-point? file-system))
         (mount?  (file-system-mount? file-system)))
    (and (or mount? create?)
         (with-imported-modules (source-module-closure
                                 '((gnu build file-systems)))
           (shepherd-service
            (inherit fsss)
            (documentation "Mount (via fstab) and unmount the given file system.")
            (auto-start? #f)
            (start
             #~(lambda args
                 #$(if create?
                       #~(mkdir-p #$target)
                       #t)
                 #$(if mount?
                       #~(system*
                        #$(file-append util-linux "/bin/mount")
                        "--target" #$target)
                       #t)
                 #t)))))))

(define (nfs-mount-shepherd-service file-system cname)
  (let* ((daemon
          (program-file
           "nfs-client-daemon" (nfs-mount-daemon file-system cname))))
    (with-imported-modules
        (source-module-closure
         '((guix build syscalls)
           (ice-9 format)))
      (shepherd-service
       (provision (list (string->symbol (format #f "nfs-mount-~a" cname))))
       (requirement (list 'file-systems 'networking))
       (start #~(lambda args
                  (define target #$(file-system-mount-point file-system))
                  (define (mountpoint)
                    (system* #$(file-append util-linux "/bin/mountpoint")
                             target))

                  (system "echo Trying to mount or keep trying nfs >> /tmp/nfs.log")
                  (if (zero? (mountpoint))
                      #t
                      (apply (make-forkexec-constructor
                              (list #$daemon)
                              #:log-file "/tmp/nfs.log")
                             args))))
       (stop #~(make-kill-destructor))))))

(define nfs-client-service-type
  (service-type
   (name 'nfs-client)
   (extensions
    (list  (service-extension shepherd-root-service-type
                              nfs-client-shepherd-services)
           (service-extension fstab-service-type identity)
           ;; (service-extension file-system-service-type
           ;;                    nfs-file-systems)
           ))
   (description "NFS client for file-systems")))
