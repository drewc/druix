(define-module (druix services gitea)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (druix packages gitea)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (gitea-configuration
            gitea-configuation?
            gitea-service-type))
(display "Gitea service\n\n")

(define-record-type* <gitea-configuration>
  gitea-configuration make-gitea-configuration
  gitea-configuation?
  (gitea gitea-configuration-gitea      ;file-like
         (default gitea))
  (work-path gitea-work-path
             (default "/var/lib/gitea"))
  (custom-path gitea-custom-path
               (default #t))
  (config gitea-config-path
          (default #t))
  (homedir gitea-user-home
           (default #t))
  (pid gitea-pid
       (default "/var/run/gitea.pid"))
  (port gitea-web-port
        (default 3000))
  (extra-options gitea-configuration-options ;list of strings
                 (default '())))

(define gitea-activation
  (match-lambda
    (($ <gitea-configuration>
        gitea work-path custom-path conf home pid port extra-options)
     #~(begin
         (use-modules (guix build utils))

         (let ((user (getpwnam "gitea")))
           (mkdir-p #$work-path)
           (chown #$work-path (passwd:uid user) (passwd:gid user))
           )))))

(define %gitea-accounts
  (list (user-group (name "gitea") (system? #t))
        (user-account
         (name "gitea")
         (group "gitea")
         (system? #t)
         (comment "gitea server user")
         (home-directory "/var/lib/gitea")
         (shell "/bin/sh"))))
;(file-append shadow "/sbin/nologin")

(define gitea-shepherd-service
  (match-lambda
    (($ <gitea-configuration>
        gitea work-path custom-path config home pid port extra-options)
     (let* ((kustom
             (or (if (string? custom-path) custom-path #f)
                 (if (and custom-path work-path)
                     (string-append work-path "/custom")
                     #f)))
            (konf (or (if (string? config) config #f)
                      (if (and config kustom)
                          (string-append kustom "/conf/app.ini")
                          #f)))
            (homed (or (if (string? home) home #f)
                       (if work-path work-path
                           #f))))
       (format #t "here")
       (list (shepherd-service
              (provision '(gitea))
              (documentation "Run `gitea web` as a daemon")
              (requirement '(networking))
              (start #~(make-forkexec-constructor
                        (list
                        ; "/usr/bin/env"
                        ; (string-append "HOME=" #$homed)
                         ;; "echo"
                         (string-append #$gitea "/bin/gitea")
                                        ;"web"
                         #$@(if work-path
                                #~("--work-path" #$work-path)
                                #~())
                         #$@(if konf
                                #~("--config" #$konf)
                                #~())
                         #$@(if kustom
                                #~("--custom-path" #$kustom)
                                #~())
                         #$@(if pid
                                #~("--pid" #$pid)
                                #~())
                         #$@(if port
                                #~("--port" #$port)
                                #~())
                         #$@extra-options)
                        #:user "gitea"
                        #:log-file (string-append "/var/log" "/gitae.log")))
              (stop #~(make-kill-destructor))))))))

(define gitea-service-type
  (service-type (name 'gitea)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          gitea-shepherd-service)
                       (service-extension activation-service-type
                                          gitea-activation)
                       (service-extension account-service-type
                                          (const %gitea-accounts))))
                                        ;(compose concatenate)
                (default-value
                  (gitea-configuration))
                (description "Run the Gitea Web server.")))
