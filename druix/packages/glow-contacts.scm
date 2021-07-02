(define-module (druix packages glow-contacts)
  #:use-module (druix packages scheme gerbil)
  #:use-module (druix packages scheme gerbil gerbil-utils)
  #:use-module (druix packages scheme gerbil gerbil-ftw)
  #:use-module (druix packages scheme gerbil smug-gerbil)
  #:use-module (druix packages scheme gerbil gerbil-poo)
  #:use-module (druix packages scheme gerbil gerbil-libp2p)
  #:use-module (druix packages scheme gerbil gerbil-crypto)
  #:use-module (druix packages scheme gerbil gerbil-persist)
  #:use-module (druix packages scheme gerbil gerbil-ethereum)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages busybox)
  #:use-module (gnu packages compression)
  #:use-module (druix packages glow-lang)
  #:use-module (druix utils gerbil-packages))

(define-public gloui-spa
  (package
    (name "gloui-spa")
    (version "0.5.1")
    (synopsis "gloUI, a UI for glow, which is a programming language used to make decentralized applications.")
    (description synopsis)
    (home-page "https://github.com/drewc/gloui")
    (license (@ (guix licenses) expat))
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri "https://github.com/drewc/gloui/releases/download/v0.5.1/gloui-SPA-0.5.1.tar.gz")
       (sha256
        (base32 "0yvkyd6xg9x1pgzxhpyz6v4mrvshainil0w2j7yplqrk65vwjyck"))))
   (build-system copy-build-system)
   (arguments
  `(#:phases
     (modify-phases %standard-phases
       (delete 'reset-gzip-timestamps))))))

(define shebang
  '(lambda _
     (display "#!/bin/sh\n")
     (display "ORIG_GERBIL_LOADPATH=\"$GERBIL_LOADPATH\"\n")
     (display "ORIG_GERBIL_PATH=\"$GERBIL_PATH\"\n")
     (display "unset GERBIL_HOME\n")
     (display "GERBIL_LOADPATH=") (write (gerbil-loadpath outputs)) (newline)
     (display "GERBIL_PATH=\"$HOME/.cache/glow/gerbil\"\n")
     (display "export GERBIL_PATH GERBIL_LOADPATH GLOW_SOURCE ORIG_GERBIL_PATH ORIG_GERBIL_LOADPATH\n")

     ;;; Ok, is there a database?
     (display "GLOW_CONTACTS_DB=\"$HOME/.config/glow/db/contacts.db\"\n")
     ;;; If not, create one.
     (display "if [ ! -f \"$GLOW_CONTACTS_DB\" ]; then\n")
     (display (string-append out "/bin/make-glow-contacts-db \n fi\n"))

     ;;; Now the wwwroot
     (display "WWWROOT=$HOME/.config/glow/wwwroot\n")
     ;;;  If it exist and is not a symlink, warn and continue
     (display (string-append
               "if [ -d \"$WWWROOT\" -a ! -h \"$WWWROOT\" ];\n then\n"
               "   echo WARNING $WWWROOT is a directory and not a symlink."
               "Consider changing it and proceed with caution. \n"
               "else \n"))
     ;;; if it is a symlink delete it.
     (display "  [[ -h \"$WWWROOT\" ]] && rm $WWWROOT;\n")
     ;;; ... and finally create the symlink
     (display (string-append "ln -s " gloui-spa " \"$WWWROOT\" ; \n fi \n"))


     (display "exec ") (display gerbil) (display "/bin/gxi ") (display out)
     (display "/lib/gerbil/lib/mukn/glow-contacts/main.ss \"$@\"")))

(define-public glow-contacts
  (gxpkg/clan
   (@ (druix versions glow-contacts) latest)
   "Glow Contacts for Decentralized Applications (DApps)"
   "https://gitlab.com/drewc/glow-contacts" (@ (guix licenses) asl2.0)
   `(("gerbil" ,gerbil-unstable)
     ("gerbil-utils" ,gerbil-utils)
     ("gerbil-poo" ,gerbil-poo)
     ("gerbil-persist" ,gerbil-persist)
     ("gerbil-libp2p" ,gerbil-libp2p)
     ("gerbil-ethereum" ,gerbil-ethereum)
     ("smug-gerbil" ,smug-gerbil)
     ("gerbil-crypto" ,gerbil-crypto)
     ("gerbil-ftw" ,gerbil-ftw)
     ("gloui-spa" ,gloui-spa)
     ("glow-lang", glow-lang)
     ("busybox", busybox))
   #:clan '(#:software-name "Glow Contacts"
            #:gerbil-package "mukn/glow-contacts"
            #:version-path "version")
   #:arguments
  `(#:phases
     (modify-phases %standard-phases
      ;; (add-before 'copy-source 'patch-glow-install-path)
       (add-after 'build
           'shebang-glow
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (gerbil (assoc-ref inputs "gerbil"))
                  (gloui-spa (assoc-ref inputs "gloui-spa"))
                  (db (string-append out "/share/glow-contacts/db")))
             (install-file
              "main.ss"
              (string-append out "/lib/gerbil/lib/mukn/glow-contacts/"))
             (with-output-to-file "glow-contacts"
               ,shebang)
             (chmod "glow-contacts" #o755)
             (install-file "glow-contacts" (string-append out "/bin"))
             (invoke "echo" "Made a shebang") (newline)
             (invoke "cat" "glow-contacts") (newline)
             (copy-recursively "db" db)
             (with-output-to-file "make-glow-contacts-db"
               (lambda _(display (string-append
                         "#!/bin/sh\n cd " db "\n"
                         "./makedb.sh"))))
             (chmod "make-glow-contacts-db" #o755)
             (install-file "make-glow-contacts-db" (string-append out "/bin"))
             #t)))))))
