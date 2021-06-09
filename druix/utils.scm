(define-module (druix utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (guix build utils)
  #:export
  (cmd-line $cmd sha256<-directory
   git-clone-repo ensure-git-repo
   git-repo-current-commit
   git-repo-describe--tags))

(define (cmd-line command-line)
     (let* ((port (open-input-pipe command-line))
            (str (read-line port))) ; from (ice-9 rdelim)
  (close-pipe port)
  str))

(define ($cmd prog . args)
  (let* ((p (apply open-pipe* OPEN_READ prog args))
        (str (string-trim-right (get-string-all p) #\newline)))
    (close-pipe p)
    str))

(define (sha256<-directory dir)
  (with-directory-excursion dir
    (cmd-line "guix hash -xr .")))

(define (git-clone-repo repo . args)
  ;; => /"path of repo"/
  (define srcdir (mkdtemp "/tmp/gambc-XXXXXX"))
  (define src (basename repo ".git"))
  (with-directory-excursion srcdir
    (apply invoke "git" "clone" (append args (list repo)))
    (with-directory-excursion src (getcwd))))

(define (ensure-git-repo repo . args)
  (if (not (string-contains  repo ":")) repo
      (apply git-clone-repo repo args)))

(define (git-repo-current-commit repo . args)
  (define srcdir (apply ensure-git-repo repo args))
    (with-directory-excursion srcdir
      (cmd-line "git log -1 --format=\"%H\"")))

(define* (git-repo-describe--tags repo-or-url #:key (abbrev #f))
  (define repo (ensure-git-repo repo-or-url))
  (define (cmd)
    (cmd-line (apply string-append "git describe --tags"
           (if abbrev (list " --abbrev=" (number->string abbrev)) '()))))
  (with-directory-excursion repo (cmd)))

(define (git-repo-get-commit-and-sha256 repo)
  ;;;   => ("commit" . "sha256")

  (define srcdir (mkdtemp "/tmp/gambc-XXXXXX"))
  (define src (basename repo ".git"))
  (define commit #f)
  (define sha256 #f)

  (with-directory-excursion srcdir
    (invoke "git" "clone" "--depth=1" repo)
    (chdir src)
    (set! commit (cmd-line "git log -1 --format=\"%H\""))
    (set! sha256 (cmd-line "guix hash -xr .")))
  (cons commit sha256))
