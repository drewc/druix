;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nongnu packages emacs)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (nonguix licenses)
  #:use-module (nongnu packages lisp))

(define-public clhs
  (package
    (name "clhs")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://beta.quicklisp.org/archive/clhs/2015-04-07/clhs-"
                           version
                           ".tgz"))
       (sha256
        (base32
         "1cn5bfrcawrbc8s1wb07lpr6xv8758l1n5pgkyhamagmi1r0x128"))))
    (build-system emacs-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-loader
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (delete-file "clhs-use-local.el")
             (let* ((out (assoc-ref outputs "out"))
                    (hyperspec-dir (string-append "file://" out
                                                  "/share/HyperSpec-7-0/")))
               (with-output-to-file "clhs.el"
                 (lambda ()
                   (format #t ";;;###autoload~%~s~%~%~s"
                           `(defun clhs-setup ()
                              (setq common-lisp-hyperspec-root ,hyperspec-dir))
                           `(provide 'clhs)))))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (hyperspec-dir (string-append out "/share/HyperSpec-7-0/")))
               (mkdir-p hyperspec-dir)
               (copy-recursively "HyperSpec-7-0/HyperSpec" hyperspec-dir)))))))
    (home-page "http://quickdocs.org/clhs/")
    (synopsis "Offline Common Lisp HyperSpec")
    (description
     "This package bundles the full Common Lisp HyperSpec ready for offline
browsing. An Emacs package is provided for easy access.  Load it with:

@lisp
(require 'clhs)
(clhs-setup)
@end lisp
")
    (license (nonfree "http://quickdocs.org/clhs/"))))

(define-public emacs-eli
  ;; 10.1 HEAD has a fix for Emacs 28.1, as opposed to the latest "express" tag.
  (let ((commit "8f9a8b9eb1aa518774c54d51e4f38ba534356415"))
    (package
      (name "emacs-eli")
      (version (git-version "acl10.1express_Feb2022update2022-02-11" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/franzinc/eli/")
               (commit commit)))
         (file-name (git-file-name "emacs-eli" version))
         (sha256
          (base32
           "0w1hdkr9x3byf1l6dbcbnd4x8110wvps3527ivyj1bmdk1hyqnzb"))))
      ;; The `emacs-build-system' fails here, probably because ELI is meant to
      ;; be load with (load "fi-site-init.el") and not with `require'.  See
      ;; https://franz.com/emacs/.
      (build-system copy-build-system)
      (inputs (list allegro-cl))
      (arguments
       `(#:install-plan
         `(("." ,,(string-append "share/emacs/site-lisp/eli-" version)
            ;; Remove useless Windows or build files.
            #:exclude ("emacsdir.pl" "nsis" "Makefile" "local.mak")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((allegro-cl (assoc-ref inputs "allegro-cl")))
                 ;; FIXME: Can't get `emacs-substitute-variables' to work here, why?
                 (substitute* "fi-vars.el"
                   (("defvar fi:common-lisp-image-name \"alisp\"")
                    (string-append "defvar fi:common-lisp-image-name \""
                                   allegro-cl "/bin/alisp\""))
                   (("defvar fi:common-lisp-image-file nil")
                    (string-append "defvar fi:common-lisp-image-file \""
                                   allegro-cl "/share/allegro-cl/alisp.dxl\""))
                   (("defvar fi:common-lisp-directory nil")
                    (string-append "defvar fi:common-lisp-directory \""
                                   allegro-cl "/share/allegro-cl\"")))))))))
      (home-page "https://franz.com/emacs/")
      (synopsis "Allegro Common Lisp Emacs interface")
      (description
       "An integral part of the Allegro CL programming environment is the interface
between Emacs and Allegro CL, hereafter referred to as the Emacs-Lisp
interface.  This interface allows the editing and running of Common Lisp
programs, and contains enhancements that allow a tight coupling between Emacs
and Lisp, very similar to those which used to be available only on Lisp
machines.

To load it, call @code{(load \"fi-site-init.el\")} from Emacs.
Then you can start Allegro CL by entering @code{M-x fi:common-lisp}.")
      ;; While this may be a free license, this Emacs package is only useful
      ;; with the non-free Allegro CL.
      (license (nonfree "https://raw.githubusercontent.com/franzinc/eli/acl10.1express_Feb2022update2022-02-11/LICENSE")))))
