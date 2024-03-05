((nil
  .
  ((eval . (setq-local
            org-roam-directory
	    (expand-file-name
	     "./notes/"
	     (locate-dominating-file
              default-directory ".dir-locals.el"))))
   (eval .
	 (setq-local org-roam-db-location
		     (expand-file-name
		      "org-roam.db"
		      org-roam-directory))))))
