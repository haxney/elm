;;; elm.el --- maintain a mirror of Emacs Lisp packages

(require 'cl)
(require 'elx)
(require 'epkg)
(require 'vcomp)

(defgroup elm nil
  "Maintain a mirror of Emacs Lisp packages."
  :group 'package)

(defcustom elm-id "elm"
  "Identifies the mirror being maintained.
Since `epkg.el' is currently unable to handle multiple mirrors it does not
make much sense to create a mirror of your own.  If you are thinking about
creating your own mirror you should first get in contact with the
maintainer of the Emacs Lisp Mirror."
  :group 'elm
  :type 'string)

(defvar elm-emacs-version nil
  "While we wait for 23.1 this has to be set manually for each rc.")

(defvar elm-missing-features nil
  "List of missing features.")

;;; Utilities.

(defmacro elm-hash (name vendor &optional version)
  (if version
      `(let ((object (cadr (epkg-git-live 128 "cat-file tag elm/%s/%s/%s"
				       ,name ,vendor ,version))))
	 (when object
	   (substring object 7)))
    `(cadr (epkg-git-live 128 "rev-parse remotes/%s/%s/%s"
			  elm-id ,name ,vendor))))

;;; Synchronizing.

(defun elm-synchronize ()
  ;; (dolist (remote (epkg-git-live "remote"))
  ;;   (unless (string-match "/" remote)
  ;;     (message "Pulling  %s..." remote)
  ;;     (epkg-git-live 128 "fetch %s" remote)))
  (dolist (branch (epkg-git-live "branch -r"))
    (when (string-match (format "^. %s/\\([^/]+\\)/\\([^/]+\\)$" elm-id) branch)
      (let ((name (match-string 1 branch))
	    (vendor (match-string 2 branch)))
	(when (elm-pull-vendor name vendor)
	  (if (assoc vendor (assoc name epkg-available-packages))
	      (elm-bump-vendor name vendor)
	    (elm-init-vendor name vendor)))))))

;;; Generating.

(defun elm-init-vendor (name vendor &optional repo branch &rest data)
  (message "Creating %s/%s..." name vendor)
  (when data
    (epkg-save-data name vendor nil data t))
  (when repo
    (epkg-git-live "remote add %s/%s %s" name vendor repo)
    (epkg-git-live "config remote.%s/%s.fetch +refs/heads/%s:refs/remotes/%s/%s/%s"
		   name vendor branch elm-id name vendor)
    (epkg-git-live "config --add remote.%s/%s.fetch +refs/tags/*:refs/tags/import/%s/%s/*"
		   name vendor name vendor)
    (epkg-git-live "config remote.%s/%s.tagopt --no-tags" name vendor))
  (if (elm-pull-vendor name vendor)
      (elm-bump-vendor name vendor)
    (error "Initial pull failed for %s/%s" name vendor))
  (message "Creating %s/%s...done" name vendor))

(defun elm-bump-vendor (name vendor)
  (message "Updating %s/%s..." name vendor)
  (let* ((versions (elm-new-versions name vendor))
	 (tagged (car versions)))
    (setq versions (cdr versions))
    (when (and (not (equal (car (cadr (last versions))) "HEAD"))
	       (equal (caar (plist-get (epkg-data name vendor) :versions)) "HEAD"))
      (epkg-git-live "tag -d %s/%s/%s/HEAD" elm-id name vendor)
      (epkg-git-live "rm %s/%s/HEAD" name vendor))
    (if (not versions)
	(message "Updating %s/%s...failed" name vendor)
      (dolist (version versions)
	(elm-make-version name vendor (car version) (cdr version))
	(epkg-add-version name vendor (car version) tagged))
      (epkg-save-data name vendor nil
		      (elm-extract-vendor-data name vendor (cdar versions)) t)
      (with-temp-file (concat epkg-data-repo "index")
	(insert (pp-to-string epkg-available-packages)))
      (epkg-git-data "commit -m \"%s\" index %s"
		     (if (= (length versions) 1)
			 (if (equal (car versions) "HEAD")
			     (format "update %s/%s/HEAD" name vendor)
			   (format "add %s/%s/%s" name vendor (caar versions)))
		       (format "add %s/%s/%s..%s" name vendor (caar versions)
			       (caar (last versions))))
		     name)
      (message "Updating %s/%s...done" name vendor))))

(defun elm-pull-vendor (name vendor)
  (when (member (concat name "/" vendor) (epkg-git-live "remote"))
    (message "Pulling  %s/%s..." name vendor)
    (epkg-git-live 128 "fetch %s/%s" name vendor)
    (message "Pulling  %s/%s..." name vendor))
  (not (equal (elm-hash name vendor)
	      (elm-hash name vendor
			(caddr (assoc vendor
				      (assoc name
					     epkg-available-packages)))))))

(defun elm-make-version (name vendor version object)
  (message "Creating %s/%s/%s..." name vendor version)
  (let ((data (elm-extract-version-data name vendor version object)))
    (dolist (p (plist-get data :provided))
      (epkg-add-feature p name))
    (epkg-save-data name vendor version data))
  (epkg-git-live "tag -m '%s' -a /%s/%s/%s/%s %s"
		 version elm-id name vendor version object)
  (message "Creating %s/%s/%s...done" name vendor version))

;;; Extracting.

(defun elm-lisp-files (object)
  "Return list of all Emacs Lisp files in git object OBJECT."
  (mapcan (lambda (file)
	    (when (string-match "\\.el$" file)
	      (list file)))
	  (epkg-git-live "ls-tree -r --name-only %s" object)))

(defun elm-mainfile (name vendor object)
  "Return the file from git object OBJECT matching NAME.
If that fails look for a file matching NAME with \"-mode\" added to or
removed from the end, whatever makes sense.  If that fails also return
the vendor default if any."
  (let ((files (elm-lisp-files object)))
    (if (and (= 1 (length files))
	     (string-match "\\.el$" (car files)))
	(car files)
      (flet ((match (feature)
		    (car (member* (format "^\\([^/]+/\\)*?%s\\.el$" feature)
				  files :test 'string-match))))
	(cond ((match name))
	      ((match
		(if (string-match "-mode$" name)
		    (substring name 0 -5)
		  (concat name "-mode"))))
	      (t
	       (or (plist-get (epkg-data name vendor nil) :mainfile)
		   (plist-get (epkg-data name nil nil) :mainfile))))))))

(defun elm-new-versions (name vendor)
  "Return list of new versions available from the specified vendor.
The first of the return value actually is a boolean indicating whether
the vendor uses tags.  (Which does not necessarly mean that there were
any new tags.)"
  (let* (versions
	 (head (car (epkg-git-live "rev-parse remotes/%s/%s/%s"
				   elm-id name vendor)))
	 (version-entry (assoc vendor (assoc name epkg-available-packages)))
	 (previous (caddr version-entry))
	 (tagged (cadr version-entry)))
    (if (equal vendor "emacs")
	(progn (setq tagged t)
	       (push (cons elm-emacs-version head) versions))
      (let ((commits (nreverse (epkg-git-live
				"log --pretty=format:%%H%%d %s"
				(concat (when previous
					  (format "remotes/%s/%s/%s/%s.."
						  elm-id name vendor previous))
					head)))))
	;; Get versions from tags.
	(dolist (commit commits)
	  (when (string-match "^\\([^ ]+\\) (\\(.+\\))$" commit)
	    (let ((hash (match-string 1 commit))
		  (revs (split-string (or (match-string 2 commit) "") ", " t)))
	      (while revs
		(let ((rev (pop revs)))
		  (when (string-match (format "^\
\\(?:tag: \\)?refs/tags/import/%s/%s/\
\\(?:\\(?:\\|%s\\|v\\(?:ersion\\)?\\|r\\(?:elease\\)?\\)-?\\)?\
\\([0-9]+\\(\\[-_.][0-9]+\\)*[a-z]?\
\\(_\\(alpha\\|beta\\|pre\\|rc\\|p\\)[0-9]*\\)?\
\\(-r\\([0-9]+\\)\\)?\\)$" name vendor name) rev)
		    (push (cons (match-string 1 rev) hash) versions)
		    (setq tagged t revs nil)))))))
	(unless tagged
	  (message "Vendor doesn't tag releases")
	  ;; Get versions from source.
	  (dolist (commit commits)
	    (string-match "^\\([^ ]+\\)" commit)
	    (let* ((hash (match-string 1 commit))
		   (file (elm-mainfile name vendor hash)))
	      (if (not file)
		  (message "Can't extract mainfile")
		(epkg-with-file hash file
		  (let ((version (elx-version file)))
		    (when version
		      (unless (equal version previous)
			(when (and previous (vcomp< version previous))
			  (message "Inconsistant version sequence"))
			;; Kludge.
			;; TODO improve error handling
			(if (member* version versions :key 'car :test 'equal)
			    (message "Inconsistant version sequence")
			  (push (cons version hash) versions))
			(setq previous version))))))))
	  ;; Grrrrr.
	  (unless versions
	    (message "Vendor doesn't believe in releases")))))
    (setq versions (nreverse versions))
    (cons tagged
	  ;; Handle head.
	  (if (equal (cdr (car (last versions)))
		     (car (epkg-git-live "rev-parse %s" head)))
	      versions
	    (when (elm-mainfile name vendor head)
	      (nconc versions (list (cons "HEAD" head))))))))

(defun elm-extract-vendor-data (name vendor object)
  "Extract data specific to the specified version."
  (let* ((mainfile (elm-mainfile name vendor object))
	 (default-directory epkg-live-repo))
    (epkg-with-file object mainfile
      (list :created    (elx-created mainfile)
	    :keywords   (elx-keywords mainfile)
	    :authors    (elx-authors)
	    :maintainer (elx-maintainer)
	    :adapted-by (elx-adapted-by)
	    :homepage   (elm-extract-homepage name vendor)
	    :wikipage   (elm-extract-wikipage name)))))

(defun elm-extract-version-data (name vendor version object)
  "Extract data specific to the specified version."
  (let ((mainfile (elm-mainfile name vendor object))
	(default-directory epkg-live-repo))
    (epkg-with-file object mainfile
      (let ((provided (elm-extract-provided name vendor version object)))
	(list :summary (elx-summary nil t)
	      :updated (elx-updated mainfile)
	      :license (elx-license)
	      :provided provided
	      :required (elm-extract-required name vendor version object provided)
	      :commentary (elm-extract-commentary name vendor version))))))

(defun elm-extract-homepage (name vendor)
  "Extract the homepage of the specified vendor.
This function expects the current buffer to contain the mainfile"
  (let ((repo (cadr (epkg-git-live 1 "config remote.%s/%s.repo" name vendor))))
    (cond ((lm-header "homepage"))
	  ((not repo) nil)
	  ((equal vendor "emacs")
	   "http://www.gnu.org/software/emacs/")
	  ((string-match "^git://github.com/\\(.*\\)$" repo)
	   (concat "http://github.com/" (match-string 1 repo)))
	  ((string-match "^git://repo.or.cz/\\(.*\\)\.git$" repo)
	   (concat "http://repo.or.cz/w/" (match-string 1 repo)))
	  ((string-match "^git://gitorious.org/\\(.*\\)/mainline.git$" repo)
	   (concat "http://gitorious.org/projects/" (match-string 1 repo))))))

(defun elm-extract-wikipage (name)
  "Extract the page on the Emacswiki for the specified package."
  (let ((page (upcase-initials
	       (replace-regexp-in-string "\\+$" "Plus"
		(replace-regexp-in-string "-."
		 (lambda (str)
		   (upcase (substring str 1)))
		 (file-name-sans-extension
		  (file-name-nondirectory name)))))))
    (when (member page (epkg-git-live "ls-tree --name-only ewiki/pages"))
      (concat "http://www.emacswiki.org/emacs/" page))))

(defun elm-extract-commentary (name vendor version)
  "Extract the commentary section of the specified version.
This function expects the current buffer to contain the version's mainfile"
  ;; TODO maybe replace "^(" with "\("?
  (let ((commentary (elx-commentary)))
     (if (or (null commentary)
	     (equal (substring commentary 0 1) "\n"))
	 commentary
       (concat "\n" commentary))))

(defun elm-extract-provided (name vendor version object)
  "Extract the features provided by the specified version."
  (let (provided)
    (dolist (file (elm-lisp-files object))
      (epkg-with-file object file
	(let ((prov (elx-buffer-provided (current-buffer))))
	  (dolist (p prov)
	    (add-to-list 'provided p)))))
    (dolist (p (nconc (plist-get (epkg-data name)        '+provided)
		      (plist-get (epkg-data name vendor) '+provided)))
      (add-to-list 'provided p))
    (sort provided #'string<)))

(defun elm-extract-required (name vendor version object provided)
  "Extract the packages and features required by the specified version.
The returned value is nil or has the form:
\(((HARD-REQUIRED-PACKAGE FEATURE...)...)
 [((SOFT-REQUIRED-PACKAGE FEATURE...)...)])"
  (when (eq provided t)
    (setq provided (elm-extract-provided name vendor version object)))
  (let (hard soft package-add package-del vendor-add vendor-del)
    (let ((package-data (epkg-data name))
	  (vendor-data  (epkg-data name vendor)))
      (setq package-add (plist-get package-data '+required)
	    package-del (plist-get package-data '-required)
	    vendor-add  (plist-get vendor-data  '+required)
	    vendor-del  (plist-get vendor-data  '-required)))
    (dolist (r (nconc (nth 0 package-add) (nth 0 vendor-add)))
      (add-to-list 'hard r))
    (dolist (r (nconc (nth 1 package-add) (nth 1 vendor-add)))
      (add-to-list 'soft r))
    (dolist (file (elm-lisp-files object))
      (epkg-with-file object file
	(let ((requ (elx-buffer-required (current-buffer))))
	  (dolist (r (nth 0 requ))
	    (unless (memq r provided)
	      (add-to-list 'hard r)))
	  (dolist (r (nth 1 requ))
	    (unless (memq r provided)
	      (add-to-list 'soft r))))))
    (setq hard (elm-prepare-required hard (nconc (nth 0 package-del)
						 (nth 0 vendor-del)))
	  soft (elm-prepare-required soft (nconc (nth 1 package-del)
						 (nth 1 vendor-del))))
    (if soft
	(list hard soft)
      (when hard
	(list hard)))))

(defun elm-prepare-required (required remove)
  (let (clean)
    (dolist (requ required)
      (unless (memq requ remove)
	(let* ((package (gethash requ epkg-available-features))
	       (elt (car (member* package clean :test 'equal :key 'car))))
	  (unless package
	    (add-to-list 'elm-missing-features requ))
	  (if elt
	      (unless (memq requ (cdr elt))
		(setcdr elt (sort (cons requ (cdr elt)) 'string<)))
	    (push (list package requ) clean)))))
    (sort* clean
	   (lambda (a b)
	     (cond ((null a) nil)
		   ((null b) t)
		   (t (string< a b))))
	   :key 'car)))

;;; Updating from Defaults.

(defun elm-update-data (name vendor &optional version)
  "Update all metadata specific to the specified vendor or version.
This is used to merge changes made to the package and/or vendor defaults."
  ) ; TODO

(defun elm-update-dependencies (name vendor version &optional force)
  "Try to resolve any unresolved dependencies of the specified version.
If FORCE is non-nil update dependencies even if all have been previously
resolved."
  (let* ((hash (elm-hash name vendor version))
	 (data (epkg-data name vendor version))
	 (required (plist-get data :required))
	 (msg (format "Update dependencies of %s/%s/%s..."
		      name vendor version)))
    (when (or force
	      (member* nil (nth 0 required) :key 'car)
	      (member* nil (nth 1 required) :key 'car))
      (message msg)
      (setq required (epkg-with-file hash (elm-mainfile name vendor hash)
		       (elm-extract-required name vendor version hash
					     (plist-get data :provided))))
      (epkg-save-data name vendor version (list :required required) t)
      (message (concat msg "done")))))

(defun elm-update-wikipage (name vendor)
  (unless (plist-get (epkg-data name vendor) :wikipage)
    (let ((page (elm-extract-wikipage name))
	  (msg (format "Update wikipage of %s/%s..." name vendor)))
      (when page
	(message msg)
	(epkg-save-data name vendor nil (list :wikipage page) t)
	(message (concat msg "done"))))))

;;; Check Consistancy.

(defun elm-missing-dependencies (name vendor version)
  (let* ((ref (concat elm-id "/" name "/" vendor
		      (unless (equal version "HEAD")
			(concat "/" version))))
	 (data (epkg-data name vendor version))
	 (files (elm-lisp-files ref))
	 (provided (plist-get data :provided))
	 (required (plist-get data :required))
	 (hard (car (member* nil (nth 0 required) :key 'car)))
	 (soft (car (member* nil (nth 1 required) :key 'car))))
    (when (or hard soft)
      (message "%s/%s/%s\n  hard: %s\n  soft: %s\n  ref:  %s"
	       name vendor version hard soft
	       (car (epkg-git-live "rev-parse %s" ref))))))

(provide 'elm)
;;; elm.el ends here