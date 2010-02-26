;;; elm.el --- utilities for maintaining the Emacsmirror

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Updated: 20100226
;; Version: 0.1+
;; Homepage: https://github.com/tarsius/elm
;; Keywords: libraries

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is used to maintain the Emacsmirror which can be found at
;; http://www.emacsmirror.org.

;; This library is mostly used to extracts metadata from packages and save
;; it for future use by e.g. a package manager.

;; Most of the code used to generate the mirrors webpage can be found in
;; the accompanying libary `elm-org.el' which might eventually be replaced
;; by a more dynamic webpage.

;;; Code:

(require 'cl)
(require 'elx)
(require 'assoc)
(require 'finder)

(defgroup elm nil
  "Utilities for maintaining the Emacsmirror."
  :group 'package)

(defcustom elm-base-directory
  (convert-standard-filename "/home/devel/emacs/mirror/")
  "The directory where work on the Emacsmirror is done."
  :group 'elm
  :type 'directory)

(require 'elm-org)

;;; Extracted Information.

(defcustom elm-internal-features nil
  "Alist of all features provided by mirrored packages.

The car of each element is a feature symbols and the cdr a string
representing the providing package.  Currently the package is always
\"emacs\" but in the future this will be the actual package (which is
distributed with Emacs) that provides the feature.

The value of this variable is overwritten when running the functions
`elm-update-features' or `elm-update-packages-list'"
  :group 'elm
  :type '(repeat (cons (symbol :tag "Feature")
		       (string :tag "Package"))))

(defcustom elm-external-features nil
  "Alist of all features provided by mirrored packages.

The car of each element is a feature symbols and the cdr a string
representing the providing package.

The value of this variable is overwritten when running the functions
`elm-update-features' or `elm-update-packages-list'."
  :group 'elm
  :type '(repeat (cons (symbol :tag "Feature")
		       (string :tag "Package"))))

(defcustom elm-known-keywords nil
  "Alist of all known keywords.

The car of each element is a keyword string and the cdr is a list of
packages categorized under that keyword.

The value of this variable is overwritten when running the function
`elm-update-keywords-list'."
  :group 'elm
  :type '(repeat (cons (string :tag "Keyword")
		       (repeat :tag "Packages"
			       (string :tag "Package")))))

;; TODO the information stored in this variable is nolonger being used.
;; The customized value is still being kept so that those fixes do not
;; get lost and can later be applied to the epkg branch used for manual
;; fixes.
(defcustom elm-non-names nil
  "Known strings extracted as people names, that are not actually names."
  :group 'elm
  :type '(repeat string))

;; TODO replace this with elm-keyword-remap which should be able to
;; eigher drop or replace a keyword.
(defcustom elm-non-keywords nil
  "Known strings extracted as keywords, that are not actually keywords."
  :group 'elm
  :type '(repeat string))

(defcustom elm-missing-license nil
  "List of packages known not to contain any license information."
  :group 'elm
  :type '(repeat string))

;;; Input Locations.

(defcustom elm-emacs-directory
  (convert-standard-filename "/usr/share/emacs/lisp/")
  "The directory containing the Lisp files of Emacs."
  :group 'elm
  :type 'directory)

(defcustom elm-packages-directory
  (convert-standard-filename "/home/devel/emacs/mirror/pkgs/")
  "The directory containing the repositories of mirrored packages."
  :group 'elm
  :type 'directory)

;;; Output Locations.

;; TODO move these out of the page repository

(defcustom elm-epkg-repo
  (concat elm-page-repo (convert-standard-filename "meta/epkg/"))
  "The repository containing epkg sexps."
  :group 'elm
  :type '(directory :tag "Repository"))

(defcustom elm-package-commentary-repo
  (concat elm-page-repo (convert-standard-filename
			 "meta/package-commentaries/"))
  "The repository containing package commentary files."
  :group 'elm
  :type '(directory :tag "Repository"))

(defcustom elm-keyword-commentary-repo
  (concat elm-page-repo (convert-standard-filename
			 "meta/keyword-commentaries/"))
  "The repository containing keyword commentary files."
  :group 'elm
  :type '(directory :tag "Repository"))

;;; Git Utilities.

(defun elm-git-1 (&rest args)
  "Execute git command in `default-directory'.

Return git's exit status.  It is an error if git's exit status is greater
than OKSTATUS or if that is not specified 0.  OKSTATUS has to be an
integer or be omitted.

CMDARGS is applied to CMDFORMAT using `format' to get the command line.

Output goes to the current buffer and is logged to buffer `*elm-git-log*'
if LOG is t.  LOG has to be t or omitted.

\(fn [OKSTATUS] [LOG] CMDFORMAT CMDARGS)"
  (let* ((okstatus (if (integerp (car args))
		       (pop args)
		     0))
	 (log (when (eq (car args) t)
		(pop args)))
	 (cmdline (concat "git " (apply #'format args)))
	 (exit (call-process shell-file-name nil t nil
			     shell-command-switch
			     cmdline)))
    (when log
      (let ((output (buffer-string)))
	(with-current-buffer (get-buffer-create "*elm-git-log*")
	  (insert (format "\n$ %s\n" cmdline))
	  (insert output))))
    (if (<= exit okstatus)
	exit
      (pop-to-buffer (current-buffer))
      (error "Failed (%s): %s" exit cmdline))))

(defun elm-git (&rest args)
  "Execute a git command inside the repository REPO.

Return git's output as a list of lines.  If OKSTATUS is specified it is
consed onto the return value.  Also see `elm-git-1' which is used by this
function.

\(fn REPO [OKSTATUS] [LOG] CMDFORMAT CMDARGS)"
  (with-temp-buffer
    (let* ((default-directory (pop args))
	   (ret (apply 'elm-git-1 args))
	   (output (split-string (buffer-string) "\n" t)))
      (if (integerp (car args))
	  (cons ret output)
	output))))

(defmacro elm-with-file (repo commit file &rest body)
  "Execute the forms in BODY with a FILE temporarly current.

REPO is the path to a git repository and COMMIT has to be an existing
commit in that repository.  FILE is the path to a file relative to the
repositories root which has to exist in COMMIT.

`buffer-file-name' is set to the basename of FILE while the forms in BODY
are evaluated.  The value returned is the value of the last form in BODY."
  (declare (indent 3))
  (let ((filesym (gensym "file"))
	(commsym (gensym "comm")))
    `(let ((default-directory ,repo)
	   (,filesym ,file)
	   (,commsym ,commit))
       (with-temp-buffer
	 (elm-git-1 "show %s:%s" ,commsym ,filesym)
	 (let ((buffer-file-name (file-name-nondirectory ,filesym)))
	   (with-syntax-table emacs-lisp-mode-syntax-table
	     ,@body))))))

;;; Package Utilities.

(defmacro elm-map-packages (function)
  "Apply FUNCTION to each package stored in `elm-packages-directory'.
FUNCTION is applied to the name of each package in alphabetic order while
ignoring case."
  (declare (indent 0))
  `(mapc ,function
	 (mapcan
	  (lambda (file)
	    (when (file-directory-p file)
	      (list (file-name-nondirectory file))))
	  (sort* (directory-files elm-packages-directory t "^[^.]" t)
		 (lambda (a b)
		   (string< (upcase (file-name-nondirectory a))
			    (upcase (file-name-nondirectory b))))))))

(defun elm-package-repo (name)
  "Return the path of the repository of the package named NAME.
This is a subdirectory of directory `elm-packages-directory'."
  (file-name-as-directory (concat elm-packages-directory name)))

(defun elm-package-mainfile (name &optional full)
  "Return the mainfile of the package named NAME.
If optional FULL is non-nil return the absolute path otherwise relative to
the root of package's repository."
  (let* ((repo (elm-package-repo name))
	 (main (cadr (elm-git repo 1 "config --get elm.mainfile"))))
    (if main
	(if full
	    (concat repo main)
	  main)
      (elx-package-mainfile repo full))))

(defun elm-package-epkg (name)
  "Return the file containing the epkg of the package named NAME."
  (concat elm-epkg-repo name ".epkg"))

(defun elm-package-page (name)
  "Return the file containing the page of the package named NAME."
  (concat elm-epkg-page-repo name ".org"))

(defun elm-package-commentary (name)
  "Return the file containing the commentary of the package named NAME."
  (concat elm-package-commentary-repo name ".txt"))

(defun elm-feature-commentary (feature)
  "Return the file containing the commentary of FEATURE."
  (concat elm-package-commentary-repo (symbol-name feature) ".txt"))

;;; Creating and Reading Epkgs and Commentary Files.

(defun elm-save-data (name &optional homepage)
  "Save the metadata (epkg and commentary) of the package named NAME.
If optional HOMEPAGE is non-nil and the homepage can not be determined
use HOMEPAGE, otherwise use the extracted value or nil if non can be
extracted."
  (let* ((repo (elm-package-repo name))
	 (main (elm-package-mainfile name t))
	 (data (elx-package-metadata repo main))
	 (prev (elm-read-epkg name)))
    (unless (plist-get data :license)
      (plist-put data :license (plist-get prev :license)))
    (unless (plist-get data :homepage)
      (plist-put data :homepage (or homepage (plist-get prev :homepage))))
    (unless (plist-get data :wikipage)
      (plist-put data :wikipage (plist-get prev :wikipage)))
    (elm-save-epkg name (butlast data 2))
    (elm-save-commentary name (car (last data)))))

(defun elm-save-commentary (name commentary)
  "Save the commentary COMMENTARY of the package named NAME."
  (when commentary
    (with-temp-file (elm-package-commentary name)
      (insert commentary))))

(defun elm-save-epkg (name data)
  "Save the metadata DATA of the package named PACKAGE."
  ;; TODO checkout the branch containing automatically extracted value
  ;; (as opposed to the branch containing manual fixes and additions).
  (with-temp-file (elm-package-epkg name)
    (insert (elx-pp-metadata data))))

(defun elm-read-epkg (name &optional full)
  "Return the epkg data of the package named NAME.
If optional FULL is non-nil include the commentary otherwise don't."
  (let ((epkg (elm-package-epkg name))
	(comm (elm-package-commentary name))
	str data)
    (when (file-regular-p epkg)
      (with-temp-buffer
	(insert-file-contents epkg)
	(setq str (buffer-string)))
      (when str
	(setq data (read str)))
      (when (and full
		 (not (plist-get data :commentary))
		 (file-regular-p comm))
	(with-temp-buffer
	  (insert-file-contents comm)
	  (setq str (buffer-string)))
	(when str
	  (plist-put data :commentary str))))
    data))

;; Updating all Metadata.

(defun elm-update-metadata ()
  "Update the metadata of all mirrored packages.
Also update the value of some runtime variables used for this task and
generate org pages about pages for later export to html."
  (interactive)
  (elm-update-features-lists)
  (elm-update-keywords-list)
  (elm-update-packages-data)
  ;; This does not create the webpages (html) but the files (org) from
  ;; which these are created.  So it is okay (for now) to do this here.
  (elm-update-packages-index)
  (elm-update-keywords-index)
  (elm-update-packages-pages)
  (elm-update-features-pages))

(defun elm-update-features-lists ()
  "Update the value of `elm-internal-features' and `elm-external-features'."
  (interactive)
  (setq elx-external-features nil)
  (elm-map-packages
    (lambda (name)
      (message "Updating features of package '%s'..." name)
      (dolist (feature (elx-provided (elm-package-repo name)))
	(aput 'elx-external-features feature name)
	(message "Updating features of package '%s'...done" name))))
  (setq elx-internal-features nil)
  (message "Updating features of Emacs...")
  (dolist (feature (elx-provided elm-emacs-directory))
    (aput 'elx-internal-features feature "emacs"))
  (message "Updating features of Emacs...done"))

(defun elm-update-keywords-list ()
  "Update the value of `elm-known-keywords'."
  (interactive)
  (let ((keywords (mapcan (lambda (elt)
			    (list (list (symbol-name (car elt)))))
			  finder-known-keywords)))
    (elm-map-packages
      (lambda (name)
	(message "Updating keywords of package '%s'..." name)
	(dolist (keyword (elx-keywords (elm-package-mainfile name t)))
	  (unless (member keyword elm-non-keywords)
	    (aput 'keywords keyword
		  (sort (cons name (cdr (assoc keyword keywords))) 'string<))))
	(message "Updating keywords of package '%s'...done" name)))
    (setq elm-known-keywords (sort* keywords 'string< :key 'car))))

(defun elm-update-packages-data ()
  "Update the metadata of all mirrored packages."
  (interactive)
  (elm-map-packages
    (lambda (name)
      (message "Updating metadata of package '%s'..." name)
      (elm-save-data name)
      (message "Updating metadata of package '%s'...done" name))))

(provide 'elm)
;;; elm.el ends here
