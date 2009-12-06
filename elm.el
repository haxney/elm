;;; elm.el --- maintain a mirror of Emacs Lisp packages

(require 'cl)
(require 'elx)

(defgroup elm nil
  "Maintain a mirror of Emacs Lisp packages."
  :group 'package)

(defcustom elm-data-repo
  (cons (convert-standard-filename "/home/devel/emacs/mirror/meta/sexp/")
	"master")
  "The git repository (and optionally branch) containing mirror metadata."
  :group 'elm
  :type '(choice (directory :tag "Repository")
		 (cons (directory :tag "Repository")
		       (string :tag "Branch"))))

(defcustom elm-wiki-repo
  (cons (convert-standard-filename "/home/devel/emacs/mirror/wiki/")
	"wikipages")
  "The git repository (and optionally branch) containing Emacswiki pages."
  :group 'elm
  :type '(choice (directory :tag "Repository")
		 (cons (directory :tag "Repository")
		       (string :tag "Branch"))))

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

(defun elm-prepare-repo (repo)
  "Prepare the git repository REPO.

If REPO is an atom simply return REPO.  Otherwise it's cdr has to be a
commit.  In this case checkout that commit and return the car.  REPO, or
if it is a cons cell it's car, has to be the path to a git repository."
  (if (atom repo)
      repo
    (elm-git (car repo) "checkout %s" (cdr repo))
    (car repo)))

;;; Metadata Utilities.

(defun elm-read-data (name)
  "Return the metadata of the package named NAME.
The metadata is read from the file named NAME inside the git repository
`elm-data-repo' if it exists, otherwise return nil."
  (let* ((repo (elm-prepare-repo elm-data-repo))
	 (file (concat repo name)))
    (when (file-regular-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(let ((string (buffer-string)))
	  (when string
	    (read string)))))))

(defun elm-save-data (name data)
  "Save the metadata DATA for the package named NAME.
The metadata is stored in a file named NAME inside the git repository
`elm-data-repo'."
  (let* ((repo (elm-prepare-repo elm-data-repo))
	 (file (concat repo name)))
    (with-temp-file file
      (insert (elm-pp-data data)))
    (elm-git repo "add %s" file)))

(defun elm-pp-data (data)
  "Return a string containing the pretty-printed representation of DATA."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (princ "(")
      (while data
	(let ((key (pop data))
	      (val (pop data)))
	  (when val
	    (unless (looking-back "(")
	      (princ "\n "))
	    (princ (format "%-11s " key))
	    (if (eq key :commentary)
		(prin1 val)
	      (let ((lines (split-string (pp-to-string val) "\n" t)))
		(princ (pop lines))
		(while (car lines)
		  (princ "\n")
		  (indent-to 13)
		  (princ (pop lines))))))))
      (princ ")\n"))
    (buffer-string)))

;;; Extracting.

(defun elm-extract-data (repo commit name)
  "Extract the metadata of the package named NAME from COMMIT in REPO.

For `:homepage' and `:wikipage' if that information can not be extracted
the stored value is used, if any.  Likewise if the \"mainfile\" from which
most information is extracted can't be determined use the value of option
\"elm.mainfile\" stored in the git config of the repository, if any.
If them mainfile can be determined and also isn't stored an error is
raised."
  (let ((mainfile (elm-mainfile repo commit name))
	(old-data (elm-read-data name)))
    (unless mainfile
      (error "The mainfile of package %s can not be determined" name))
    ;; Kludge.  Even though we use `elm-with-file' we have to checkout the
    ;; correct branch, because we use some functions that use the files
    ;; from the livefs (`elx-provided' and `elx-required-packages').
    ;; Instead we should implement alternative functions that work on a
    ;; git tree instead of the livefs (like I used too).
    (elm-git repo "checkout %s" commit)
    (let* ((provided (elx-provided repo))
	   (required (elx-required-packages repo provided)))
      (elm-with-file repo commit mainfile
	(list :summary (elx-summary nil t)
	      :created (elx-created mainfile)
	      :updated (elx-updated mainfile)
	      :license (elx-license)
	      :authors (elx-authors)
	      :maintainer (elx-maintainer)
	      :adapted-by (elx-adapted-by)
	      :provided provided
	      :required required
	      :keywords (elx-keywords mainfile)
	      :homepage (or (elx-homepage mainfile)
			    (plist-get old-data :homepage))
	      :wikipage (or (elx-wikipage
			     mainfile (elm-prepare-repo elm-wiki-repo) t)
			    (plist-get old-data :wikipage)))))))

(defun elm-lisp-files (repo commit)
  "Return a list of all Emacs Lisp files in COMMIT of REPO."
  (mapcan (lambda (file)
	    (when (string-match "\\.el$" file)
	      (list file)))
	  (elm-git repo "ls-tree -r --name-only %s" commit)))

(defun elm-mainfile (repo commit name)
  "Return the mainfile of the package named NAME stored in COMMIT of REPO.

The returned path is relative to the root of the repository.

If the package has only one file ending in \".el\" return that
unconditionally.  Otherwise return the file which provides the feature
matching NAME, or if no such file exists the file that provides the
feature matching NAME with \"-mode\" added to or removed from the end,
whatever makes sense.

If no file providing a matching feature can be found return the value of
option \"elm.mainfile\" stored in the git config of the repository, or if
that is undefined nil."
  (let ((files (elm-lisp-files repo commit)))
    (if (= 1 (length files))
	(car files)
      (flet ((match (feature)
		    (car (member* (format "^\\([^/]+/\\)*?%s\\.el$" feature)
				  files :test 'string-match))))
	(cond ((match name))
	      ((match (if (string-match "-mode$" name)
			  (substring name 0 -5)
			(concat name "-mode"))))
	      (t (let ((file (elm-git repo 1 "config --get elm.mainfile")))
		   (unless (equal file "") file))))))))

(provide 'elm)
;;; elm.el ends here
