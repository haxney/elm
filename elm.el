;;; elm.el --- maintain a mirror of Emacs Lisp packages

(require 'cl)
(require 'elx)

(defgroup elm nil
  "Maintain a mirror of Emacs Lisp packages."
  :group 'package)

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

;;; Metadata Utilities.

(defun elm-data (package)
  "Return the data of PACKAGE."
  (let ((file (concat elm-data-repo package)))
    (when (file-regular-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(let ((string (buffer-string)))
	  (when string
	    (read string)))))))

(defun elm-save-data (package vendor version data &optional merge)
  "Save DATA for PACKAGE."
  (let ((file (concat elm-data-repo package)))
    (with-temp-file file
      (insert (elm-pp-data data)))
    (elm-git elm-data-repo "add %s" file)))

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


(provide 'elm)
;;; elm.el ends here
