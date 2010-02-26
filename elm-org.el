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

;; This library is used to generate the Emacsmiror's webpage at
;; http://www.emacsmirror.org.  However this statically generated and
;; usually not up-to-date page might eventually be replaced by a more
;; dynamic webpage.

;; Also see `elm.el'.

;;; Code:

(require 'cl)

(defgroup elm-org nil
  "Utilities for creating package pages."
  :group 'elm)

(defcustom elm-page-base
  (concat elm-base-directory (convert-standard-filename "page/"))
  "The directory containing the webpage's source and generated files."
  :group 'elm-org
  :type 'directory)

;; Repositories.

(defcustom elm-page-repo
  (concat elm-page-base (convert-standard-filename "src/"))
  "The directory containing page source files."
  :group 'elm-org
  :type 'directory)

(defcustom elm-epkg-page-repo
  (concat elm-page-repo (convert-standard-filename "package/"))
  "The repository containing epkg pages."
  :group 'elm-org
  :type '(directory :tag "Repository"))

(defcustom elm-keyword-page-repo
  (concat elm-page-repo (convert-standard-filename "keyword/"))
  "The repository containing keyword pages."
  :group 'elm-org
  :type '(directory :tag "Repository"))

;;; Destinations of exports.

(defcustom elm-page-dest
  (concat elm-page-base (convert-standard-filename "dst/"))
  "The directory containing published page files."
  :group 'elm-org
  :type 'directory)

(defcustom elm-epkg-page-dest
  (concat elm-page-dest (convert-standard-filename "package/"))
  "The directory containing published epkg pages."
  :group 'elm-org
  :type 'directory)

;; TODO stop this nonsense
(defcustom elm-epkg-dest
  (concat elm-page-dest (convert-standard-filename "epkg/"))
  "The directory containing published epkg sexps."
  :group 'elm-org
  :type 'directory)

;; TODO don't even start with this nonsense
(defcustom elm-package-commentary-dest ; not currently used
  (concat elm-page-dest (convert-standard-filename "commentary/"))
  "The directory containing published package commentary files."
  :group 'elm-org
  :type 'directory)

(defcustom elm-keyword-page-dest
  (concat elm-page-dest (convert-standard-filename "keyword/"))
  "The directory containing published keyword pages."
  :group 'elm-org
  :type 'directory)

(defcustom elm-keyword-commentary-dest ; not currently used
  (concat elm-page-dest (convert-standard-filename "commentary/"))
  "The directory containing published keyword commentary files."
  :group 'elm
  :type 'directory)


;; Output Links.

(defcustom elm-epkg-link
  "[[../epkg/%s.epkg][%s]]"
  "Format string used to create org link to an epkg."
  :group 'elm-org
  :type 'string)

(defcustom elm-repo-link
  "[[http://github.com/emacsmirror/%s][%s]]"
  "Format string used to create org link to a repository."
  :group 'elm-org
  :type 'string)

(defcustom elm-repo-url
  "http://github.com/emacsmirror/%s.git"
  "Format string used to create git url for a repository."
  :group 'elm-org
  :type 'string)





(defcustom elm-mirrored-packages-file
  "/home/devel/emacs/mirror/page/src/meta/list/mirrored.org"
  ""
  :group 'elm-org
  :type 'file)

(defcustom elm-keywords-file
  "/home/devel/emacs/mirror/page/src/meta/list/keywords.org"
  ""
  :group 'elm-org
  :type 'file)


;; Utilities.

(defun elm--format-list-elements (&optional insert-fn key-fn prefix elements)
  "Return a string suitable to be inserted into a page containing ELEMENTS."
  ;; TODO complete doc.
  (interactive)
  (let* (letter
	 (fn (lambda (elt)
	       (let ((ltr (upcase
			   (substring
			    (funcall (or key-fn 'identity) elt) 0 1))))
		 (when (string-match "[^A-Z]" ltr)
		   (setq ltr "#"))
		 (unless (equal ltr letter)
		   (setq letter ltr)
		   (elm--insert-alphabetic-header letter))
		 (funcall (or insert-fn 'insert) elt prefix)))))
    (with-temp-buffer
      (if (eq elements :packages)
	  (elm-map-packages fn)
	(mapc fn elements))
      (buffer-string))))

(defun elm--insert-alphabetic-header (letter)
  "Insert a header using LETTER."
  (insert (format "\n* %s\n\n" letter)))

(defun elm--insert-package-link (name prefix)
  "Insert a link to the page of the package named NAME."
  (let ((summary (plist-get (elm-read-epkg name) :summary)))
    (insert (format "+ [[%spackage/%s.org][%s]]%s\n"
		    prefix name name
		    (if summary (concat " --- " summary) "")))))

(defun elm--insert-keyword-link (arg prefix)
  "Insert a link to the keyword page specified by ARG."
  ;; TODO fix ARG argument?
  (let* ((keyword (car arg))
	 (summary (cdr (assoc (intern keyword) finder-known-keywords))))
    (insert (format "+ [[%skeyword/%s.org][%s]]%s\n"
		    prefix keyword keyword
		    (if summary (concat " --- " summary) "")))))

(defun elm--insert-resource-link (str url)
  ;; TODO doc.
  (insert (format "+ %s: " str))
  (insert (if url
	      (if (string-match "^404:\\(.+\\)" url)
		  (format "[[%s]] (dead link)"
			  (match-string 1 url))
		(format "[[%s]]" url))
	    "unknown"))
  (insert "\n"))

;;; Updating Webpages.

(defun elm-org-publish (project)
  (org-publish-project (assoc project org-publish-project-alist)))

(defun elm-update-all-pages ()
  (interactive)
  (elm-org-publish "elm-packages-pages")
  (elm-org-publish "elm-packages-lists")
  (elm-org-publish "elm-keywords-pages")
  (elm-org-publish "elm-keywords-lists")
  (elm-org-publish "elm-keywords-index")
  (elm-org-publish "elm-epkgs"))

(defun elm-update-packages-pages ()
  (interactive)
  (elm-map-packages 'elm-create-package-page))

(defun elm-update-features-pages ()
  (interactive)
  (mapc 'elm-create-keyword-page elm-known-keywords))

;;; Updating Index Pages.

(defun elm-update-packages-index ()
  (interactive)
  (with-temp-file elm-mirrored-packages-file
    (insert (elm--format-list-elements 'elm--insert-package-link
				       nil "./" :packages))))

(defun elm-update-keywords-index ()
  (interactive)
  (with-temp-file elm-keywords-file
    (insert (elm--format-list-elements 'elm--insert-keyword-link
				       'car "./"
				       elm-known-keywords))))

;;; Creating Pages.

(defun elm-create-page (file sections &rest args)
  "Create a page containing SECTIONS using ARGS.
The page is saved to FILE.  SECTIONS has the form ((FUNCTION . TITLE)...).
Each TITLE if non-nil is inserted into the page and each FUNCTION is
applied to ARGS in order while output goes to the page too."
  (with-temp-file file
    (while sections
      (let ((elt (pop sections)))
	(when (cdr elt)
	  (insert (format "* %s\n\n" (cdr elt))))
	(apply (car elt) args)
	(unless (looking-back "\n\n")
	  (insert "\n")
	  (when (and sections (not (looking-back "\n\n")))
	    (insert "\n")))))))

;;; Creating Statistics Pages.

(defun elm-update-packages-statistics ()
  (let (licenses)
    (flet ((inc-license (key)
	     (let ((ass (assoc key licenses)))
	       (aput 'licenses key (if ass (1+ (cdr ass)) 1)))))
      (elm-map-packages
       (lambda (name)
	 (let* ((data (elm-read-epkg name)))
	   (inc-license (or (plist-get data :license)
			    (if (member name elm-missing-license)
				"undefined"
			      "unknown"))))))
      (sort* licenses '> :key 'cdr)
      ;; TODO output in useful format, that is a pretty webpage
      )))

;; Creating Package Pages.

(defcustom elm-package-page-sections
  '((elm--insert-package-header       . nil)
    (elm--insert-package-commentary   . "Commentary")
    (elm--insert-package-resources    . "Resources")
    (elm--insert-package-download     . "Download")
    (elm--insert-package-dependencies . "Dependencies")
    (elm--insert-package-metadata     . "Metadata"))
  ""
  :group 'elm-org
  :type '(list (function :tag "Function")
	       (string   :tag "Title")))

(defun elm-create-package-page (name &optional data)
  "Create page for the package named NAME using metadata DATA."
  (elm-create-page (elm-package-page name)
		   elm-package-page-sections name
		   (or data (elm-read-epkg name))))

(defun elm--insert-package-header (name data)
  (let ((summary (plist-get data :summary)))
    (insert "#+SETUPFILE: ../inc/pkg-page-setup.org\n\n")
    (insert "#+TITLE: ")
    (insert name)
    (when summary
      (insert " --- ")
      (insert summary))
    (insert "\n")))

(defun elm--insert-package-commentary (name data)
  (let ((commentary (plist-get data :commentary)))
    (if (not commentary)
	(insert "No parsable commentary found in package.\n")
      (insert "#+BEGIN_EXAMPLE\n")
      (insert commentary)
      (insert "#+END_EXAMPLE\n"))))
  ;; TODO
  ;; (insert (replace-regexp-in-string
  ;; 	   (let ((r "[\s\t]+[^\s\t\n]+\n?"))
  ;; 	     (format (concat "^\\((.+\n\\)?"
  ;; 			     "\\(%s\\)"
  ;; 			     "\\("
  ;; 			     "\\([\s\t]*\n\\|%s\\)*"
  ;; 			     "\\(%s\\)+"
  ;; 			     "\\)*")
  ;; 		     r r r))
  ;; 	   (lambda (match)
  ;; 	     (concat "#+BEGIN_EXAMPLE\n"
  ;; 		     (replace-regexp-in-string "^[\s\t]+" "" match t t)
  ;; 		     "#+END_EXAMPLE\n"))
  ;; 	   (or (plist-get data :commentary)
  ;; 	       "No parsable commentary found in package.\n")
  ;; 	   t t)))


(defun elm--insert-package-resources (name data)
  (elm--insert-resource-link "Homepage" (plist-get data :homepage))
  (elm--insert-resource-link "Wikipage" (plist-get data :wikipage)))
  ;; TODO
  ;; (insert "\n")
  ;; (insert "If you know of additional resources about this package ")
  ;; (insert "(e.g. tutorials,\nblog posts, screencasts) please let me ")
  ;; (insert "[[mailto:metadata@emacsmirror.org][know]]\n")
  ;; (insert "so that they can be listed here.\n")
  
(defun elm--insert-package-download (name data)
  (insert "** From Mirror\n\n")

  (insert "The [[../index.org][Emacsmirror]] ")
  (insert "contains a git repository which mirrors upstream.\n")
  (insert "Note that upstream might use a different version control system or none\n")
  (insert "at all.  Upstream might not even distribute the library anymore.\n\n")

  (insert "You can look at the ")
  (insert (format elm-repo-link name "repository"))
  (insert "\nin a browser or you can get a local copy like this:\n\n")

  (insert "#+BEGIN_EXAMPLE\n")
  (insert (format "git clone %s\n" (format elm-repo-url name)))
  (insert "#+END_EXAMPLE\n\n")

  (let ((homepage (plist-get data :homepage)))
    (insert "** From Upstream\n\n")
    (cond ((and homepage (not (string-match "^404:" homepage)))
	   (insert "If you prefer you can also get the library directly from upstream's ")
	   (insert (format "[[%s][homepage]].\n\n" homepage)))
	  (t
	   (insert "The location of this package's upstream is unknown.  ")
	   (insert "Upstream might not\n")
	   (insert "even distribute this package anymore.  ")
	   (insert "If you do know where upstream can\n")
	   (insert "be found please let me ")
	   (insert "[[mailto:metadata@emacsmirror.org][know]].\n")))))

(defun elm--insert-package-dependencies (name data)
  (let* ((deps (plist-get data :required))
	 (hard (nth 0 deps))
	 (soft (nth 1 deps)))
    (if (not deps)
	(progn
	  (insert "This package probably does not have any external dependencies.\n")
	  (insert "However it likely depends on various libraries that are part\n")
	  (insert "of Emacs. Unfortunatly these libraries can not yet be listed.\n"))
      (insert "A package may have hard and/or soft dependencies. ")
      (insert "Hard dependencies are\n mandatory - the package's ")
      (insert "libraries do not load if they are not satisfied.\n")
      (insert "Soft\n dependencies are optional - additional ")
      (insert "functionality might be available\n")
      (insert "when they are satisfied.\n\n")
      (when (or (member* nil hard :key 'car :test 'eq)
		(member* nil soft :key 'car :test 'eq))
	(insert "This package requires some features that could ")
	(insert "not yet be mapped to a package.\n\n"))
      (when hard
	(insert "** Hard Dependencies\n\n")
	(dolist (elt hard)
	  (insert (concat "+ " (or (car elt) "unknown package") "\n")))
	(insert "\n")
	(setq soft (mapcan (lambda (elt)
			     (unless (member* (car elt) hard
					      :key 'car :test 'equal)
			       (list elt)))
			   soft)))
      (when soft
	(insert "** Soft Dependencies\n\n")
	(dolist (elt soft)
	  (insert (concat "+ " (or (car elt) "unknown package") "\n")))
	(insert "\n")))))

(defun elm--insert-package-metadata (name data)
  (insert "The epkg (metadata as pretty-printed S-expression) of this package is\n")
  (insert (concat "available " (format elm-epkg-link name "here") ".\n")))
  ;; TODO
  ;; (insert "This information has mostly been auto-generated from the file ")
  ;; (insert (format elm-mainfile-link name file
  ;; 		  (file-name-nondirectory file)))
  ;; (insert "and possibly other files that are part of this library.  This\n")
  ;; (insert "information is also available as a pretty-printed ")
  ;; (insert (format elm-epkg-link name "S-expression"))
  ;;
  ;; This information is incomplete and/or otherwise broken which likely means
  ;; that the library does not respect the [[http://][header conventions]].
  ;; At the moment we
  ;; do not fix incomplete or otherwise broken information on the mirror but
  ;; depend on upstream.  To get this information and the library itself fixed
  ;; you have to contact the maintainer of the library.  If you do please read
  ;; [[../contacting-upstream.org][this]] first.

;; Create Keyword Pages.

(defcustom elm-feature-page-sections
  '((elm--insert-keyword-header     . nil)
    (elm--insert-keyword-commentary . "Commentary")
    (elm--insert-keyword-packages   . nil))
  "List of sections in keyword pages.
The car of each entry is a function"
  :group 'elm-org
  :type '(list (function :tag "Function")
	       (string   :tag "Title")))

(defun elm-create-keyword-page (spec)
  "Create page for the keyword specified by SPEC.
If the page already exists it is not modified."
  (elm-create-page (concat elm-keyword-page-repo (car spec) ".org")
		   elm-feature-page-sections spec))

(defun elm--insert-keyword-header (spec)
  (insert "#+SETUPFILE: ../inc/keyword-page-setup.org\n\n")
  (insert "#+TITLE: ")
  (insert (car spec))
  (let ((summary (cdr (assoc (intern (car spec)) finder-known-keywords))))
    (when summary
      (insert " --- ")
      (insert summary)))
  (insert "\n"))

(defun elm--insert-keyword-commentary (spec)
  (let ((commentary nil)) ; FIXME
    (if commentary
	(insert (concat commentary "\n"))
      (insert "No commentary available.\n"))))
    
(defun elm--insert-keyword-packages (spec)
  (insert (elm--format-list-elements 'elm--insert-package-link
				     nil "../" (cdr spec))))

;; Create Pre- and Postambles.

(defun elm--insert-preamble (file args)
  (let (string)
    (setq file (concat elm-page-repo (convert-standard-filename
				      (concat "inc/" file))))
    (when (file-exists-p file)
      ;; Kludge.  `org-publish-org-to-html' puts everything inside
      ;; div#content.  We don't want that - it doesn't make any sense.
      (insert "</div>\n<div id=\"footer\">\n")
      (insert-file-contents file)
      (goto-char (point-max)))))

(defalias 'elm--insert-postamble 'elm--insert-preamble)

(defun elm--insert-preamble-for-regular-page (&rest args)
  (elm--insert-preamble "web-page-pre.html" args))

(defun elm--insert-postamble-for-regular-page (&rest args)
  (elm--insert-postamble "web-page-post.html" args))

(defun elm--insert-preamble-for-package-list (&rest args)
  (elm--insert-preamble "pkg-list-pre.html" args))

(defun elm--insert-postamble-for-package-list (&rest args)
  (elm--insert-postamble "pkg-list-post.html" args))

(defun elm--insert-preamble-for-package-page (&rest args)
  (elm--insert-preamble "pkg-page-pre.html" args))

(defun elm--insert-postamble-for-package-page (&rest args)
  (let* ((name (substring (file-name-nondirectory buffer-file-name) 0 -5))
	 (term (plist-get (elm-read-epkg name) :license))
	 (file (concat elm-page-repo
		       (convert-standard-filename
			(concat "inc/license/"
				(if (member* term elx-license-search
					     :key 'car :test 'equal)
				    term
				  "unknown")
				".html")))))
    ;; Kludge.  `org-publish-org-to-html' puts everything inside
    ;; div#content.  We don't want that - it doesn't make any sense.
    (insert "</div>\n")
    (insert-file-contents file))
  (goto-char (point-max))
  (elm--insert-postamble "pkg-page-post.html" args))

(defun elm--insert-preamble-for-keyword-page (&rest args)
  (elm--insert-preamble "keyword-page-pre.html" args))

(defun elm--insert-postamble-for-keyword-page (&rest args)
  (elm--insert-preamble "keyword-page-post.html" args))

(provide 'elm-org)
;;; elm-org.el ends here
