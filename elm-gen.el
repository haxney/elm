;;; elm-gen.el --- 

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20091231
;; Updated: 20100216
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

;; See library `elm.el'.

;;; Code:

(require 'cl)

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

;; Updating Metadata.

(defun elm-update-metadata ()
  (interactive)
  (elm-update-features-list)
  (elm-update-keywords-list)
  (elm-update-packages-data)
  (elm-update-packages-index)
  (elm-update-keywords-index)
  ;; This does not create the webpages (html) but the files from which
  ;; (org) these are created.  So it is okay to do this here.
  (elm-update-packages-pages)
  (elm-update-features-pages))

(defun elm-update-features-list ()
  (interactive)
  (setq elx-known-features nil)
  (elm-map-packages
    (lambda (name)
      (message "Updating features of package '%s'..." name)
      (dolist (feature (elx-provided (elm-package-repo name)))
	(aput 'elx-known-features feature name)
	(message "Updating features of package '%s'...done" name))))
  ;; Adding the features provided by Emacs after the mirrored packages
  ;; enjures that packages are not pulled in that are actually provided
  ;; by emacs.  However this also means that the actual package being
  ;; depended on is not listed in the epkg.
  ;; TODO find a solution to satisfy both needs.
  (message "Updating features of Emacs...")
  (dolist (feature (elx-provided elm-emacs-directory))
    (aput 'elx-known-features feature "emacs"))
  (message "Updating features of Emacs...done"))

(defun elm-update-keywords-list ()
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
  (interactive)
  (elm-map-packages
    (lambda (name)
      (message "Updating metadata of package '%s'..." name)
      (elm-save-data name)
      (message "Updating metadata of package '%s'...done" name))))

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

;; Updating Webpages.

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

;; Updating Statistics.

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
      ;; TODO output in useful format
      )))

(provide 'elm-gen)
;;; elm-gen.el ends here
