;;; pullbib.el --- library to pull zotero libraries via betterbibtex  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <joerg@joergvolbers.de>
;; Keywords: bib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Library to pull zotero libraries 'into' a bib file.

;;; Code:

(defcustom pullbib-url-map
  '(("http://127.0.0.1:23119/better-bibtex/export/library?/1/library.biblatex" . "test.bib"))
  "Alist mapping URLs to filenames.
See https://retorque.re/zotero-better-bibtex/exporting/pull/ for
how to get or create the URL. This function uses the base url
without additional arguments (that is, it should not contain an
ampersand)."
  :group 'pullbib
  :type '(alist :key-type (string :tag "URL" :value-type file)))

(defun pullbib-pull-url (url file)
  "Using URL, pull zotero library to a .bib FILE.
See https://retorque.re/zotero-better-bibtex/exporting/pull/ for
how to get or create the URL. This function uses the base url
without additional arguments (that is, it should not contain an
ampersand).

Example URL: http://127.0.0.1:23119/better-bibtex/export/library?/1/library.biblatex"
  (let* ((bufname  "*Pullbib Shell Output*")
	 (curlname "curl")
	 (result  0))
    (unless (executable-find curlname)
      (error "pullbib fatal error: executable binary 'curl' could not be found"))
    (when (get-buffer bufname)
      (kill-buffer bufname))
    (unless
	(eq 0 (setq result (call-process curlname
					 nil
					 (list bufname t)
					 nil
					 url
					 "--no-progress-meter"
					 (concat "-o" file))))
      (error "pullbib fatal error: curl returns error code %d, see %s for details" result bufname))))

;;;###autoload
(defun pullbib-pull (url-file-map)
  "Pull all urls in URL-FILE-MAP to their respective files."
  (interactive (list pullbib-url-map))
  (cl-dolist (kv url-file-map)
    (let* ((file (cdr kv))
	   (url  (car kv))
	   (msg  (format "Pulling library '%s'..." file)))
    (with-temp-message msg
      (pullbib-pull-url url file))
    (message (concat msg "done.")))))

(provide 'pullbib)
;;; pullbib.el ends here
