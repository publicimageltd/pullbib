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

;; * Dependencies

(require 'cl-lib)

;; * Fixed Global Settings

(defvar pullbib-curl-binary-name "curl"
  "Name for the Curl executable binary.")

(defvar pullbib-zotero-binary-name "zotero"
  "Name for the Zotero executable binary.")

;; * Customizable Global Settings

(defcustom pullbib-shell-output-buffer "*Pullbib Shell Output*"
  "Name for the buffer catching the output of the curl binary."
  :group 'pullbib
  :type 'string)

(defcustom pullbib-url-map
  nil
  "Alist mapping URLs to filenames.
See https://retorque.re/zotero-better-bibtex/exporting/pull/ for
how to get or create the URL.  This function uses the base url
without additional arguments (that is, it should not contain an
ampersand).

Example URL: http://127.0.0.1:23119/better-bibtex/collection?/library.biblatex"
  :group 'pullbib
  :type '(alist :key-type (string :tag "URL" :value-type file)))

(defcustom pullbib-ping-url
  '("http://127.0.0.1:23119/connector/ping" "Zotero is running")
  "URL and regexp to 'ping' the Zotero Connector Service.
Pullbib retrieves the URL and checks the result against the
regexp.  A match indicates that Zotero is ready)."
  :group 'pullbib
  :type '(list (string :tag "URL") (regexp :tag "Regexp")))

;; * Curl Stuff

(define-error 'curl-error "Exit code returned by the curl binary")

(defun pullbib-assert-curl-binary ()
  "Raise an error if curl cannot be executed."
  (unless (executable-find pullbib-curl-binary-name)
    (error "Pullbib fatal error: executable binary 'curl' could not be found")))

(defun pullbib-curl (&rest args)
  "Call curl with ARGS.
If curl exits with 0, return its output as a string.  Else raise
an error of type `curl-error' with curl's exit code.  Shell output
is redirected to `pullbib-shell-output-buffer'."
  (let ((result (apply #'call-process
                       pullbib-curl-binary-name
                       nil
                       (list pullbib-shell-output-buffer t)
                       nil
                       args)))
    (if (eq 0 result)
        (with-current-buffer pullbib-shell-output-buffer
          (buffer-string))
      (signal 'curl-error
              (list result)))))

;; * Test if Zotero is running

;; It is possible to start Zotero from within emacs if it is not
;; running. However, it is hard to detach the Zotero process from
;; Emacs. An associated process would also close when Emacs crashes.
;; Further, opening it asynchronously still requires some confirmation
;; from the user that Zotero is actually ready. Like an OS, Zotero
;; takes some time initializing. So I do not see an easy way to "just
;; open it", and there would be still the hazzle with having a process
;; running which dies when Emacs (or Emacsclient) closes.

(defun pullbib-zotero-running-p ()
  "Test if zotero is runnning."
  (pullbib-assert-curl-binary)
  (when (get-buffer pullbib-shell-output-buffer)
    (kill-buffer pullbib-shell-output-buffer))
  (let ((ping-url (car pullbib-ping-url))
        (ping-match (cadr pullbib-ping-url))
        (result nil))
    (condition-case err
        (progn
          (or (setq result (pullbib-curl ping-url))
              (error "Pullbib: URL %s yielded no result" ping-url))
          (or (string-match-p ping-match result)
              (error "Pullbib: Pinging Zotero yielded unexpected result %s" result)))
      (curl-error (if (eq 7 (cadr err))
                      ;; exit code 7 indicates host not reachable:
                      nil
                    ;; any other error might be due to other causes:
                    (error "Pullbib: could not reach Zotero binary using URL %s" ping-url))))))

;; * Interactive Functions

;;;###autoload
(defun pullbib-pull (&optional url-file-map)
  "Pull all urls in URL-FILE-MAP to their respective files.
URL-FILE-MAP is an alist mapping URL strings (as car) to a
filename (as cdr).

If URL-FILE-MAP is nil, use the value of `pullbib-url-map'
instead."
  (interactive (list pullbib-url-map))
  (unless (pullbib-zotero-running-p)
    (error "Pullbib: You have to start Zotero to pull any library"))
  (when (get-buffer pullbib-shell-output-buffer)
    (kill-buffer pullbib-shell-output-buffer))
  (let (error-p)
    (cl-dolist (kv (or url-file-map pullbib-url-map))
      (let* ((file (cdr kv))
             (url  (car kv))
             (msg  (format "Pullbib: Pulling library '%s'..." file)))
        (condition-case-unless-debug err
            (progn
              (with-temp-message msg
                (pullbib-curl url
                              "--no-progress-meter"
                              (concat "-o" (expand-file-name file))))
              (message (concat msg "done.")))
          (curl-error (with-current-buffer pullbib-shell-output-buffer
                        (setq error-p t)
                        (insert (error-message-string err) "\n"))))))
    (when error-p
      (message "Pullbib: There were errors pulling the libraries, see %s for details"
               pullbib-shell-output-buffer))))

(provide 'pullbib)
;;; pullbib.el ends here
