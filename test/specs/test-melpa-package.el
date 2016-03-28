;;; test-melpa-package.el --- Flycheck Specs: MELPA Package -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Specs for Flycheck MELPA packaging.

;;; Code:

(require 'flycheck-buttercup)
(require 'seq)
(require 'json)
(require 'let-alist)

(defvar url-http-end-of-headers)

(defun flycheck/get-melpa-version ()
  "Get the MELPA version of Flycheck.

Return the version as string, or nil if we failed to obtain the
version."
  (let ((buffer (with-timeout (5)
                  (url-retrieve-synchronously
                   "http://melpa.org/archive.json" 'silent))))
    (when (and buffer (buffer-live-p buffer))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char url-http-end-of-headers)
            (let-alist (json-read)
              (format "%s.%s" (aref .flycheck.ver 0) (aref .flycheck.ver 1))))
        (kill-buffer)))))

(describe "MELPA package"
  (let* ((directory (make-temp-file "flycheck-test-package" 'directory))
         (filename (expand-file-name "flycheck.tar" directory))
         (travis-p (getenv "TRAVIS"))
         version
         entries)

    (before-all
      (unless travis-p
        (with-demoted-errors "Failed to obtain Flycheck package: %S"
          (setq version (flycheck/get-melpa-version))

          (when version
            (let* ((name (format "flycheck-%s" version))
                   (url (format "http://melpa.org/packages/%s.tar" name)))
              (with-timeout (5)
                (url-copy-file url filename)))

            (when (file-exists-p filename)
              (setq entries (seq-map (lambda (entry)
                                       (replace-regexp-in-string
                                        (rx bos (1+ (not (any "/"))) "/")
                                        "" entry))
                                     (process-lines "tar" "-tf" filename))))))))

    (before-each
      (assume (not travis-p)
              (concat "Don't test package on Travis CI. "
                      "Let's not spoil MELPA download stats."))
      (assume version "Flycheck MELPA version not found")
      (assume entries "Could not download and parse Flycheck package"))

    (after-all
      (ignore-errors (delete-directory directory 'recursive)))

    (it "contains flycheck"
      (expect entries :to-contain "flycheck.el"))

    (it "contains flycheck-buttercup"
      (expect entries :to-contain "flycheck-buttercup.el"))

    (it "contains flycheck-ert"
      (expect entries :to-contain "flycheck-ert.el"))))

;;; test-melpa-package.el ends here
