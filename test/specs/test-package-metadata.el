;;; test-package-metadata.el --- Flycheck Specs: Package metadata -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;; Specs for Flycheck's own package metadata.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'lisp-mnt)

(defun flycheck/version-header ()
  "Return the `Version' header of flycheck.el."
  (let ((file (expand-file-name "flycheck.el" flycheck-test-source-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (lm-header "version"))))

(defun flycheck/eask-version ()
  "Return the version the Eask file declares."
  (let ((file (expand-file-name "Eask" flycheck-test-source-directory)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward
             (rx "(package" (1+ space) "\"flycheck\"" (1+ (any space "\n"))
                 "\"" (group (1+ (not (any "\"")))) "\"")
             nil 'noerror)
        (match-string 1)))))

(describe "Package metadata"

  ;; `flycheck-version' is only a fallback for when Flycheck was not
  ;; installed as a package, so nothing at runtime notices it going
  ;; stale.  It sat at 37.0 through three releases before anyone did.
  (it "declares the same version in flycheck.el, its header and Eask"
    (expect (flycheck/version-header) :to-equal flycheck-version)
    (expect (flycheck/eask-version) :to-equal flycheck-version))

  (it "declares a version Emacs can compare"
    (expect (ignore-errors (version-to-list flycheck-version))
            :not :to-be nil)))

;;; test-package-metadata.el ends here
