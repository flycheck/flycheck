;;; test-documentation.el --- Flycheck Specs: Documentation -*- lexical-binding: t; -*-

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

;; Specs for our documentation

;;; Code:

(require 'flycheck-buttercup)
(require 'cl-lib)
(require 'seq)
(require 'dash)

(defun flycheck/collect-matches (pattern filename)
  "Collect all instances matching PATTERN from FILENAME."
  (let ((matches))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (re-search-forward pattern nil 'noerror)
        (let ((match (match-string 1)))
          (cl-pushnew match matches))
        (-when-let (match (match-string 2))
          (cl-pushnew match matches))))
    matches))

(defconst flycheck/checker-re
  (rx bol "   .. syntax-checker:: " (group (1+ nonl)) "\n"
      (? "                       " (group (1+ nonl)) eol)))

(defconst flycheck/defcustom-re
  (rx bol "      .. defcustom:: " (group (1+ nonl)) "\n"
      (? "                     " (group (1+ nonl)) eol)))

(describe "Documentation"
  (let* ((source-dir (locate-dominating-file default-directory "Cask"))
         (languages (expand-file-name "doc/languages.rst" source-dir))
         (documented-checkers
          (seq-map (lambda (sig)
                     (let ((parts (split-string sig " ")))
                       (cons (intern (nth 1 parts)) (intern (nth 0 parts)))))
                   (flycheck/collect-matches flycheck/checker-re
                                             languages))))

    (describe "Syntax checkers"
      (dolist (checker flycheck-checkers)
        (it (format "documents %s with stage %s"
                    checker (flycheck-checker-get checker 'stage))
          (expect (cdr (assq checker documented-checkers))
                  :to-equal (flycheck-checker-get checker 'stage))))

      (it "doesn't document syntax checkers that don't exist"
        (expect (seq-difference (seq-map #'car documented-checkers)
                                flycheck-checkers)
                :to-equal nil))

      (describe "Options"
        (let ((documented-options (seq-map #'intern
                                           (flycheck/collect-matches
                                            flycheck/defcustom-re languages)))
              (all-options (seq-mapcat (lambda (c)
                                         (flycheck-checker-get c 'option-vars))
                                       flycheck-checkers)))

          (it "documents all syntax checker options"
            (expect (seq-difference all-options documented-options)
                    :to-equal nil))

          (it "doesn't document syntax checker options that don't exist"
            (expect (seq-difference documented-options all-options)
                    :to-equal nil))))

      (describe "Configuration files"
        (let ((documented-file-vars
               (seq-map #'intern
                        (flycheck/collect-matches
                         (rx ".. syntax-checker-config-file:: "
                             (group (1+ nonl)) eol)
                         languages)))
              (all-file-vars (delq nil
                                   (seq-map (lambda (c)
                                              (flycheck-checker-get
                                               c 'config-file-var))
                                            flycheck-checkers))))
          (it "documents all syntax checker configuration files"
            (expect (seq-difference all-file-vars documented-file-vars)
                    :to-equal nil))

          (it "it doesn't document configuration files that don't exist"
            (expect (seq-difference documented-file-vars all-file-vars)
                    :to-equal nil)))))))

;;; test-documentation.el ends here
