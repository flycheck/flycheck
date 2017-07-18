;;; test-emacs-lisp.el --- Flycheck Specs: Emacs Lisp      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>

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

;; Specs for Emacs Lisp support.

;;; Code:

(require 'flycheck-buttercup)
(require 'checkdoc)

(defconst flycheck/excluded-checkdoc-vars
  '(checkdoc-autofix-flag checkdoc-bouncy-flag checkdoc-minor-mode-string)
  "Checkdoc variables that should not be passed to the checker.")

(defun flycheck/custom-var-p (var)
  "Check if VAR is a customization pair describing a variable."
  (eq (cadr var) 'custom-variable))

(describe "Language Emacs Lisp"

  (describe "Checkdoc"

    (it "exports all customizable variables"
      (assume (version<= "25" emacs-version)
              "Checkdoc has additional variables in Emacs 25")
      (let* ((checkdoc-custom-group (get 'checkdoc 'custom-group))
             (checkdoc-custom-pairs
              (seq-filter #'flycheck/custom-var-p checkdoc-custom-group))
             (checkdoc-custom-vars (seq-map #'car checkdoc-custom-pairs))
             (checkdoc-relevant-vars
              (cons 'sentence-end-double-space
                    (seq-difference checkdoc-custom-vars
                                    flycheck/excluded-checkdoc-vars))))
        (expect (seq-sort #'string-lessp flycheck-emacs-lisp-checkdoc-variables)
                :to-equal
                (seq-sort #'string-lessp checkdoc-relevant-vars))))))

;;; test-emacs-lisp.el ends here
