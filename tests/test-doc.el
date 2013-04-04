;;; test-doc.el --- Tests for documentation -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Test the contents of documentation.

;;; Code:

(require 'ert)
(require 'flycheck)

(defmacro flycheck-with-doc-buffer (doc-file &rest body)
  "Create a temp buffer from DOC-FILE and execute BODY."
  (declare (indent 1))
  `(let* ((filename (expand-file-name (concat "../doc/" ,doc-file)
                                      flycheck-testsuite-dir)))
     (should (file-exists-p filename))
     (with-temp-buffer
       (insert-file-contents filename)
       ,@body)))

(ert-deftest doc-all-checkers-documented ()
  "Test that all registered checkers are documented in the Flycheck manual."
  (flycheck-with-doc-buffer "checkers.texi"
    ;; Search for the beginning of the list of checkers
    (re-search-forward "@itemize")
    (dolist (checker flycheck-checkers)
      (forward-line 1)
      (should (looking-at "^@iflyc \\(.*?\\)$"))
      (should (equal (match-string 1) (symbol-name checker))))
    (forward-line 1)
    (should (looking-at "@end itemize"))))

(ert-deftest doc-all-options-documented ()
  "Tests that all option variables are documented in the manual."
  (let ((config-vars (sort (-flatten (-keep #'flycheck-checker-option-vars
                                            (flycheck-declared-checkers)))
                           #'string<)))
    (flycheck-with-doc-buffer "usage.texi"
      ;; Go to the beginning of the configuration section
      (re-search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (re-search-forward "configured via options\\.")
      ;; Verify that all variables are documented
      (dolist (var config-vars)
        (re-search-forward "^@defopt \\(.*?\\)$")
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest doc-all-config-vars-documented ()
  "Tests that all configuration file variables are documented in the manual."
  (let ((option-file-vars (sort (-keep #'flycheck-checker-config-file-var
                                       (flycheck-declared-checkers))
                                #'string<)))
    (flycheck-with-doc-buffer "usage.texi"
      ;; Go to the beginning of the configuration section
      (re-search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (re-search-forward "configuration file variables")
      ;; Verify that all variables are documented
      (dolist (var option-file-vars)
        (re-search-forward "^@defopt \\(.*?\\)$")
        (should (equal (match-string 1) (symbol-name var)))))))


;; Local Variables:
;; coding: utf-8
;; End:

;;; test-doc.el ends here
