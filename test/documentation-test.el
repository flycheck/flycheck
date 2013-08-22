;;; documentation-test.el --- Tests for Flycheck documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Tests for documentation and manual.

;;; Code:

(require 'test-helper)

(defmacro flycheck-with-doc-buffer (&rest body)
  "Create a temp buffer with flycheck.texi and execute BODY."
  (declare (indent 0))
  `(let* ((filename (f-join flycheck-testsuite-dir "../doc/flycheck.texi")))
     (with-temp-buffer
       (insert-file-contents filename)
       ,@body)))

(ert-deftest doc-all-checkers-documented ()
  "Test that all registered checkers are documented in the Flycheck manual."
  (flycheck-with-doc-buffer
    (search-forward "@node Syntax checkers")
    (search-forward "@itemize")
    (dolist (checker flycheck-checkers)
      (forward-line 1)
      (should (looking-at (rx line-start "@iflyc " symbol-start
                              (group (one-or-more not-newline))
                              symbol-end line-end)))
      (should (equal (match-string 1) (symbol-name checker))))
    (forward-line 1)
    (should (looking-at (rx "@end itemize")))))

(ert-deftest doc-all-options-documented ()
  "Tests that all option variables are documented in the manual."
  (let ((config-vars (sort (-flatten (-keep #'flycheck-checker-option-vars
                                            (flycheck-defined-checkers)))
                           #'string<)))
    (flycheck-with-doc-buffer
      ;; Go to the beginning of the configuration section
      (search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (search-forward "configured via options.")
      ;; Verify that all variables are documented
      (dolist (var config-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest doc-all-config-vars-documented ()
  "Tests that all configuration file variables are documented in the manual."
  (let ((option-file-vars (sort (-keep #'flycheck-checker-config-file-var
                                       (flycheck-defined-checkers))
                                #'string<)))
    (flycheck-with-doc-buffer
      ;; Go to the beginning of the configuration section
      (search-forward "@node Configuration")
      ;; Go to the beginning of the option variable listing
      (search-forward "configuration file variables")
      ;; Verify that all variables are documented
      (dolist (var option-file-vars)
        (re-search-forward (rx line-start "@defopt " symbol-start
                               (group (one-or-more not-newline))
                               symbol-end line-end))
        (should (equal (match-string 1) (symbol-name var)))))))

(ert-deftest flycheck-describe-checker-pops-up-help ()
  "Test that describing a syntax checker pops up a help buffer."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (should (buffer-live-p (get-buffer (help-buffer))))
      (should (get-buffer-window (help-buffer)))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward (rx symbol-start (group (one-or-more not-newline))
                               symbol-end " is a Flycheck syntax checker"))
        (should (= (match-beginning 0) 1))
        (should (string= (match-string 1) (symbol-name checker)))))))

(ert-deftest flycheck-describe-checker-navigate-to-source ()
  "Test that checkers are properly described."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         (rx "`" (minimal-match (zero-or-more not-newline)) "'"))
        (should (string= (match-string 0) "`flycheck.el'"))
        (push-button (+ 2 (match-beginning 0)))
        (unwind-protect
            (progn
              (should (string= (buffer-name) "flycheck.el"))
              (should (looking-at
                       (rx line-start "("
                           symbol-start "flycheck-define-checker" symbol-end " "
                           symbol-start (group (one-or-more not-newline)) symbol-end
                           line-end)))
              (should (string= (match-string 1) (symbol-name checker))))
          (kill-buffer))))))

(ert-deftest flycheck-describe-checker-executable-name ()
  "Test that the command name appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "This\\s-+syntax\\s-+checker\\s-+executes\\s-+\"\\(.+?\\)\"\\(?:\\.\\|,\\)")
        (should (string= (match-string 1)
                         (flycheck-checker-executable checker)))))))

(ert-deftest flycheck-describe-checker-config-file-var ()
  "Test that the config file var appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (let ((config-file-var (flycheck-checker-config-file-var checker)))
          (if (not config-file-var)
              (should-not (s-contains? "configuration file"
                                       (buffer-substring (point-min) (point-max))))
            (goto-char (point-min))
            (re-search-forward
             ", using\\s-+a\\s-+configuration\\s-+file\\s-+from\\s-+`\\(.+?\\)'\\.")
            (should (equal (match-string 1) (symbol-name config-file-var)))))))))

(ert-deftest flycheck-describe-checker-option-vars ()
  "Test that option variables appear in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (let ((option-vars (sort (flycheck-checker-option-vars checker)
                                 #'string<))
              ;; The regular expression to find the beginning of the option
              ;; variable list
              (regexp "This\\s-+syntax\\s-+checker\\s-+can\\s-+be\\s-+configured\\s-+with\\s-+these\\s-+options:\n"))
          (goto-char (point-min))
          (if (not option-vars)
              ;; If there are no variables, we should not see a list of them
              (should-not (re-search-forward regexp nil :no-error))
            ;; Find the beginning of the option var listing
            (re-search-forward regexp)
            (goto-char (match-end 0))
            ;; Test that each variable is properly listed
            (dolist (var option-vars)
              (forward-line 1)
              (should (looking-at "^     \\* `\\(.+?\\)'$"))
              (should (equal (match-string 1) (symbol-name var))))
            ;; After the list of options there should be a blank line to
            ;; separate the variable list from the actual docstring
            (forward-line 1)
            (should (looking-at "^$"))))))))

(ert-deftest flycheck-describe-checker-docstring ()
  "Test that the docstring appears in syntax checker help."
  (dolist (checker (flycheck-defined-checkers))
    (flycheck-testsuite-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (should (s-contains? (flycheck-checker-documentation checker)
                             (buffer-substring (point-min) (point-max))))))))

;;; documentation-test.el ends here
