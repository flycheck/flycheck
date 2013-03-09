;;; test-help.el --- Tests for checker help -*- lexical-binding: t; -*-

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

;;; Code:

(require 'help-mode)
(require 'ert)
(require 'flycheck)

(defmacro flycheck-with-help-buffer (&rest body)
  "Execute BODY and kill the help buffer afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (buffer-live-p (get-buffer (help-buffer)))
       (kill-buffer (help-buffer)))))

(ert-deftest flycheck-describe-checker-pops-up-help ()
  "Test that describing a syntax checker pops up a help buffer."
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
      (flycheck-describe-checker checker)
      (should (buffer-live-p (get-buffer (help-buffer))))
      (should (get-buffer-window (help-buffer)))
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward "\\(.*?\\) is a Flycheck syntax checker")
        (should (= (match-beginning 0) 1))
        (should (string= (match-string 1) (symbol-name checker)))))))

(ert-deftest flycheck-describe-checker-navigate-to-source ()
  "Test that checkers are properly described."
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward "`.*?'")
        (should (string= (match-string 0) "`flycheck.el'"))
        (push-button (+ 2 (match-beginning 0)))
        (unwind-protect
            (progn
              (should (string= (buffer-name) "flycheck.el"))
              (should (string=
                       (buffer-substring-no-properties
                        (point) (line-end-position))
                       (format "(flycheck-declare-checker %S" checker))))
          (kill-buffer))))))

(ert-deftest flycheck-describe-checker-executable-name ()
  "Test that the command name appears in syntax checker help."
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (goto-char (point-min))
        (re-search-forward
         "This\\s-+syntax\\s-+checker\\s-+executes\\s-+\"\\(.+?\\)\"\\(?:\\.\\|,\\)")
        (should (string= (match-string 1)
                         (flycheck-checker-executable checker)))))))

(ert-deftest flycheck-describe-checker-config-file-var ()
  "Test that the config file var appears in syntax checker help."
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
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
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
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
  (dolist (checker (flycheck-declared-checkers))
    (flycheck-with-help-buffer
      (flycheck-describe-checker checker)
      (with-current-buffer (help-buffer)
        (should (s-contains? (flycheck-checker-documentation checker)
                             (buffer-substring (point-min) (point-max))))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-help.el ends here
