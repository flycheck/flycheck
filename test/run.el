;;; run.el --- Flycheck: Test runner    -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Flycheck contributors
;; Copyright (C) 2014-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;; URL: https://www.flycheck.org

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

;; Flycheck test runner.

;;; Code:

(defun flycheck-run-check-selector (selector)
  "Check SELECTOR if it fails loading."
  (with-temp-buffer
    (insert selector)
    (goto-char (point-min))
    (condition-case nil
        (progn
          (check-parens)
          (message "Invalid selector: %S" selector))
      (error
       (message "Unbalanced parenthesis in selector %S at %s"
                selector (1+ (current-column)))))))

(defun flycheck-transform-selector (selector)
  "Transform SELECTOR to implement some custom selectors.

This function adds the following custom selectors:

`(language FOO)' -> `(tag language-FOO)'
`(checker FOO)'  -> `(tag checker-FOO)'"
  (pcase selector
    (`(language ,(and language (pred symbolp)))
     (list 'tag (intern (concat "language-" (symbol-name language)))))
    (`(checker ,(and checker (pred symbolp)))
     (list 'tag (intern (concat "checker-" (symbol-name checker)))))
    (`(new-checker-for ,(and language (pred symbolp)))
     ;; For a new checker for a language we need to run the documentation and
     ;; style tests, and all tests for the corresponding language, in order to
     ;; make sure that all chaining still works, and that the order of checkers
     ;; is still correct.
     (flycheck-transform-selector `(or (tag documentation)
                                       (tag style)
                                       (language ,language))))
    (`(,group . ,body)
     (cons group (mapcar #'flycheck-transform-selector body)))
    (simple simple)))

(defun flycheck-read-whole-string (str)
  "Read from whole STR."
  (pcase-let ((`(,obj . ,index) (read-from-string str)))
    (if (/= index (length str))
        (error "Can't read whole string")
      obj)))

(defun flycheck-run-tests-batch-and-exit ()
  "Run test cases matching tags in `argv' and exit.

Read an ERT test selector from the first command line argument,
run matching tests and exit.  See `ert-select-tests' and Info
Node `(ert)Test Selectors' for information about test selectors."
  (when (string= (car argv) "--")
    ;; Skip over the command line argument separator
    (pop argv))
  (let ((selector (pop argv)))
    (when argv
      ;; Warn about unused trailing arguments, and remove them, to prevent Emacs
      ;; from trying to parse them.
      (message "WARNING: Unused trailing arguments: %S" argv)
      (setq argv nil))
    (setq selector
          `(and "flycheck-"
                ,(cond
                  ((not selector) t)
                  ((= (length selector) 0)
                   (message "Warning: Empty test selector, defaulting to t")
                   t)
                  (t (condition-case nil
                         (flycheck-read-whole-string selector)
                       (error
                        (flycheck-run-check-selector selector)
                        (kill-emacs 1)))))))
    (flycheck-run-unlimit-ert-pretty-printer)
    (ert-run-tests-batch-and-exit (flycheck-transform-selector selector))))

;; ERT in batch mode will truncate long output by default.  We don't want that,
;; as it's easier to fix failures with the full output, since we cannot always
;; easily reproduce locally (for integration tests running on CI).
(defun flycheck-run-unlimit-ert-pretty-printer ()
  "Install advice to unlimit the ERT output."
  (advice-add 'ert--pp-with-indentation-and-newline :around
              (lambda (orig &rest args)
                (let ((print-length nil)
                      (print-level nil))
                  (apply orig args)))))

(defvar flycheck-runner-file
  (if load-in-progress load-file-name (buffer-file-name)))

(defun flycheck-run-tests-main ()
  "Main entry point of the test runner."
  (let* ((load-prefer-newer t)
         (source-directory (locate-dominating-file flycheck-runner-file "Cask"))
         (pkg-rel-dir (format ".cask/%s.%S/elpa"
                              emacs-major-version
                              emacs-minor-version)))

    ;; Standardise on the C locale to prevent programs from writing fancy
    ;; unicode characters and thus make test output predictable
    (setenv "LC_ALL" "C")

    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s, built at %s"
             emacs-version (format-time-string "%F" emacs-build-time))

    (let ((debug-on-error t))
      (flycheck-run-tests-batch-and-exit))))

;;; run.el ends here
