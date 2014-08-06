#!/bin/bash
":"; exec ${EMACS:-emacs} -Q --script "$0" "${@}" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; run.el --- Flycheck: Test runner

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Flycheck test runner.
;;
;; See URL `http://stackoverflow.com/a/6259330/355252' for the shebang.

;;; Code:

(require 'thingatpt)                    ; For `read-from-whole-string'

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
    (setq selector (cond
                    ((not selector) t)
                    ((= (length selector) 0)
                     (message "Warning: Empty test selector, defaulting to t")
                     t)
                    (t (read-from-whole-string selector))))
    (ert-run-tests-batch-and-exit selector)))

(defun flycheck-runs-this-script-p ()
  "Whether this file is executed as script."
  t)

(defun flycheck-run-tests-main ()
  "Main entry point of the test runner."
  (let* ((load-prefer-newer t)
         (current-file (if load-in-progress load-file-name (buffer-file-name)))
         (source-directory (locate-dominating-file current-file "Cask"))
         (pkg-rel-dir (format ".cask/%s/elpa" emacs-version)))
    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s" emacs-version)
    (load (expand-file-name "flycheck" source-directory))
    (load (expand-file-name "flycheck-test"
                            (file-name-directory current-file))))

  (let ((debug-on-error t))
    (flycheck-run-tests-batch-and-exit)))

(when (and noninteractive (flycheck-runs-this-script-p))
  (flycheck-run-tests-main))

;;; run-tests.el ends here
