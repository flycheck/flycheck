;;; testsuite.el --- Testsuite for Flycheck

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

;;; Commentary:

;; Entry point of the Flycheck test suite.

;; Load Flycheck, all required dependencies and all tests files.

;;; Code:

(setq debug-on-error t)

(defconst testsuite-dir
  (if load-file-name
      (file-name-directory load-file-name)
    ;; Fall back to default directory (in case of M-x eval-buffer)
    default-directory)
  "Directory of the test suite.")

(message "Running tests on Emacs %s" emacs-version)

(require 'dash)
(require 's)

;; Load flycheck
(load (expand-file-name "../flycheck" testsuite-dir) nil :no-message)

;; Load test helpers
(load (expand-file-name "testhelpers.el" testsuite-dir) nil :no-message)

;; Discover tests
(defconst testsuite-test-file-pattern "^.*/test-[^/]+$"
  "Regular expression to match names of test files.")

(defun testsuite-test-file-p (filename)
  "Determine whether FILENAME is a test file."
  (and (s-matches? testsuite-test-file-pattern filename)
       (file-regular-p filename)))

(defun testsuite-test-dir-p (dirname)
  "Determine whether DIRNAME is a test directory."
  (and (s-matches? testsuite-test-file-pattern dirname)
       (file-directory-p dirname)))

(defun testsuite-find-all-test-files (directory)
  "Recursively all test files in DIRECTORY."
  (let* ((contents (directory-files directory :full-names))
         (test-files (-filter 'testsuite-test-file-p contents))
         (test-dirs (-filter 'testsuite-test-dir-p contents)))
    (append test-files
            (-flatten (-map 'testsuite-find-all-test-files test-dirs))
            nil)))

(defconst testsuite-testfiles (testsuite-find-all-test-files testsuite-dir)
  "All test files of this test suite.")

(defun testsuite-load-testfile (testfile)
  "Load a TESTFILE."
  (let ((default-directory (file-name-directory testfile)))
    (load testfile nil :no-message)))

;; Eventually load all test files
(-each testsuite-testfiles 'testsuite-load-testfile)

;;; testsuite.el ends here
