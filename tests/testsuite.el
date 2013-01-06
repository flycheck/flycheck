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

(defconst testsuite-dir (file-name-directory load-file-name)
  "Directory of the test suite.")

(defconst testsuite-deps-dir (expand-file-name "deps" testsuite-dir)
  "Directory containing the dependencies of the test suite.")

;; Load the dependencies
(dolist (dep '("dash.el/dash.el" "s.el/s.el"))
  (load (expand-file-name dep testsuite-deps-dir) nil :no-message))

;; Load flycheck
(load (expand-file-name "../flycheck.el" testsuite-dir) nil :no-message)

(defconst testsuite-test-file-pattern "^.*/test-.+$"
  "Regular expression to match names of test files.")

(defun testsuite-test-file-p (filename)
  "Determine whether FILENAME is a test file."
  (and (s-matches? testsuite-test-file-pattern filename)
       (file-regular-p filename)))

(defun testsuite-find-all-test-files (&optional directory)
  "Find and return all test files in DIRECTORY.

If omitted DIRECTORY is `testsuite-dir'."
  (let* ((directory (or directory testsuite-dir))
         (contents (directory-files directory :full-names)))
     (-filter 'testsuite-test-file-p contents)))

(defconst testsuite-testfiles (testsuite-find-all-test-files)
  "All test files of this test suite.")

(dolist (testfile testsuite-testfiles)
  (load testfile nil :no-message))

;;; testsuite.el ends here
