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


;; Test helpers
(defun should-flycheck-overlay (overlay error)
  "Test that OVERLAY is in REGION and corresponds to ERROR."
  (let* ((region (flycheck-error-region error))
         (text (flycheck-error-text error))
         (level (flycheck-error-level error))
         (face (if (eq level 'warning)
                   'flycheck-warning-face
                 'flycheck-error-face))
         (category (if (eq level 'warning)
                       'flycheck-warning-overlay
                     'flycheck-error-overlay))
         (fringe-icon (if (eq level 'warning)
                          '(left-fringe question-mark flycheck-warning-face)
                        `(left-fringe ,flycheck-fringe-exclamation-mark
                                      flycheck-error-face))))
    (should overlay)
    (should (overlay-get overlay 'flycheck-overlay))
    (should (= (overlay-start overlay) (car region)))
    (should (= (overlay-end overlay) (cdr region)))
    (should (eq (overlay-get overlay 'face) face))
    (should (equal (get-char-property 0 'display
                                      (overlay-get overlay 'before-string))
                   fringe-icon))
    (should (eq (overlay-get overlay 'category) category))
    (should (equal (overlay-get overlay 'flycheck-error) error))
    (should (string= (overlay-get overlay 'help-echo) text))))

(defun should-flycheck-error (filename expected-err)
  "Test that ERR is an error in the current buffer."
  (let* ((real-error (flycheck-make-error
                      :buffer (current-buffer)
                      :file-name filename
                      :line-no (nth 0 expected-err)
                      :col-no (nth 1 expected-err)
                      :text (nth 2 expected-err)
                      :level (nth 3 expected-err)))
         (overlay (car (flycheck-overlays-at (flycheck-error-pos real-error)))))
    (should (-contains? flycheck-current-errors real-error))
    (should-flycheck-overlay overlay real-error)))

(defun should-flycheck-checker (filename setup-fn checker &rest errors)
  "Test that checking FILENAME with CHECKER gives ERRORS."
  (should (file-exists-p filename))
  (with-temp-buffer
    (insert-file-contents filename t)
    (funcall setup-fn)
    (set (make-local-variable 'flycheck-checkers) (list checker))
    (flycheck-mode)
    (while (flycheck-running-p)
      (sleep-for 1))
    (if (not errors)
        (should-not flycheck-current-errors)
      (dolist (err errors)
        (should-flycheck-error filename err)))))

(defmacro resource (filename)
  "Access a test resource with FILENAME."
  `(expand-file-name ,filename ,default-directory))

(-each testsuite-testfiles 'testsuite-load-testfile)

;;; testsuite.el ends here
