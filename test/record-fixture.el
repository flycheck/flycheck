;;; record-fixture.el --- Record checker output fixtures -*- lexical-binding: t; -*-

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

;; Record what a syntax checker's tool prints, so a spec can test the
;; parsing of that output without the tool being installed.
;;
;; The command comes from the checker itself, via the same substitution
;; Flycheck uses to run it, so a fixture cannot drift from the command
;; that produced it without the recording failing.
;;
;; Usage:
;;
;;     emacs -Q --batch -l test/record-fixture.el \
;;       -f flycheck-record-fixture-batch CHECKER RESOURCE [OUTPUT-FILE]
;;
;; RESOURCE is a path under test/resources.  With no OUTPUT-FILE the
;; fixture goes to test/fixtures/CHECKER/<resource-basename>.txt.

;;; Code:

(require 'flycheck)
(require 'seq)
(require 'subr-x)

(defconst flycheck-record-fixture--test-directory
  ;; Captured now, while this file is the one being loaded
  (file-name-directory (or load-file-name buffer-file-name))
  "The test directory, as of loading this file.")

(defvar flycheck-record-fixture-directory
  (expand-file-name "fixtures" flycheck-record-fixture--test-directory)
  "Directory holding recorded checker output.")

(defun flycheck-record-fixture-file (checker resource)
  "The file holding CHECKER's recorded output over RESOURCE.

Checker names can contain slashes, as `c/c++-gcc' does, so they are
flattened rather than read as directories."
  (expand-file-name
   (format "%s/%s.txt"
           (replace-regexp-in-string "/" "_" (symbol-name checker))
           (file-name-nondirectory resource))
   flycheck-record-fixture-directory))

(defun flycheck-record-fixture--resource (resource)
  "Absolute name of RESOURCE under the test resources directory."
  (expand-file-name
   resource
   (expand-file-name "resources" flycheck-record-fixture--test-directory)))

(defun flycheck-record-fixture--run (checker file)
  "Run CHECKER's command over FILE and return (COMMAND EXIT-STATUS OUTPUT).

The command is the one Flycheck would run, so a fixture recorded
here matches what the checker actually invokes.  A checker that
reads standard input gets the file's contents there, like it would
during a real check."
  (with-current-buffer (find-file-noselect file)
    (let* ((args (flycheck-checker-substituted-arguments checker))
           (program (flycheck-find-checker-executable checker))
           (stdin (flycheck-checker-get checker 'standard-input))
           (source (buffer-string))
           (default-directory
            (if-let* ((wd (flycheck-checker-get checker 'working-directory)))
                (or (funcall wd checker) default-directory)
              default-directory)))
      (unless program
        (error "%s is not installed, so there is nothing to record" checker))
      (with-temp-buffer
        (let ((status
               (if stdin
                   (let ((input (current-buffer)))
                     (insert source)
                     (apply #'call-process-region (point-min) (point-max)
                            program 'delete (list input t) nil args))
                 (apply #'call-process program nil t nil args))))
          (list (cons program args) status (buffer-string)))))))

(defun flycheck-record-fixture (checker resource &optional output-file)
  "Record CHECKER's output over RESOURCE into OUTPUT-FILE.

Return the file the fixture was written to."
  (let* ((file (flycheck-record-fixture--resource resource))
         (recorded (flycheck-record-fixture--run checker file))
         (command (nth 0 recorded))
         (status (nth 1 recorded))
         (output (nth 2 recorded))
         (target (or output-file
                     (flycheck-record-fixture-file checker file))))
    (make-directory (file-name-directory target) 'parents)
    (with-temp-file target (insert output))
    (message "%s\n  command : %s\n  exit    : %s\n  bytes   : %d\n  written : %s"
             checker (mapconcat #'identity command " ") status
             (length output) target)
    target))

(defun flycheck-record-fixture-batch ()
  "Record a fixture from the remaining command-line arguments."
  (let ((checker (intern (pop command-line-args-left)))
        (resource (pop command-line-args-left))
        (target (pop command-line-args-left)))
    (flycheck-record-fixture checker resource target)))

(provide 'record-fixture)

;;; record-fixture.el ends here
