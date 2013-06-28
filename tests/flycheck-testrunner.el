;;; flycheck-testrunner.el --- Test runner  -*- lexical-binding: t -*-

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

;; The unit test runner

;;; Code:

(require 'dash)

;;; Load the testsuite
(let* ((testdir (file-name-directory load-file-name))
       (sourcedir (expand-file-name ".." testdir)))
  (--each (list testdir sourcedir)
    (add-to-list 'load-path it)))

(require 'flycheck-testsuite)

(princ (format "Running Flycheck tests under Emacs %s\n" emacs-version))

(ert-run-tests-batch-and-exit (car command-line-args-left))

;; Local Variables:
;; coding: utf-8
;; End:

;;; flycheck-testrunner.el ends here
