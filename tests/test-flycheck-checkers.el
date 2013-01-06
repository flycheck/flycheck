;;; test-flycheck-checkers.el --- Tests for flycheck-checkers

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

;; Test that the contents of `flycheck-checkers' are sane.

;;; Code:

(require 'ert)
(require 's)
(require 'flycheck)

(defmacro* ert-deftest-checkers (test () docstring &body body)
  "Define BODY as test over all `flycheck-checkers'.

Within BODY the current checker is bound to checker."
  (declare (doc-string 3)
           (indent 2))

  `(ert-deftest ,test ()
     ,docstring
     (dolist (checker flycheck-checkers)
       ,@body)))

(ert-deftest-checkers all-checkers-registered ()
  "Test that all `flycheck-checkers' are considered registered."
  (should (flycheck-registered-checker-p checker)))

(ert-deftest-checkers all-checkers-valid ()
  "Test that all `flycheck-checkers' are valid."
  (should (flycheck-valid-checker-p checker)))

;;; test-flycheck-checkers.el ends here
