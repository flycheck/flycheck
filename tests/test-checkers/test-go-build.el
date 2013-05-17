;;; test-go-build.el --- Test the go-build checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;                    Peter Vasil <mail@petervasil.net>,
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
;;         Peter Vasil <mail@petervasil.net>,
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

(require 'ert)
(require 'flycheck)

(require 'go-mode nil :no-error)

(ert-deftest checker-go-build-error ()
  "Test an import error."
  :expected-result (flycheck-testsuite-fail-unless-checker 'go-build)
  (flycheck-testsuite-should-syntax-check
   "checkers/go-testpackage/go-build-error.go" 'go-mode nil
   '(6 nil "undefined: fmt" error :checker go-build)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-go-build.el ends here
