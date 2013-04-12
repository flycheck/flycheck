;;; test-lua.el --- Test the lua checker -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>,
;;                    Peter Vasil <mail@petervasil.net>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>,
;;         Peter Vasil <mail@petervasil.net>
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

(require 'lua-mode nil t)

(ert-deftest checker-lua-syntax-error ()
  :expected-result (flycheck-testsuite-fail-unless-checker 'lua)
  (flycheck-testsuite-should-syntax-check
   "checkers/lua-syntax-error.lua" 'lua-mode nil
   '(5 nil "unfinished string near '\"oh no'" error)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-lua.el ends here
