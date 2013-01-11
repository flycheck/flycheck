;;; test-lua.el --- Test the lua checker

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

(if (or (string= (getenv "TRAVIS") "true") (string= user-login-name "vagrant"))
    ;; HACK: Use system-wide installation in Vagrant and on Travis.  The MELPA
    ;; package of lua-mode causes tests to hang in these environments
    (load "/usr/share/emacs/site-lisp/lua-mode/lua-mode.el")
  (package-need 'lua-mode))
(require 'lua-mode)

(ert-deftest lua-missing-quote ()
  "Test a syntax error with Lua."
  :expected-result (flycheck-fail-unless-checker 'lua)
  (flycheck-with-resource-buffer "test-lua/missing-quote.lua"
    (lua-mode)
    (flycheck-should-checker
     'lua '(5 nil "unfinished string near '\"oh no'" error))))

;;; test-lua.el ends here
