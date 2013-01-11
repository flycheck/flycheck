;;; test-ruby.el --- Test the Ruby checker

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as publirubyed by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You rubyould have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest ruby-unexpected-string ()
  "Test a Ruby syntax error."
  (flycheck-with-resource-buffer "unexpected-string.rb"
    (ruby-mode)
    (flycheck-should-checker
     'ruby
     '(4 nil "syntax error, unexpected tSTRING_BEG, expecting $end" error))))

(ert-deftest ruby-missing-quote ()
  "Test a Ruby syntax error."
  (flycheck-with-resource-buffer "missing-quote.rb"
    (ruby-mode)
    (flycheck-should-checker
     'ruby
     '(5 nil "syntax error, unexpected tCONSTANT, expecting $end" error))))

;;; test-ruby.el ends here
