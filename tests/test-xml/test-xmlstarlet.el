;;; test-xmlstarlet.el --- Test the xmlstarlet checker

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

;;; Code:

(require 'ert)
(require 'flycheck)

(require 'nxml-mode)

(ert-deftest checker-xmlstarlet-missing-quote ()
  "Test a missing quote in an attribute value."
  :expected-result (flycheck-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-with-resource-buffer "test-xml/missing-quote.xml"
    (nxml-mode)
    (flycheck-should-checker
     'xml-xmlstarlet
     '(5 1 "Unescaped '<' not allowed in attributes values" error)
     '(5 1 "attributes construct error" error)
     '(5 1 "Couldn't find end of Start Tag with" error))))

(ert-deftest checker-xmlstarlet-missing-closing-tag ()
  "Test a missing closing tag."
  :expected-result (flycheck-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-with-resource-buffer "test-xml/missing-closing-tag.xml"
    (nxml-mode)
    (flycheck-should-checker
     'xml-xmlstarlet
     '(5 8 "Opening and ending tag mismatch: with line 4 and spam" error))))

(ert-deftest checker-xmlstarlet-lone-closing-tag ()
  "Test a lone closing tag."
  :expected-result (flycheck-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-with-resource-buffer "test-xml/lone-closing-tag.xml"
    (nxml-mode)
    (flycheck-should-checker
     'xml-xmlstarlet
     '(4 10 "Opening and ending tag mismatch: spam line 3 and with" error))))

(ert-deftest checker-xmlstarlet-undefined-entity ()
  "Test an undefined entity."
  :expected-result (flycheck-fail-unless-checker 'xml-xmlstarlet)
  (flycheck-with-resource-buffer "test-xml/undefined-entity.xml"
    (nxml-mode)
    (flycheck-should-checker
     'xml-xmlstarlet '(4 25 "Entity 'foo' not defined" error))))

;;; test-xmlstarlet.el ends here
