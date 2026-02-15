;;; test-xml.el --- Flycheck Specs: XML      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for XML support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language XML"
  (flycheck-buttercup-def-checker-test xml-xmllint xml nil
    (let ((inhibit-message t))
      (flycheck-buttercup-should-syntax-check
       "language/xml.xml" 'nxml-mode
       '(4 nil error "parser error : Opening and ending tag mismatch: spam line 3 and with"
           :checker xml-xmllint)
       '(5 nil error "parser error : Extra content at the end of the document"
           :checker xml-xmllint)))))

;;; test-xml.el ends here
