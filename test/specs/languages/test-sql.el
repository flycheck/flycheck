;;; test-sql.el --- Flycheck Specs: SQL      -*- lexical-binding: t; -*-

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

;; Specs for SQL support.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language SQL"
  (flycheck-buttercup-def-checker-test sqlint sql nil
    (flycheck-buttercup-should-syntax-check
     "language/sql.sql" 'sql-mode
     `(1 15 error "unterminated quoted string at or near \"';\n  \""
         :checker sql-sqlint))))

;;; test-sql.el ends here
