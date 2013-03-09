;;; test-option-filters.el --- Tests for option filters -*- lexical-binding: t; -*-

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

;;; Commentary

;; Test option filters

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest flycheck-option-int ()
  "Test conversion with `flycheck-option-int'."
  (should (null (flycheck-option-int nil)))
  (should (equal (flycheck-option-int 10) "10")))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-option-filters.el ends here
