;;; test-mode.el --- Tests for the mode definition -*- lexical-binding: t; -*-

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

;; Test the general mode definition.

;;; Code:

(require 'ert)
(require 'flycheck)

(ert-deftest flycheck-mode-disabled-for-no-undo-buffers ()
  "Tests that Flycheck mode is not enabled for buffers starting with a space."
  (with-temp-buffer
    (rename-buffer " foo")
    (should (equal (buffer-name) " foo"))
    (flycheck-mode)
    (should-not flycheck-mode)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-mode.el ends here
