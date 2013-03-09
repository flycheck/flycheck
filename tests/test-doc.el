;;; test-doc.el --- Tests for documentation -*- lexical-binding: t; -*-

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

;; Test error parsers

;;; Code:

(require 'ert)
(require 'flycheck)


(ert-deftest doc-all-checkers-documented ()
  "Test that all registered checkers are documented in the Flycheck manual."
  (flycheck-with-resource-buffer "../doc/checkers.texi"
    ;; Search for the beginning of the list of checkers
    (re-search-forward "@itemize")
    (dolist (checker flycheck-checkers)
      (forward-line 1)
      (should (looking-at "^@iflyc \\(.*?\\)$"))
      (should (equal (match-string 1) (symbol-name checker))))
    (forward-line 1)
    (should (looking-at "@end itemize"))))


;; Local Variables:
;; coding: utf-8
;; End:

;;; test-doc.el ends here
