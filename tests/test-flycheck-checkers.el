;;; test-flycheck-checkers.el --- Tests for flycheck-checkers -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Test that the contents of `flycheck-checkers' are sane.

;;; Code:

(require 'ert)
(require 's)
(require 'flycheck)

(ert-deftest flycheck-checkers-not-empty ()
  "Test that there are any registered checkers."
  (should flycheck-checkers))

(ert-deftest flycheck-checkers-sound ()
  "Test that `flycheck-checkers' is sound.

Any checker in this list should be valid and registered."
  (dolist (checker flycheck-checkers)
    (should (flycheck-valid-checker-p checker))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checkers-complete ()
  "Test that `flycheck-checkers' is complete.

All declared checkers should be registered."
  (let ((declared-checkers (flycheck-declared-checkers)))
    (should declared-checkers)
    (dolist (checker declared-checkers)
      (should (memq checker flycheck-checkers))
      (should (flycheck-registered-checker-p checker)))))

(ert-deftest flycheck-checkers-patterns-or-parser ()
  "Test that all `flycheck-checkers' have parser and patterns."
  (dolist (checker flycheck-checkers)
    (let ((patterns (flycheck-checker-error-patterns checker))
          (parser (flycheck-checker-error-parser checker)))
      (should checker)
      (should (or (and (eq parser 'flycheck-parse-with-patterns) patterns)
                  (null patterns))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-flycheck-checkers.el ends here
