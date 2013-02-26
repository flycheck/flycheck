;;; test-selection.el --- Tests for deferred checking API -*- lexical-binding: t; -*-

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

(ert-deftest flycheck-buffer-deferred ()
  "Test that deferred checking is enabled correctly."
  (with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))

(defvar-local flycheck-buffer-safe-called nil
  "Whether flycheck-buffer-safe was called.")

(defadvice flycheck-buffer-safe (around flycheck-buffer-safe-mock)
  (setq flycheck-buffer-safe-called t))

(ert-deftest flycheck-perform-deferred-syntax-check ()
  "Test that a deferred syntax check is correctly performed."
  (unwind-protect
      (with-temp-buffer
        (ad-activate 'flycheck-buffer-safe)
        (should-not flycheck-buffer-safe-called)
        (flycheck-perform-deferred-syntax-check)
        (should-not flycheck-buffer-safe-called)
        (flycheck-buffer-deferred)
        (flycheck-perform-deferred-syntax-check)
        (should flycheck-buffer-safe-called))
    (ad-deactivate 'flycheck-buffer-safe)))

(ert-deftest flycheck-buffer-safe-deferred ()
  "Test that `flycheck-buffer-safe' properly defers the check."
  (with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-safe)
    (should (flycheck-deferred-check-p))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; checkers-selection.el ends here
