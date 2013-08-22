;;; deferred-test.el --- Tests for deferred syntax checking  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Sebastian Wiesner

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/flycheck/flycheck

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

;; Tests for deferred syntax checking.

;;; Code:

(require 'test-helper)

(ert-deftest flycheck-buffer-deferred ()
  "Test that deferred checking is enabled correctly."
  (with-temp-buffer
    (should-not (flycheck-deferred-check-p))
    (flycheck-buffer-deferred)
    (should (flycheck-deferred-check-p))
    (flycheck-clean-deferred-check)
    (should-not (flycheck-deferred-check-p))))

(ert-deftest flycheck-perform-deferred-syntax-check-no-check ()
  "Test that a deferred syntax check is correctly performed."
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:min-occur 0 :max-occur 0))))
    (with-temp-buffer
      (flycheck-perform-deferred-syntax-check)))
  (mocker-let
      ((flycheck-buffer-automatically (&optional error condition)
                                      ((:input '(nil nil)))))
    (with-temp-buffer
      (flycheck-buffer-deferred)
      (flycheck-perform-deferred-syntax-check))))

;;; deferred-test.el ends here
