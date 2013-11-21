;;; customization-test.el --- Tests for Flycheck customization  -*- lexical-binding: t; -*-

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

;; Unit tests for Flycheck customization interface.

;;; Code:

(require 'test-helper)
(require 'flycheck)

(ert-deftest flycheck-checkers/there-are-registered-checkers ()
  (should flycheck-checkers))

(ert-deftest flycheck-checkers/all-registered-checkers-are-declared ()
  (dolist (checker flycheck-checkers)
    (should (flycheck-valid-checker-p checker))
    (should (flycheck-registered-checker-p checker))))

(ert-deftest flycheck-checkers/all-declared-checkers-are-registered ()
  (let ((declared-checkers (flycheck-defined-checkers)))
    (should declared-checkers)
    (dolist (checker declared-checkers)
      (should (memq checker flycheck-checkers))
      (should (flycheck-registered-checker-p checker)))))

(ert-deftest flycheck-checkers/should-have-either-patterns-or-parser ()
  (dolist (checker flycheck-checkers)
    (let ((patterns (flycheck-checker-error-patterns checker))
          (parser (flycheck-checker-error-parser checker)))
      (should checker)
      (should (or (and (eq parser 'flycheck-parse-with-patterns) patterns)
                  (null patterns))))))

(ert-deftest flycheck-locate-config-file-functions/default ()
  (should (equal flycheck-locate-config-file-functions
                 '(flycheck-locate-config-file-absolute-path
                   flycheck-locate-config-file-projectile
                   flycheck-locate-config-file-ancestor-directories
                   flycheck-locate-config-file-home))))

(ert-deftest flycheck-process-error-functions/default ()
  (should (equal flycheck-process-error-functions '(flycheck-add-overlay))))

(ert-deftest flycheck-display-errors-delay/default ()
  (should (equal flycheck-display-errors-delay 0.9)))

(ert-deftest flycheck-display-errors-function/default ()
  (should (eq flycheck-display-errors-function
              #'flycheck-display-error-messages)))

(ert-deftest flycheck-indication-mode/default ()
  (should (eq flycheck-indication-mode 'left-fringe)))

(ert-deftest flycheck-highlighting-mode/default ()
  (should (eq flycheck-highlighting-mode 'symbols)))

(ert-deftest flycheck-check-syntax-automatically/default ()
  (should (equal flycheck-check-syntax-automatically
                 '(save idle-change new-line mode-enabled))))

(ert-deftest flycheck-idle-change-delay/default ()
  (should (equal flycheck-idle-change-delay 0.5)))

(ert-deftest flycheck-google-max-messages/default ()
  (should (equal flycheck-google-max-messages 5)))

(ert-deftest flycheck-standard-error-navigation/default ()
  (should (eq flycheck-standard-error-navigation t)))

(ert-deftest flycheck-completion-system/default ()
  (should (equal flycheck-completion-system 'ido)))

(ert-deftest flycheck-executable-variables ()
  (dolist (checker flycheck-checkers)
    (let ((variable (flycheck-checker-executable-variable checker)))
      (should (symbolp variable))
      (should (custom-variable-p variable))
      (should-not (symbol-value variable)))))

;;; customization-test.el ends here
