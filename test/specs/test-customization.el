;;; test-customization.el --- Flycheck Specs: Customization -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Specs for Flycheck customization variables and their defaults.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Customization"

  (describe "flycheck-checkers"

    (it "has registered checkers"
      (expect flycheck-checkers :to-be-truthy))

    (it "all registered checkers are declared"
      (dolist (checker flycheck-checkers)
        (expect (flycheck-valid-checker-p checker) :to-be-truthy)
        (expect (flycheck-registered-checker-p checker) :to-be-truthy)))

    (it "all declared checkers are registered"
      (let ((declared-checkers (flycheck-defined-checkers)))
        (expect declared-checkers :to-be-truthy)
        (dolist (checker declared-checkers)
          (expect (memq checker flycheck-checkers) :to-be-truthy)
          (expect (flycheck-registered-checker-p checker) :to-be-truthy))))

    (it "no registered checker is disabled"
      (dolist (checker flycheck-checkers)
        (expect (flycheck-disabled-checker-p checker) :not :to-be-truthy))))

  (describe "flycheck-disabled-checkers"

    (it "is empty"
      (expect (default-value 'flycheck-disabled-checkers) :not :to-be-truthy)))

  (describe "flycheck-locate-config-file-functions"

    (it "defaults to the standard functions"
      (expect flycheck-locate-config-file-functions
              :to-equal
              '(flycheck-locate-config-file-by-path
                flycheck-locate-config-file-ancestor-directories
                flycheck-locate-config-file-home))))

  (describe "flycheck-process-error-functions"

    (it "defaults to add-overlay"
      (expect flycheck-process-error-functions
              :to-equal '(flycheck-add-overlay))))

  (describe "flycheck-display-errors-delay"

    (it "defaults to 0.9"
      (expect flycheck-display-errors-delay :to-equal 0.9)))

  (describe "flycheck-display-errors-function"

    (it "defaults to display-error-messages"
      (expect flycheck-display-errors-function
              :to-be #'flycheck-display-error-messages)))

  (describe "flycheck-indication-mode"

    (it "defaults to left-fringe"
      (expect flycheck-indication-mode :to-be 'left-fringe)))

  (describe "flycheck-highlighting-mode"

    (it "defaults to symbols"
      (expect flycheck-highlighting-mode :to-be 'symbols)))

  (describe "flycheck-check-syntax-automatically"

    (it "defaults to all events"
      (expect flycheck-check-syntax-automatically
              :to-equal '(save idle-change new-line mode-enabled))))

  (describe "flycheck-idle-change-delay"

    (it "defaults to 0.5"
      (expect flycheck-idle-change-delay :to-equal 0.5)))

  (describe "flycheck-standard-error-navigation"

    (it "defaults to t"
      (expect flycheck-standard-error-navigation :to-be t)))

  (describe "flycheck-CHECKER-executable"

    (it "is a special variable"
      (dolist (checker flycheck-checkers)
        (let ((variable (flycheck-checker-executable-variable checker)))
          (expect (custom-variable-p variable) :to-be-truthy))))

    (it "is customizable"
      (dolist (checker flycheck-checkers)
        (let ((variable (flycheck-checker-executable-variable checker)))
          (expect (custom-variable-p variable) :to-be-truthy))))

    (it "defaults to nil"
      (dolist (checker flycheck-checkers)
        (let ((variable (flycheck-checker-executable-variable checker)))
          (expect (null (symbol-value variable)) :to-be-truthy)))))

  (describe "flycheck-keymap-prefix"

    (it "customize-variable will modify keymap"
      (unwind-protect
          (progn
            (custom-set-variables '(flycheck-keymap-prefix (kbd "C-c e")))
            (expect (lookup-key flycheck-mode-map (kbd "C-c e n"))
                    :to-be 'flycheck-next-error))
        (ignore-errors
          (custom-set-variables '(flycheck-keymap-prefix (kbd "C-c !")))))))

  (describe "flycheck-temp-prefix"

    (it "has the default value"
      (expect flycheck-temp-prefix :to-equal "flycheck"))))

;;; test-customization.el ends here
