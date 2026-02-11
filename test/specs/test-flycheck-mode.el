;;; test-flycheck-mode.el --- Flycheck Specs: Flycheck Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2025 Sebastian Wiesner and Flycheck contributors

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

;; Specs for Flycheck Mode and checker selection.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Flycheck Mode"

  (describe "flycheck-mode"
    (it "enables-standard-error-navigation"
      (flycheck-buttercup-with-temp-buffer
        (setq next-error-function :old)
        (flycheck-mode 1)
        (expect flycheck-mode :to-be-truthy)
        (expect next-error-function :to-be 'flycheck-next-error-function)
        (flycheck-mode -1)
        (expect flycheck-mode :not :to-be-truthy)
        (expect next-error-function :to-be :old)))

    (it "does-not-enable-standard-error-navigation"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-standard-error-navigation nil))
          (setq next-error-function :old)
          (flycheck-mode +1)
          (expect flycheck-mode :to-be-truthy)
          (expect next-error-function :to-be :old)
          (expect flycheck-old-next-error-function :to-be :unset)
          (setq next-error-function :new)
          (flycheck-mode -1)
          (expect flycheck-mode :not :to-be-truthy)
          ;; Disabling the mode should not affect `next-error-function' now
          (expect next-error-function :to-be :new))))

    (it "clears-errors-after-revert"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (goto-char (point-min))
        (insert "foo-bar")
        (flycheck-mode)
        (flycheck-buttercup-buffer-sync)
        (expect flycheck-current-errors :to-be-truthy)
        (let ((hack-local-variables-hook))
          (revert-buffer 'ignore-auto 'no-confirm))
        (expect flycheck-current-errors :not :to-be-truthy)
        (expect (flycheck-deferred-check-p) :not :to-be-truthy)))))

(describe "Syntax Checker Selection"

  (describe "flycheck-checker"
    (it "unusable-checker-causes-an-error"
      (flycheck-buttercup-with-temp-buffer
        (emacs-lisp-mode)
        (flycheck-mode)
        (let* ((flycheck-checker 'sh-bash))
          (flycheck-buffer)
          (expect flycheck-checker :to-be 'sh-bash)
          (expect flycheck-last-status-change :to-equal 'no-checker))))

    (it "usable-checker-is-used"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (expect (flycheck-get-checker-for-buffer) :to-be 'emacs-lisp)
        (let ((flycheck-checker 'emacs-lisp-checkdoc))
          (expect (flycheck-get-checker-for-buffer) :to-be 'emacs-lisp-checkdoc)
          (flycheck-buttercup-buffer-sync)
          (flycheck-buttercup-should-errors
           '(12 nil info "First sentence should end with punctuation"
                :checker emacs-lisp-checkdoc)))))

    (it "disabled-checker-is-not-used"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (let ((flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
          (expect (flycheck-get-checker-for-buffer) :not :to-be-truthy)
          (let* ((flycheck-checker 'emacs-lisp))
            (flycheck-buffer)
            (expect flycheck-checker :to-be 'emacs-lisp)
            (expect flycheck-last-status-change :to-equal 'no-checker)))))

    (it "unregistered-checker-is-used"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (expect (flycheck-get-checker-for-buffer) :to-be 'emacs-lisp)
        (let ((flycheck-checkers (remq 'emacs-lisp-checkdoc flycheck-checkers)))
          (expect (flycheck-registered-checker-p 'emacs-lisp-checkdoc)
                  :not :to-be-truthy)
          (let ((flycheck-checker 'emacs-lisp-checkdoc))
            (expect (flycheck-get-checker-for-buffer) :to-be 'emacs-lisp-checkdoc)
            (flycheck-buttercup-buffer-sync)
            (flycheck-buttercup-should-errors
             '(12 nil info "First sentence should end with punctuation"
                  :checker emacs-lisp-checkdoc)))))))

  (describe "flycheck-select-checker"
    (it "selecting-sets-the-syntax-checker"
      (flycheck-buttercup-with-temp-buffer
        (emacs-lisp-mode)
        (flycheck-select-checker 'emacs-lisp-checkdoc)
        (expect flycheck-checker :to-be 'emacs-lisp-checkdoc)))

    (it "unselecting-unsets-the-syntax-checker"
      (flycheck-buttercup-with-temp-buffer
        (emacs-lisp-mode)
        (flycheck-select-checker 'emacs-lisp-checkdoc)
        (flycheck-select-checker nil)
        (expect flycheck-checker :not :to-be-truthy)))

    (it "selecting-runs-a-syntax-check"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        ;; By default we should get both, because emacs-lisp chains to checkdoc
        (flycheck-buttercup-buffer-sync)
        (dolist (err flycheck-current-errors)
          (expect (memq (flycheck-error-checker err)
                        '(emacs-lisp emacs-lisp-checkdoc))
                  :to-be-truthy))
        ;; We select checkdoc, and should now only have checkdoc errors
        (flycheck-select-checker 'emacs-lisp-checkdoc)
        (flycheck-buttercup-wait-for-syntax-checker)
        (dolist (err flycheck-current-errors)
          (expect (flycheck-error-checker err) :to-be 'emacs-lisp-checkdoc))))

    (it "unselecting-a-checker-goes-back-to-automatic-selection"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (emacs-lisp-mode)
        (flycheck-mode)
        (flycheck-select-checker 'emacs-lisp-checkdoc)
        (expect flycheck-checker :to-be 'emacs-lisp-checkdoc)
        (flycheck-buttercup-wait-for-syntax-checker)
        (dolist (err flycheck-current-errors)
          (expect (flycheck-error-checker err) :to-be 'emacs-lisp-checkdoc))
        (flycheck-select-checker nil)
        (expect flycheck-checker :not :to-be-truthy)
        (flycheck-buttercup-wait-for-syntax-checker)
        (dolist (err flycheck-current-errors)
          (expect (memq (flycheck-error-checker err)
                        '(emacs-lisp emacs-lisp-checkdoc))
                  :to-be-truthy)))))

  (describe "flycheck/selects-checker-automatically"
    (it "no-disabled-checker"
      (flycheck-buttercup-with-resource-buffer "language/emacs-lisp/warnings.el"
        (let ((flycheck-disabled-checkers '(emacs-lisp)))
          (flycheck-buttercup-buffer-sync)
          (expect flycheck-checker :not :to-be-truthy)
          (flycheck-buttercup-should-errors
           '(12 nil info "First sentence should end with punctuation"
                :checker emacs-lisp-checkdoc))))))

  (describe "flycheck-disable-checker"
    (it "disables-checker"
      (flycheck-buttercup-with-temp-buffer
        (flycheck-mode)
        (with-no-warnings
          (flycheck-disable-checker 'emacs-lisp))
        (expect flycheck-disabled-checkers :to-equal '(emacs-lisp))
        (expect (default-value 'flycheck-disabled-checkers) :not :to-be-truthy)
        ;; Disabling a disabled checker should be a no-op
        (with-no-warnings
          (flycheck-disable-checker 'emacs-lisp))
        (expect flycheck-disabled-checkers :to-equal '(emacs-lisp))))

    (it "enables-checker"
      (flycheck-buttercup-with-temp-buffer
        (flycheck-mode)
        (setq flycheck-disabled-checkers '(emacs-lisp python-pylint))
        (with-no-warnings
          (flycheck-disable-checker 'emacs-lisp 'enable))
        (expect flycheck-disabled-checkers :to-equal '(python-pylint))))))

;;; test-flycheck-mode.el ends here
