;;; test-global-mode.el --- Flycheck Specs: Global Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for Global Flycheck Mode.

;;; Code:

(require 'flycheck-buttercup)

(describe "Global Flycheck Mode"

  (let (global-mode-enabled global-modes)

    ;; Save and restore the configuration for Global Flycheck Mode
    (before-all
      (setq global-mode-enabled global-flycheck-mode
            global-modes flycheck-global-modes))

    (after-all
      (global-flycheck-mode (if global-mode-enabled 1 -1))
      (setq flycheck-global-modes global-modes))

    (before-each
      ;; Make sure that Global Flycheck Mode is enabled and all modes are
      ;; permitted in each spec
      (global-flycheck-mode 1)
      (setq flycheck-global-modes t))

    (it "checks whether it may enable the mode"
      (let (flycheck-may-enable-mode)
        (spy-on 'flycheck-may-enable-mode :and-return-value t)

        (with-temp-buffer
          (emacs-lisp-mode)
          (expect flycheck-mode :to-be-truthy)
          (expect 'flycheck-may-enable-mode :to-have-been-called))))

    (it "enables Flycheck in a regular buffer"
      (with-temp-buffer
        (rename-buffer "foo")
        (emacs-lisp-mode)
        (expect (flycheck-may-enable-mode) :to-be-truthy)))

    (it "enables Flycheck when only that major mode is allowed"
      (let ((flycheck-global-modes '(emacs-lisp-mode)))
        (with-temp-buffer
          (rename-buffer "foo")
          (emacs-lisp-mode)
          (expect (flycheck-may-enable-mode) :to-be-truthy))))

    (it "does not enable Flycheck in temporary buffers"
      (with-temp-buffer
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)))

    (it "does not enable Flycheck in ephemeral buffers"
      (with-temp-buffer
        (rename-buffer " foo")
        (emacs-lisp-mode)
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)))

    (it "does not enable Flycheck in special modes"
      (with-temp-buffer
        (rename-buffer "foo")
        (special-mode)
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)))

    (it "does not enable Flycheck in fundamental-mode"
      (with-temp-buffer
        (rename-buffer "foo")
        (fundamental-mode)
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)))

    (it "does not enable Flycheck in encrypted buffers"
      (spy-on 'flycheck-encrypted-buffer-p :and-return-value t)
      (with-temp-buffer
        (rename-buffer "foo")
        (emacs-lisp-mode)
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)
        (expect 'flycheck-encrypted-buffer-p :to-have-been-called)))

    (it "does not enable Flycheck in compilation mode"
      (with-temp-buffer
        (rename-buffer "foo")
        (compilation-mode)
        (expect (flycheck-may-enable-mode) :not :to-be-truthy)))

    (it "does not enable Flycheck if all major modes are disabled"
      (let ((flycheck-global-modes nil))
        (with-temp-buffer
          (rename-buffer "foo")
          (emacs-lisp-mode)
          (expect (flycheck-may-enable-mode) :not :to-be-truthy))))

    (it "does not enable Flycheck if the major mode is disabled"
      (let ((flycheck-global-modes '(not emacs-lisp-mode)))
        (with-temp-buffer
          (rename-buffer "foo")
          (emacs-lisp-mode)
          (expect (flycheck-may-enable-mode) :not :to-be-truthy))))

    (it "does not enable Flycheck if the major mode is not enabled"
      (let ((flycheck-global-modes '(text-mode)))
        (with-temp-buffer
          (rename-buffer "foo")
          (emacs-lisp-mode)
          (expect (flycheck-may-enable-mode) :not :to-be-truthy))))))

;;; test-global-mode.el ends here
