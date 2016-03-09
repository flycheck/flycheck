;;; init.el --- Flycheck: Init file for testing      -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://www.flycheck.org

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

;; Use with `emacs -Q -l test/init.el' to start a clean session with only
;; Flycheck present.
;;
;; Installs all dependencies of Flycheck to a temporary package directory, and
;; loads Flycheck.

;;; Code:

(require 'package)

(setq load-prefer-newer t)              ; Don't load outdated bytecode

(setq package-user-dir (expand-file-name "init-elpa"
                                         (file-name-directory load-file-name))
      package-check-signature nil)
(add-to-list 'package-archives '("MELPA" . "https://stable.melpa.org/packages/"))

(package-initialize)

(let* ((source-dir (locate-dominating-file load-file-name "flycheck.el"))
       (flycheck-el (expand-file-name "flycheck.el" source-dir)))
  ;; Install Flycheck to bring its dependencies in
  (unless (package-installed-p 'flycheck)
    (package-refresh-contents)
    (package-install-file flycheck-el))
  (load flycheck-el))

(require 'flycheck)
(global-flycheck-mode)

;; Some little convenience, to this Emacs session at least half way bearable
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Get rid of all the silly UI clutter of a default Emacs session and opt out of
;; all the stupid startup and license messages
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-scratch-message "")
(fset 'yes-or-no-p #'y-or-n-p)
(fset 'display-startup-echo-area-message #'ignore)

;; Improve OS X key behaviour
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta       ; Option is simply the natural Meta
        mac-command-modifier 'meta      ; But command is a lot easier to hit
        mac-right-command-modifier 'left
        mac-right-option-modifier 'none ; Keep right option for accented input
        mac-function-modifier 'hyper))

(defun flycheck-prepare-screenshot (&optional hide-cursor)
  "Prepare this Emacs session for a screenshot.

If HIDE-CURSOR is non-nil, or with prefix arg, hide the cursor,
otherwise keep it."
  (interactive "P")
  ;; Reduce UI and disable cursor
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq-default truncate-lines t)
  (when hide-cursor
    (setq-default cursor-type nil))

  (set-frame-font "Source Code Pro-13")
  (set-frame-size (selected-frame) 750 560 'pixelwise)
  ;; Tuck the error list to the bottom side window at a fixed height
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*")
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

;;; init.el ends here
