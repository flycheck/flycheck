;;; init.el --- Flycheck: Init file for testing      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Use with emacs -Q -l test/init.el to start a clean session with only Flycheck
;; present.
;;
;; Installs all dependencies of Flycheck to a temporary package directory, and
;; loads Flycheck.

;;; Code:

(require 'package)

(setq load-prefer-newer t)              ; Don't load outdated bytecode

(defvar temporary-package-directory
  (make-temp-file "flycheck-package-dir-" 'directory))
(add-hook 'kill-emacs-hook
          (lambda ()
            (delete-directory temporary-package-directory 'recursive)))

(setq package-user-dir temporary-package-directory
      package-check-signature nil)
(add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)
(package-install 'flycheck)             ; Install to bring dependencies in

(let* ((source-dir (locate-dominating-file load-file-name "flycheck.el")))
  (load (expand-file-name "flycheck.el" source-dir)))

(require 'flycheck)
(global-flycheck-mode)

;;; init.el ends here
