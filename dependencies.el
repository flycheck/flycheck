;;; dependencies.el --- Install and load development dependencies

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

(defconst source-dir (file-name-directory load-file-name)
  "The source directory.")

;; Enable installation of dependencies
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Install packages into the source directory (not into ~/.emacs.d!)
(setq package-user-dir (expand-file-name "elpa") source-dir)
;; Initialize the package system and fetch the archive contents
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun package-need (package)
  "Require PACKAGE to be installed."
  (unless (package-installed-p package)
    (package-install package)))

(package-need 'dash)
(package-need 's)

(require 'dash)
(require 's)

;;; dependencies.el ends here
