;;; init.el --- Flycheck: Initialize test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

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

;; Init file for Flycheck's test suite.
;;
;; Initialize the package system.
;;
;; Also serves as init file for interactive test in an uncluttered environment.

;;; Code:

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       (pkg-rel-dir (format ".cask/%s/elpa" emacs-version)))
  (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
  (package-initialize))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
