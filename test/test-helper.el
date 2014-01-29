;;; test-helper.el --- Flycheck: Initialize test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Sebastian Wiesner

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

;; Initializes the test suite for use with `ert-runner'.
;;
;; Also serves as init file for interactive test running.

;;; Code:

(let* ((current-file (if load-in-progress load-file-name (buffer-file-name)))
       (source-directory (locate-dominating-file current-file "Cask"))
       (pkg-rel-dir (format ".cask/%s/elpa" emacs-version)))
  (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
  (package-initialize)

  (load (expand-file-name "flycheck" source-directory)))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; test-helper.el ends here
