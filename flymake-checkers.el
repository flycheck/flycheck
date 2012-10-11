;;; flymake-checkers.el --- Additional syntax checkers for flymake
;;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (c) 2012 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flymake-checkers
;; Version: 0.1
;; Keywords: convenience languages tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Additional checkers for `flymake-mode'.

;; Provide checkers for:
;;
;; - TeX/LaTeX with chktex
;; - Shell scripts (only with `sh-mode')
;; - Python wither either flake8, pyflakes or pylint

;;; Code:

(eval-when-compile
  (require 'flymake)
  (require 'sh-script))

;; Customization group
(defgroup flymake-checkers nil
  "Customization for flymake checkers."
  :prefix "flymake-checkers-"
  :group 'flymake)

;; Utility functions

(defun flymake-checkers-create-temp-system (filename prefix)
  "Create a copy of FILENAME with PREFIX in temp directory.

Return the path of the file."
  (make-temp-file (or prefix "flymake-checkers") nil
                  (concat "." (file-name-extension filename))))

;; TeX/LaTeX
;;;###autoload
(defun flymake-checkers-tex-init ()
  "Initialize flymake checking for TeX."
  `("chktex" ("-v0" "-q" "-I" ,(flymake-init-create-temp-buffer-copy
                                'flymake-create-temp-inplace))))

;; sh-mode

(defconst flymake-checkers-sh-supported-shells '(bash zsh sh)
  "Shells supported by `flymake-checkers-sh-init'.")

;;;###autoload
(defun flymake-checkers-sh-init ()
  "Initialize flymake checking for `sh-mode'."
  (if (boundp 'sh-shell)
      (if (memq sh-shell flymake-checkers-sh-supported-shells)
          `(,(symbol-name sh-shell)
            ("-n" ,(flymake-init-create-temp-buffer-copy
                    'flymake-checkers-create-temp-system)))
        (flymake-log 1 "Shell %s is not supported." sh-shell)
        nil)
    (flymake-log 0 "Shell script checking needs sh-mode.")
    nil))


;; Python

(defcustom flymake-checkers-python-checker nil
  "Checker to use for Python files.

Set to `flake8', `pyflakes' or `epylint' to use the corresponding
checker for Python files.  If nil, syntax checking of Python
files is disabled."
  :group 'flymake-checkers
  :type '(choice (const :tag "Off" nil)
                 (const flake8)
                 (const pyflakes)
                 (const epylint)
                 (string :tag "Custom checker")))

(defconst flymake-checkers-python-supported-checkers
  '((flake8 . "flake8")
    (pyflakes . "pyflakes")
    (epylint . "epylint"))
  "Supported Python checkers.")

(defun flymake-checkers-python-get-checker ()
  "Get the checker to use for Python.

Return a string with the executable path of the checker, or nil
if the checker was not found."
  (let ((checker flymake-checkers-python-checker))
    (when checker
      (when (symbolp checker)
        (setq checker
              (cdr (assq checker flymake-checkers-python-supported-checkers))))
      (if (stringp checker) checker
        (error "Invalid value for flymake-checkers-python-checker: %S"
               flymake-checkers-python-checker)))))

;;;###autoload
(defun flymake-checkers-python-init ()
  "Initialize flymake checking for Python files."
  (let ((checker (flymake-checkers-python-get-checker)))
    (when checker
      (flymake-log 3 "Using python checkers %s." checker)
      `(,checker (,(flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace))))))


;; Register checkers in flymake

;;;###autoload
(defconst flymake-checkers-file-name-masks
  '(("\\.sh\\'" flymake-checkers-sh-init)
    ("\\.zsh\\'" flymake-checkers-sh-init)
    ("\\.bash\\'" flymake-checkers-sh-init)
    ("\\.[lL]a[tT]e[xX]\\'" flymake-checkers-tex-init)
    ("\\.[tT]e[xX]\\'" flymake-checkers-tex-init)
    ("\\.py\\'" flymake-checkers-python-init))
  "All checkers provided by flymake-checkers with corresponding.

Automatically added to `flymake-allowed-file-name-masks' when
this package is loaded.")

;;;###autoload
(eval-after-load 'flymake
  #'(mapc (lambda (cell) (add-to-list 'flymake-allowed-file-name-masks cell))
          flymake-checkers-file-name-masks))

(provide 'flymake-checkers)

;;; flymake-checkers.el ends here
