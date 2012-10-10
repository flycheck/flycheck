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
    (flymake-log 1 "Shell script checking needs sh-mode.")
    nil))


;; Register checkers in flymake

;;;###autoload
(defconst flymake-checkers-file-name-masks
  '(("\\.sh\\'" flymake-checkers-sh-init)
    ("\\.zsh\\'" flymake-checkers-sh-init)
    ("\\.bash\\'" flymake-checkers-sh-init)
    ("\\.[lL]a[tT]e[xX]\\'" flymake-checkers-tex-init)
    ("\\.[tT]e[xX]\\'" flymake-checkers-tex-init))
  "All checkers provided by flymake-checkers with corresponding.

Automatically added to `flymake-allowed-file-name-masks' when
this package is loaded.")

;;;###autoload
(defconst flymake-checkers-err-line-patterns
  '(("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3) ; bash
    ("^\\(.+\\): ?\\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
  "Additional error line patterns.

Automatically added to `flymake-err-line-patterns' when this
package is loaded.")

;;;###autoload
(eval-after-load 'flymake
  #'(mapc (lambda (cell) (add-to-list 'flymake-allowed-file-name-masks cell))
          flymake-checkers-file-name-masks))

;;;###autoload
(eval-after-load 'flymake
  #'(setq flymake-err-line-patterns (append flymake-checkers-err-line-patterns
                                            flymake-err-line-patterns)))

(provide 'flymake-checkers)

;;; flymake-checkers.el ends here
