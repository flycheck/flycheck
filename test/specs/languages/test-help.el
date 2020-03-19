;;; test-help.el --- Flycheck Specs: Syntax checker help  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Flycheck contributors
;; Copyright (C) 2016  Sebastian Wiesner

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

;; Specs for `flycheck-describe-checker' and related functions.

;;; Code:

(require 'flycheck-buttercup)

(describe "Syntax checker Help"
  (describe "flycheck-describe-checker"
    (let ((checker 'ruby-rubocop))
      (before-each
        (let ((inhibit-message t))
          (flycheck-describe-checker checker)))

      (after-each
        (when (buffer-live-p (get-buffer (help-buffer)))
          (kill-buffer (help-buffer))))

      (it (format "pops up a help buffer for %s" checker)
        (expect (help-buffer) :to-be-live)
        (expect (help-buffer) :to-be-visible))

      (it (format "documents %s in the help buffer" checker)
        (expect (help-buffer) :to-contain-match
                (regexp-quote (symbol-name checker))))

      (it (format "navigates to the source of %s" checker)
        (with-current-buffer (help-buffer)
          ;; Search for the button…
          (re-search-forward (rx "`" (group (1+ (not (any "'")))) "'"))
          (expect (match-string 1) :to-equal "flycheck.el")
          ;; …and push it.  We disable prompts about unsafe local variables and
          ;; disable a bunch of hooks to make the underlying call to `find-file'
          ;; as painless as possible.
          (let ((enable-local-variables :safe)
                hack-local-variables-hook
                prog-mode-hook
                emacs-lisp-mode-hook)
            (push-button (match-beginning 1)))
          (unwind-protect
              (progn
                (expect (buffer-name) :to-equal "flycheck.el")
                (expect (looking-at (rx bol
                                        (or "(flycheck-define-checker"
                                            "(flycheck-define-command-checker")
                                        symbol-end
                                        " " (? "'")
                                        symbol-start
                                        (group (1+ (or (syntax word)
                                                       (syntax symbol))))
                                        symbol-end))
                        :to-be-truthy)
                (expect (match-string 1) :to-equal (symbol-name checker)))
            ;; Kill the Flycheck buffer again
            (kill-buffer))))

      (it (format "shows next checkers in the help of %s" checker)
        (let ((next-checkers (flycheck-checker-get checker 'next-checkers)))
          (assume next-checkers (format "Syntax checker %S has no next checkers"
                                        checker))
          (dolist (next-checker next-checkers)
            (pcase next-checker
              (`(,level . ,checker)
               (expect (help-buffer)
                       :to-contain-match
                       (rx-to-string `(and "`" ,(symbol-name checker)
                                           "' (maximum level `"
                                           ,(symbol-name level)"')"))))
              ((pred symbolp)
               (expect (help-buffer)
                       :to-contain-match
                       (rx-to-string `(and "`"
                                           ,(symbol-name next-checker)
                                           "'"))))))))

      (it (format "shows the executable name in the help of %s" checker)
        (let ((executable (flycheck-checker-default-executable checker)))
          (expect (help-buffer)
                  :to-contain-match
                  (rx-to-string `(and "\"" ,executable "\"")))))

      (it (format "shows the executable variable in the help of %s" checker)
        (let ((variable (flycheck-checker-executable-variable checker)))
          (expect (help-buffer)
                  :to-contain-match
                  (regexp-quote (symbol-name variable)))))

      (it (format "shows the configuration file variable in the help of %s"
                  checker)
        (let ((variable (flycheck-checker-get checker 'config-file-var)))
          (assume variable (format "Syntax checker %s has no configuration file"
                                   checker))
          (expect (help-buffer)
                  :to-contain-match
                  (regexp-quote (symbol-name variable)))))

      (it (format "shows the option variables in the help of %s" checker)
        (let ((option-vars (flycheck-checker-get checker 'option-vars)))
          (assume option-vars (format "Syntax checker %s has no options"
                                      checker))
          (dolist (variable option-vars)
            (expect (help-buffer)
                    :to-contain-match
                    (rx-to-string `(and "* `"
                                        ,(symbol-name variable)
                                        "'" eol))))))

      (it (format "shows the docstring of %s" checker)
        (let ((docstring (flycheck-checker-get checker 'documentation)))
          (expect (help-buffer) :to-contain-match (regexp-quote docstring)))))))

;;; test-help.el ends here
