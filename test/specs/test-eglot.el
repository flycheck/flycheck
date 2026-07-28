;;; test-eglot.el --- Flycheck Specs: Eglot integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;;; Commentary:

;; Specs for the built-in Eglot bridge (`flycheck-eglot-mode' and the
;; `eglot-check' checker).  The diagnostic conversion is tested against
;; synthetic Flymake diagnostics; the mode wiring stubs Eglot.

;;; Code:

(require 'flycheck-buttercup)
(require 'flymake)

(defun test-eglot--diag (buffer beg end type text &optional lsp)
  "Build a Flymake diagnostic, stashing LSP as Eglot's data payload."
  (flymake-make-diagnostic buffer beg end type text
                           (and lsp (list (cons 'eglot-lsp-diag lsp)))))

(describe "Eglot integration"

  (describe "flycheck-eglot--convert-diagnostic"
    (it "reads level, message and id from the LSP diagnostic in the data slot"
      (flycheck-buttercup-with-temp-buffer
        (insert "line one here\n")
        (let* ((diag (test-eglot--diag
                      (current-buffer) 1 5 :warning "flymake text"
                      '(:severity 2 :message "unused" :code "F841")))
               (err (flycheck-eglot--convert-diagnostic diag)))
          (expect (flycheck-error-level err) :to-be 'warning)
          (expect (flycheck-error-message err) :to-equal "unused")
          (expect (substring-no-properties (flycheck-error-id err))
                  :to-equal "F841")
          (expect (flycheck-error-checker err) :to-be 'eglot-check))))

    (it "falls back to the Flymake fields when there is no data payload"
      (flycheck-buttercup-with-temp-buffer
        (insert "line one here\n")
        (let* ((diag (test-eglot--diag (current-buffer) 1 5 :error "raw"))
               (err (flycheck-eglot--convert-diagnostic diag)))
          (expect (flycheck-error-level err) :to-be 'error)
          (expect (flycheck-error-message err) :to-equal "raw")
          (expect (flycheck-error-id err) :to-be nil))))

    (it "maps the diagnostic's relatedInformation to related locations"
      (flycheck-buttercup-with-temp-buffer
        (insert "line one here\n")
        (let* ((diag (test-eglot--diag
                      (current-buffer) 1 5 :error "redefined"
                      '(:severity 1 :message "redefined"
                        :relatedInformation
                        [(:location (:uri "other.el"
                                     :range (:start (:line 1 :character 4)
                                             :end (:line 1 :character 9)))
                          :message "first defined here")])))
               (err (flycheck-eglot--convert-diagnostic diag))
               (rel (car (flycheck-error-relations err))))
          (expect (length (flycheck-error-relations err)) :to-equal 1)
          (expect (flycheck-related-location-filename rel) :to-equal "other.el")
          (expect (flycheck-related-location-line rel) :to-equal 2)
          (expect (flycheck-related-location-column rel) :to-equal 5)
          (expect (flycheck-related-location-end-column rel) :to-equal 10)
          (expect (flycheck-related-location-message rel)
                  :to-equal "first defined here")))))

  (describe "flycheck-eglot--report"
    (it "caches the diagnostics and re-triggers a check"
      (flycheck-buttercup-with-temp-buffer
        (spy-on 'flycheck-buffer-automatically)
        (let ((diags (list (test-eglot--diag (current-buffer) 1 2 :error "x"))))
          (flycheck-eglot--report diags)
          (expect flycheck-eglot--diagnostics :to-equal diags)
          (expect 'flycheck-buffer-automatically :to-have-been-called))))
    (it "does not re-trigger while suppressed"
      (flycheck-buttercup-with-temp-buffer
        (spy-on 'flycheck-buffer-automatically)
        (let ((flycheck-eglot--suppress-recheck t))
          (flycheck-eglot--report nil))
        (expect 'flycheck-buffer-automatically :not :to-have-been-called))))

  (describe "flycheck-eglot--flymake-diagnostics"
    (it "serves the cached diagnostics that overlap the query range"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdefghij\n")
        (let* ((flycheck-eglot-mode t)
               (wide (test-eglot--diag (current-buffer) 1 8 :error "x"))
               (flycheck-eglot--diagnostics (list wide)))
          ;; a narrow query inside a wider diagnostic must still find it
          (expect (flycheck-eglot--flymake-diagnostics #'ignore 3 4)
                  :to-equal (list wide)))))
    (it "delegates to the original when the mode is off"
      (let ((flycheck-eglot-mode nil))
        (expect (flycheck-eglot--flymake-diagnostics
                 (lambda (&rest _) 'delegated) 1 2)
                :to-be 'delegated))))

  (describe "code-action fixes"

    (it "provides the code-action fix only when enabled and supported"
      (let ((flycheck-eglot-code-actions t))
        (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t)))
          (expect (flycheck-eglot--fix-provider)
                  :to-be 'flycheck-eglot--code-action-fix))
        (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) nil)))
          (expect (flycheck-eglot--fix-provider) :to-be nil)))
      (let ((flycheck-eglot-code-actions nil))
        (expect (flycheck-eglot--fix-provider) :to-be nil)))

    (it "resolves the preferred quickfix action into a fix"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (insert "line one\n")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-uri-to-path) #'identity)
                  ((symbol-function 'flycheck-same-files-p) #'equal)
                  ((symbol-function 'eglot-code-actions)
                   (lambda (&rest _)
                     (list '(:title "Meh")
                           '(:title "Quick fix" :isPreferred t
                             :edit (:documentChanges
                                    [(:textDocument (:uri "/proj/a.el")
                                      :edits [(:range (:start (:line 0 :character 0)
                                                       :end (:line 0 :character 4))
                                               :newText "LINE")])]))))))
          (let ((fix (flycheck-eglot--code-action-fix
                      (flycheck-error-new-at 1 1 'error "x"
                                             :buffer (current-buffer)))))
            (expect (flycheck-fix-p fix) :to-be t)
            (expect (flycheck-fix-description fix) :to-equal "Quick fix")))))

    (it "does not treat isPreferred `:json-false' as preferred"
      ;; jsonrpc decodes JSON `false' to `:json-false', which is truthy; the
      ;; first action here carries it and must not be mistaken for preferred.
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (insert "line one\n")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-uri-to-path) #'identity)
                  ((symbol-function 'flycheck-same-files-p) #'equal)
                  ((symbol-function 'eglot-code-actions)
                   (lambda (&rest _)
                     (list '(:title "Not preferred" :isPreferred :json-false
                             :edit (:documentChanges
                                    [(:textDocument (:uri "/proj/a.el")
                                      :edits [(:range (:start (:line 0 :character 0)
                                                       :end (:line 0 :character 1))
                                               :newText "X")])]))
                           '(:title "The real one" :isPreferred t
                             :edit (:documentChanges
                                    [(:textDocument (:uri "/proj/a.el")
                                      :edits [(:range (:start (:line 0 :character 0)
                                                       :end (:line 0 :character 4))
                                               :newText "LINE")])]))))))
          (let ((fix (flycheck-eglot--code-action-fix
                      (flycheck-error-new-at 1 1 'error "x"
                                             :buffer (current-buffer)))))
            (expect (flycheck-fix-description fix) :to-equal "The real one")))))

    (it "returns no fix when the server offers no actions"
      (flycheck-buttercup-with-temp-buffer
        (insert "x\n")
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-code-actions) (lambda (&rest _) nil)))
          (expect (flycheck-eglot--code-action-fix
                   (flycheck-error-new-at 1 1 'error "x"))
                  :to-be nil)))))

  (describe "the eglot-check checker"
    (it "is a valid generic checker"
      (expect (flycheck-valid-checker-p 'eglot-check) :to-be-truthy))
    (it "is defined for prog-mode and text-mode"
      (expect (flycheck-checker-get 'eglot-check 'modes)
              :to-equal '(prog-mode text-mode))))

  (describe "flycheck-eglot-mode"
    ;; Stub Eglot with `cl-letf' rather than `spy-on': Emacs 28 does not bundle
    ;; Eglot, so the symbols may be unbound, and `cl-letf' can still define them.
    (it "selects eglot-check when enabled and clears it when disabled"
      (flycheck-buttercup-with-temp-buffer
        (text-mode)                     ; a mode eglot-check already supports
        (let ((flycheck-checkers (copy-sequence flycheck-checkers))
              (backend-calls 0))
          (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                    ((symbol-function 'eglot-flymake-backend)
                     (lambda (&rest _) (cl-incf backend-calls)))
                    ((symbol-function 'flycheck-mode) #'ignore)
                    ((symbol-function 'flymake-mode) #'ignore)
                    ((symbol-function 'flycheck-buffer-deferred) #'ignore))
            (flycheck-eglot-mode 1)
            (expect flycheck-checker :to-be 'eglot-check)
            (expect backend-calls :to-be-greater-than 0)
            (flycheck-eglot-mode -1)
            (expect flycheck-checker :to-be nil)
            (expect flycheck-eglot--diagnostics :to-be nil))
          (advice-remove 'flymake-diagnostics
                         #'flycheck-eglot--flymake-diagnostics))))))

;;; test-eglot.el ends here
