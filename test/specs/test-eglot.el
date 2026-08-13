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

(require 'cl-lib)
(require 'flycheck-buttercup)
(require 'flymake)

(defun test-eglot--diag (buffer beg end type text &optional lsp)
  "Build a Flymake diagnostic, stashing LSP as Eglot's data payload."
  (flymake-make-diagnostic buffer beg end type text
                           (and lsp (list (cons 'eglot-lsp-diag lsp)))))

(defun test-eglot--settle ()
  "Let the timer that ends a run of Eglot reports run.
The bridge takes the reports arriving now as one answer, so nothing
reaches the buffer until they stop; a spec that skips this asserts on a
half-assembled answer and passes for the wrong reason."
  (let ((limit 200))
    (while (and flycheck-eglot--settle-timer (> limit 0))
      (cl-decf limit)
      (sit-for 0.001))))

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

  (describe "project diagnostics for unvisited files"

    ;; Eglot parks a push about a file no buffer visits in Flymake's
    ;; `flymake-list-only-diagnostics', as (LINE . COLUMN) file diagnostics
    ;; with no LSP payload; the bridge folds them into the project view.

    (cl-flet ((parked (file line col type text)
                (cons file (list (flymake-make-diagnostic
                                  file (cons line col) nil type text)))))

      (let ((project (file-name-as-directory (expand-file-name "/proj"))))

        (it "converts a parked diagnostic to an error of the unvisited file"
          (let* ((file (expand-file-name "box/box.go" project))
                 (flymake-list-only-diagnostics
                  (list (parked file 28 2 'eglot-error
                                "compiler [UndeclaredName]: undefined: a"))))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-eglot-mode t)
              (expect (flycheck-eglot--project-extra-errors project nil)
                      :to-be-equal-flycheck-errors
                      (list (flycheck-error-new-at
                             28 2 'error
                             "compiler [UndeclaredName]: undefined: a"
                             :checker 'eglot-check
                             :filename file :buffer nil))))))

        (it "maps the flymake types onto Flycheck levels"
          (let ((flymake-list-only-diagnostics
                 (list (parked (expand-file-name "w" project) 1 1
                               'eglot-warning "w")
                       (parked (expand-file-name "n" project) 2 1
                               'eglot-note "n"))))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-eglot-mode t)
              (expect (mapcar #'flycheck-error-level
                              (flycheck-eglot--project-extra-errors project nil))
                      :to-equal '(warning info)))))

        (it "skips a parked file outside the project"
          (let ((flymake-list-only-diagnostics
                 (list (parked (expand-file-name "/elsewhere/x") 1 1
                               'eglot-error "e"))))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-eglot-mode t)
              (expect (flycheck-eglot--project-extra-errors project nil)
                      :to-be nil))))

        (it "skips a parked file that has a buffer by now"
          (let* ((file (expand-file-name "opened.go" project))
                 (flymake-list-only-diagnostics
                  (list (parked file 1 1 'eglot-error "e")))
                 (visiting (generate-new-buffer " opened")))
            (unwind-protect
                (progn
                  (with-current-buffer visiting
                    (set-visited-file-name file 'no-query))
                  (flycheck-buttercup-with-temp-buffer
                    (setq-local flycheck-eglot-mode t)
                    (expect (flycheck-eglot--project-extra-errors project nil)
                            :to-be nil)))
              (with-current-buffer visiting
                (set-buffer-modified-p nil)
                (kill-buffer)))))

        (it "contributes nothing while the bridge is off everywhere"
          (let ((flymake-list-only-diagnostics
                 (list (parked (expand-file-name "x" project) 1 1
                               'eglot-error "e"))))
            (flycheck-buttercup-with-temp-buffer
              (expect (flycheck-eglot--project-extra-errors project nil)
                      :to-be nil))))

        (it "accepts the bridge being on in another buffer of the project"
          (let ((flymake-list-only-diagnostics
                 (list (parked (expand-file-name "x" project) 1 1
                               'eglot-error "e")))
                (other (generate-new-buffer " bridged")))
            (unwind-protect
                (progn
                  (with-current-buffer other
                    (setq-local flycheck-eglot-mode t))
                  (flycheck-buttercup-with-temp-buffer
                    (expect (length (flycheck-eglot--project-extra-errors
                                     project (list other)))
                            :to-equal 1)))
              (kill-buffer other)))))))

  (describe "flycheck-eglot--report"
    (it "publishes what the server reported, once the reports stop"
      (flycheck-buttercup-with-temp-buffer
        (spy-on 'flycheck-buffer-automatically)
        (let ((diags (list (test-eglot--diag (current-buffer) 1 2 :error "x"))))
          (flycheck-eglot--report diags)
          ;; nothing yet: more reports may still belong to this answer
          (expect flycheck-eglot--diagnostics :to-be nil)
          (test-eglot--settle)
          (expect flycheck-eglot--diagnostics :to-equal diags)
          (expect 'flycheck-buffer-automatically :to-have-been-called))))

    (it "does not check again for a report of what it already shows"
      ;; A server republishes an unchanged set freely while it indexes
      (flycheck-buttercup-with-temp-buffer
        (spy-on 'flycheck-buffer-automatically)
        (let ((diags (list (test-eglot--diag (current-buffer) 1 2 :error "x"))))
          (setq flycheck-eglot--diagnostics diags)
          (flycheck-eglot--report diags)
          (test-eglot--settle)
          (expect 'flycheck-buffer-automatically :not :to-have-been-called))))

    (it "keeps the diagnostics a report does not account for"
      ;; Eglot answers with the pulled diagnostics for the whole buffer and
      ;; then the pushed ones for an empty region, meaning add these and
      ;; delete nothing.  Both belong to the buffer.
      (flycheck-buttercup-with-temp-buffer
        (insert "one\ntwo\n")
        (spy-on 'flycheck-buffer-automatically)
        (let ((pulled (list (test-eglot--diag (current-buffer) 1 2 :error "a")))
              (pushed (list (test-eglot--diag (current-buffer) 5 6 :error "b"))))
          (flycheck-eglot--report
           pulled :region (cons (point-min) (point-max)))
          (flycheck-eglot--report
           pushed :region (cons (point-min) (point-min)))
          (test-eglot--settle)
          (expect flycheck-eglot--diagnostics
                  :to-equal (append pulled pushed)))))

    (it "does not loop on an answer that arrives after the request returned"
      ;; Under the pull model of LSP 3.17, asking Eglot sends
      ;; `textDocument/diagnostic' and returns; the answer lands long after
      ;; any dynamic binding has unwound, and takes two reports.  Treating
      ;; the second as one the server volunteered started a check, which
      ;; asked again, at a hundred requests a second.  See #2291.
      (flycheck-buttercup-with-temp-buffer
        (let ((answer nil) (requests 0) (checks 0)
              ;; what rust-analyzer answers with a cargo error live: the
              ;; pulled set empty, the pushed set carrying the error
              (pushed (list (test-eglot--diag (current-buffer) 1 2
                                              :error "cannot find value"))))
          (cl-letf (((symbol-function 'eglot-flymake-backend)
                     (lambda (report-fn &rest _)
                       (cl-incf requests)
                       (setq answer report-fn)))
                    ((symbol-function 'flycheck-eglot--convert-diagnostic)
                     (lambda (_d) (flycheck-error-new-at 1 1 'error "x")))
                    ((symbol-function 'flycheck-buffer-automatically)
                     (lambda (&rest _)
                       (cl-incf checks)
                       (when (> checks 10) (error "runaway"))
                       (flycheck-eglot--start nil #'ignore))))
            (flycheck-eglot--start nil #'ignore)
            ;; answer every request the same way, as the server does
            (dotimes (_ 5)
              (when answer
                (let ((fn answer))
                  (setq answer nil)
                  (funcall fn nil :region (cons (point-min) (point-max)))
                  (funcall fn pushed :region (cons (point-min) (point-min))))
                (test-eglot--settle))))
          ;; the first answer moved the diagnostics, so one check; the
          ;; request it made was answered with the same set, and stopped
          (expect checks :to-equal 1)
          (expect requests :to-equal 2)
          (expect flycheck-eglot--diagnostics :to-equal pushed))))

    (it "survives an answer split over two synchronous reports"
      ;; `eglot--flymake-report-push+pulled' hands over the pulled
      ;; diagnostics and then the pushed ones, so one request produces two
      ;; reports.  Treating only the first as the answer made the second
      ;; look volunteered, and the check it started asked again.  The
      ;; recursion runs through `flycheck-perform-deferred-syntax-check',
      ;; so it exhausted the Lisp stack rather than merely spinning.  See
      ;; #2201.
      (flycheck-buttercup-with-temp-buffer
        (let ((requests 0) (checks 0))
          (cl-letf (((symbol-function 'eglot-flymake-backend)
                     (lambda (report-fn &rest _)
                       (cl-incf requests)
                       (funcall report-fn
                                (list (test-eglot--diag (current-buffer) 1 2
                                                        :error "pulled")))
                       (funcall report-fn
                                (list (test-eglot--diag (current-buffer) 1 2
                                                        :error "pushed")))))
                    ((symbol-function 'flycheck-eglot--convert-diagnostic)
                     (lambda (_d) (flycheck-error-new-at 1 1 'error "x")))
                    ((symbol-function 'flycheck-buffer-automatically)
                     (lambda (&rest _)
                       (cl-incf checks)
                       (when (> checks 10) (error "runaway"))
                       (flycheck-eglot--start nil #'ignore))))
            (flycheck-eglot--start nil #'ignore)
            (test-eglot--settle))
          (expect requests :to-equal 1)
          (expect checks :to-equal 0))))

    (it "still hears the next report once a synchronous answer arrived"
      ;; A check publishes what Eglot handed over during the call, and what
      ;; the server volunteers afterwards must still reach the buffer
      (flycheck-buttercup-with-temp-buffer
        (let ((triggered 0))
          (cl-letf (((symbol-function 'eglot-flymake-backend)
                     (lambda (report-fn &rest _)
                       (funcall report-fn
                                (list (test-eglot--diag (current-buffer) 1 2
                                                        :error "a")))))
                    ((symbol-function 'flycheck-eglot--convert-diagnostic)
                     (lambda (_d) (flycheck-error-new-at 1 1 'error "x")))
                    ((symbol-function 'flycheck-buffer-automatically)
                     (lambda (&rest _) (cl-incf triggered))))
            (flycheck-eglot--start nil #'ignore)
            (test-eglot--settle)
            (expect triggered :to-equal 0)
            ;; the server volunteers something new
            (flycheck-eglot--report
             (list (test-eglot--diag (current-buffer) 1 2 :error "b")))
            (test-eglot--settle)
            (expect triggered :to-equal 1)))))

    (it "cancels a settle that the mode is turned off under"
      (flycheck-buttercup-with-temp-buffer
        (spy-on 'flycheck-buffer-automatically)
        (spy-on 'flycheck-eglot--available-p :and-return-value nil)
        (flycheck-eglot--report
         (list (test-eglot--diag (current-buffer) 1 2 :error "x")))
        (expect flycheck-eglot--settle-timer :not :to-be nil)
        (flycheck-eglot--disable)
        (expect flycheck-eglot--settle-timer :to-be nil)
        (test-eglot--settle)
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

    (it "provides the code-action fix only when enabled, managed and supported"
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t)))
        (let ((flycheck-eglot-code-actions t))
          (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t)))
            (expect (flycheck-eglot--fix-provider)
                    :to-be 'flycheck-eglot--code-action-fix))
          (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) nil)))
            (expect (flycheck-eglot--fix-provider) :to-be nil)))
        (let ((flycheck-eglot-code-actions nil))
          (expect (flycheck-eglot--fix-provider) :to-be nil)))
      ;; The capability probe needs a live server, so an unmanaged buffer
      ;; must not reach it
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil))
                ((symbol-function 'eglot-server-capable)
                 (lambda (&rest _) (error "No current server"))))
        (let ((flycheck-eglot-code-actions t))
          (expect (flycheck-eglot--fix-provider) :to-be nil))))

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

(describe "Diagnostics pushes"
  ;; A server publishes whenever it likes, and some republish an
  ;; unchanged set continuously while indexing.  Each of those used to
  ;; cost a full check.
  (it "does not re-check when the pushed diagnostics are unchanged"
    (flycheck-buttercup-with-temp-buffer
      (let ((checks 0))
        (cl-letf (((symbol-function 'flycheck-buffer-automatically)
                   (lambda (&rest _) (cl-incf checks))))
          (let ((diags (list (flymake-make-diagnostic
                              (current-buffer) 1 2 :error "boom"))))
            (dotimes (_ 10)
              (flycheck-eglot--report diags)
              (test-eglot--settle))
            (expect checks :to-equal 1))))))

  (it "does not re-check for an equal set of freshly made diagnostics"
    ;; Servers build new objects every time; only the content matters
    (flycheck-buttercup-with-temp-buffer
      (let ((checks 0))
        (cl-letf (((symbol-function 'flycheck-buffer-automatically)
                   (lambda (&rest _) (cl-incf checks))))
          (dotimes (_ 10)
            (flycheck-eglot--report
             (list (flymake-make-diagnostic (current-buffer) 1 2 :error "boom")))
            (test-eglot--settle))
          (expect checks :to-equal 1)))))

  (it "re-checks once the diagnostics actually change"
    (flycheck-buttercup-with-temp-buffer
      (let ((checks 0))
        (cl-letf (((symbol-function 'flycheck-buffer-automatically)
                   (lambda (&rest _) (cl-incf checks))))
          (flycheck-eglot--report
           (list (flymake-make-diagnostic (current-buffer) 1 2 :error "one")))
          (test-eglot--settle)
          (flycheck-eglot--report
           (list (flymake-make-diagnostic (current-buffer) 1 2 :error "two")))
          (test-eglot--settle)
          (expect checks :to-equal 2)))))

  (it "counts every push, including the ones it skipped"
    (flycheck-buttercup-with-temp-buffer
      (cl-letf (((symbol-function 'flycheck-buffer-automatically) #'ignore))
        (let ((diags (list (flymake-make-diagnostic
                            (current-buffer) 1 2 :error "boom"))))
          (dotimes (_ 7)
            (flycheck-eglot--report diags)
            (test-eglot--settle)))
        (expect flycheck-lsp--push-count :to-equal 7)
        (expect flycheck-lsp--recheck-count :to-equal 1))))

  (describe "what flycheck-verify-setup reports"
    (defun test-push/render (activity)
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (flycheck--verify-princ-lsp-activity activity))
        (buffer-string)))

    (it "says nothing when the server sent no pushes"
      (expect (test-push/render nil) :to-equal ""))

    (it "shows the rate a chatty server pushes at"
      (let ((rendered (test-push/render '(1247 1247 29.0 0.1))))
        (expect rendered :to-match "1247")
        (expect rendered :to-match "43.0 per second")))

    (it "omits a rate measured over too short a window"
      (expect (test-push/render '(2 1 0.02 0.01)) :not :to-match "per second"))))

(describe "Chaining the LSP bridges"
  ;; Both bridges write to `flycheck-checker' and to checker properties
  ;; shared by every buffer, so the order the modes happen to enable in
  ;; must not decide what runs.
  (defun test-bridges/with-both (order body)
    "Enable both bridges in ORDER, then call BODY."
    (flycheck-buttercup-with-temp-buffer
      (text-mode)
      (let ((flycheck-eglot-exclusive nil)
            (flycheck-lsp-exclusive nil)
            (flycheck-lsp-servers '((text-mode "true"))))
        (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                  ((symbol-function 'eglot-flymake-backend) #'ignore)
                  ((symbol-function 'flycheck-mode) #'ignore)
                  ((symbol-function 'flymake-mode) #'ignore)
                  ((symbol-function 'flycheck-lsp--close-buffer) #'ignore)
                  ((symbol-function 'flycheck-buffer-deferred) #'ignore))
          (dolist (which order)
            (pcase which
              ('eglot (flycheck-eglot-mode 1))
              ('lsp (flycheck-lsp-mode 1))))
          (funcall body)
          (flycheck-eglot-mode -1)
          (flycheck-lsp-mode -1))
        (advice-remove 'flymake-diagnostics
                       #'flycheck-eglot--flymake-diagnostics))))

  (it "starts from the same bridge whichever mode enabled last"
    (dolist (order '((eglot lsp) (lsp eglot)))
      (test-bridges/with-both
       order (lambda () (expect flycheck-checker :to-be 'eglot-check)))))

  (it "chains the leading bridge to the other one"
    (test-bridges/with-both
     '(eglot lsp)
     (lambda ()
       (expect (flycheck-checker-get 'eglot-check 'next-checkers)
               :to-contain 'flycheck-lsp))))

  (it "never chains backwards, which would loop forever"
    (test-bridges/with-both
     '(eglot lsp)
     (lambda ()
       (expect (flycheck-checker-get 'flycheck-lsp 'next-checkers)
               :not :to-contain 'eglot-check))))

  (it "does not pile up duplicate entries when a mode re-enables"
    (test-bridges/with-both
     '(eglot lsp eglot lsp)
     (lambda ()
       (let ((next (flycheck-checker-get 'eglot-check 'next-checkers)))
         (expect (length next) :to-equal (length (delete-dups (copy-sequence next))))))))

  (it "leaves a checker the user selected by hand alone"
    (flycheck-buttercup-with-temp-buffer
      (text-mode)
      (setq flycheck-checker 'emacs-lisp)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
                ((symbol-function 'eglot-flymake-backend) #'ignore)
                ((symbol-function 'flycheck-mode) #'ignore)
                ((symbol-function 'flymake-mode) #'ignore)
                ((symbol-function 'flycheck-buffer-deferred) #'ignore))
        (flycheck-eglot-mode 1)
        (expect flycheck-checker :to-be 'emacs-lisp)
        (flycheck-eglot-mode -1))
      (advice-remove 'flymake-diagnostics
                     #'flycheck-eglot--flymake-diagnostics)))

  (describe "flycheck-lsp--primary-bridge"
    (it "is nil when neither bridge is on"
      (flycheck-buttercup-with-temp-buffer
        (expect (flycheck-lsp--primary-bridge) :to-be nil)))))

;;; test-eglot.el ends here
