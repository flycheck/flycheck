;;; test-lsp.el --- Flycheck Specs: LSP diagnostics  -*- lexical-binding: t; -*-

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

;; Specs for the shared LSP diagnostic mapping (`flycheck-lsp--...') used by
;; both the Eglot bridge and the native `lsp' checker.

;;; Code:

(require 'flycheck-buttercup)

(describe "LSP diagnostics"

  (describe "flycheck-lsp--severity-level"
    (it "maps LSP severities to Flycheck levels"
      (expect (flycheck-lsp--severity-level 1) :to-be 'error)
      (expect (flycheck-lsp--severity-level 2) :to-be 'warning)
      (expect (flycheck-lsp--severity-level 3) :to-be 'info)
      (expect (flycheck-lsp--severity-level 4) :to-be 'info))
    (it "treats a missing severity as an error"
      (expect (flycheck-lsp--severity-level nil) :to-be 'error)))

  (describe "flycheck-lsp--diagnostic-id"
    (it "uses the diagnostic code"
      (expect (substring-no-properties
               (flycheck-lsp--diagnostic-id '(:code "E501")))
              :to-equal "E501"))
    (it "carries a codeDescription href as an explainer URL"
      (let ((id (flycheck-lsp--diagnostic-id
                 '(:code "E501" :codeDescription (:href "https://x/E501")))))
        (expect (get-text-property 0 'explainer-url id)
                :to-equal "https://x/E501")))
    (it "is nil without a code"
      (expect (flycheck-lsp--diagnostic-id '(:message "x")) :to-be nil)))

  (describe "flycheck-lsp--uri-to-path"
    (it "decodes a file URI to a local path"
      (expect (flycheck-lsp--uri-to-path "file:///tmp/a%20b.rb")
              :to-equal "/tmp/a b.rb"))
    (it "decodes percent-encoded UTF-8 back to a multibyte path"
      (expect (flycheck-lsp--uri-to-path "file:///tmp/caf%C3%A9.rb")
              :to-equal "/tmp/café.rb"))
    (it "strips the leading slash of a Windows drive URI"
      (expect (flycheck-lsp--uri-to-path "file:///c:/x/y.rb")
              :to-equal "c:/x/y.rb"))
    (it "returns a non-file URI unchanged"
      (expect (flycheck-lsp--uri-to-path "untitled:1") :to-equal "untitled:1")))

  (describe "flycheck-lsp--path-to-uri"
    ;; The round-trip normalizes through `expand-file-name' (which on Windows
    ;; adds the current drive), so compare against the expanded path, not the
    ;; literal, to stay portable.
    (it "round-trips an ASCII path with uri-to-path"
      (expect (flycheck-lsp--uri-to-path
               (flycheck-lsp--path-to-uri "/tmp/a b.rb"))
              :to-equal (expand-file-name "/tmp/a b.rb")))
    (it "round-trips a non-ASCII path with uri-to-path"
      (expect (flycheck-lsp--uri-to-path
               (flycheck-lsp--path-to-uri "/tmp/café.rb"))
              :to-equal (expand-file-name "/tmp/café.rb"))))

  (describe "flycheck-lsp--related-locations"
    (it "returns nil when there is no relatedInformation"
      (expect (flycheck-lsp--related-locations '(:message "x")) :to-be nil))

    (it "converts each entry, incrementing LSP's 0-based positions"
      (let* ((locs (flycheck-lsp--related-locations
                    '(:relatedInformation
                      [(:location (:uri "a.el"
                                   :range (:start (:line 0 :character 0)
                                           :end (:line 0 :character 3)))
                        :message "one")
                       (:location (:uri "b.el"
                                   :range (:start (:line 9 :character 2)
                                           :end (:line 9 :character 2)))
                        :message "two")]))))
        (expect (length locs) :to-equal 2)
        (expect (flycheck-related-location-line (nth 0 locs)) :to-equal 1)
        (expect (flycheck-related-location-column (nth 0 locs)) :to-equal 1)
        (expect (flycheck-related-location-filename (nth 1 locs))
                :to-equal "b.el")
        (expect (flycheck-related-location-line (nth 1 locs))
                :to-equal 10))))

  (describe "workspace-edit fixes (shared)"
    (it "converts an LSP TextEdit to a fix edit, one-basing positions"
      (let ((fe (flycheck-lsp--text-edit
                 '(:range (:start (:line 0 :character 4)
                           :end (:line 0 :character 9))
                   :newText "hello"))))
        (expect (flycheck-fix-edit-line fe) :to-equal 1)
        (expect (flycheck-fix-edit-column fe) :to-equal 5)
        (expect (flycheck-fix-edit-end-line fe) :to-equal 1)
        (expect (flycheck-fix-edit-end-column fe) :to-equal 10)
        (expect (flycheck-fix-edit-replacement fe) :to-equal "hello")))

    (it "builds a fix from a single-file WorkspaceEdit"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
          (let ((fix (flycheck-lsp--workspace-edit-fix
                      '(:documentChanges
                        [(:textDocument (:uri "/proj/a.el")
                          :edits [(:range (:start (:line 0 :character 0)
                                           :end (:line 0 :character 3))
                                   :newText "X")])])
                      "Fix it")))
            (expect (flycheck-fix-description fix) :to-equal "Fix it")
            (expect (length (flycheck-fix-edits fix)) :to-equal 1)))))

    (it "declines a multi-file WorkspaceEdit"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
          (expect (flycheck-lsp--workspace-edit-fix
                   '(:documentChanges
                     [(:textDocument (:uri "/proj/a.el")
                       :edits [(:range (:start (:line 0 :character 0)
                                        :end (:line 0 :character 1))
                                :newText "X")])
                      (:textDocument (:uri "/proj/b.el")
                       :edits [(:range (:start (:line 0 :character 0)
                                        :end (:line 0 :character 1))
                                :newText "Y")])])
                   "x")
                  :to-be nil))))

    (it "declines a WorkspaceEdit with a resource operation"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
          ;; a file-creation op alongside a text edit is not a plain fix
          (expect (flycheck-lsp--workspace-edit-fix
                   '(:documentChanges
                     [(:kind "create" :uri "/proj/new.el")
                      (:textDocument (:uri "/proj/a.el")
                       :edits [(:range (:start (:line 0 :character 0)
                                        :end (:line 0 :character 1))
                                :newText "X")])])
                   "mix")
                  :to-be nil)))))

  (describe "the native lsp checker"

    (describe "flycheck-lsp--language-id"
      (it "strips the -mode and -ts-mode suffixes"
        (expect (flycheck-lsp--language-id 'ruby-mode) :to-equal "ruby")
        (expect (flycheck-lsp--language-id 'ruby-ts-mode) :to-equal "ruby")
        (expect (flycheck-lsp--language-id 'js-mode) :to-equal "js")))

    (describe "flycheck-lsp--command"
      (it "returns the configured command for a mode, nil otherwise"
        (let ((flycheck-lsp-servers '((ruby-mode "rubocop" "--lsp"))))
          (expect (flycheck-lsp--command 'ruby-mode)
                  :to-equal '("rubocop" "--lsp"))
          (expect (flycheck-lsp--command 'python-mode) :to-be nil))))

    (describe "the default flycheck-lsp-servers"
      (it "maps every mode to a non-empty command of strings"
        (dolist (entry flycheck-lsp-servers)
          (expect (symbolp (car entry)) :to-be-truthy)
          (expect (cdr entry) :to-be-truthy)
          (expect (seq-every-p #'stringp (cdr entry)) :to-be-truthy)))
      (it "covers the shipped languages"
        (dolist (mode '(ruby-mode python-mode js-ts-mode toml-mode
                        terraform-mode protobuf-mode markdown-mode))
          (expect (flycheck-lsp--command mode) :to-be-truthy))))

    (describe "flycheck-lsp--position-to-point"
      (it "converts a 0-based line and column to a buffer point"
        (flycheck-buttercup-with-temp-buffer
          (insert "abc\ndef\n")
          ;; line 1 (\"def\"), character 2 -> the \"f\"
          (expect (char-after (flycheck-lsp--position-to-point 1 2))
                  :to-equal ?f)))
      (it "counts an astral character as two UTF-16 code units"
        (flycheck-buttercup-with-temp-buffer
          (insert "\U0001D54Fyz\n")   ; one astral char, then y z
          ;; character offset 2 is past the 2-unit astral char, on the \"y\"
          (expect (char-after (flycheck-lsp--position-to-point 0 2))
                  :to-equal ?y))))

    (describe "flycheck-lsp--diagnostic->error"
      (it "maps a raw LSP diagnostic to a flycheck-error"
        (flycheck-buttercup-with-temp-buffer
          (insert "abcdef\n")
          (let ((err (flycheck-lsp--diagnostic->error
                      '(:severity 2 :message "oops" :code "C1"
                        :range (:start (:line 0 :character 1)
                                :end (:line 0 :character 4)))
                      (current-buffer)
                      (flycheck-lsp--server-create) "file:///x")))
            (expect (flycheck-error-level err) :to-be 'warning)
            (expect (flycheck-error-message err) :to-equal "oops")
            (expect (substring-no-properties (flycheck-error-id err))
                    :to-equal "C1")
            (expect (flycheck-error-line err) :to-equal 1)
            (expect (flycheck-error-column err) :to-equal 2)
            (expect (flycheck-error-checker err) :to-be 'lsp)
            ;; a server with no code-action capability -> no fix
            (expect (flycheck-error-fix err) :to-be nil)))))

    (describe "flycheck-lsp--enabled-p"
      (it "is non-nil only with the mode on, a file, and an installed server"
        (flycheck-buttercup-with-temp-buffer
          (setq-local major-mode 'ruby-mode)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (program &rest _) (concat "/usr/bin/" program))))
            (let ((flycheck-lsp-servers '((ruby-mode "rubocop" "--lsp")))
                  (flycheck-lsp-mode t)
                  (buffer-file-name "/x/a.rb"))
              (expect (flycheck-lsp--enabled-p) :to-be-truthy)
              (setq-local major-mode 'python-mode)
              (expect (flycheck-lsp--enabled-p) :to-be nil)
              (setq-local major-mode 'ruby-mode)
              (setq buffer-file-name nil)
              (expect (flycheck-lsp--enabled-p) :to-be nil)))))
      (it "is nil when the configured server is not installed"
        (flycheck-buttercup-with-temp-buffer
          (setq-local major-mode 'ruby-mode)
          (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
            (let ((flycheck-lsp-servers '((ruby-mode "rubocop" "--lsp")))
                  (flycheck-lsp-mode t)
                  (buffer-file-name "/x/a.rb"))
              (expect (flycheck-lsp--enabled-p) :to-be nil))))))

    (describe "flycheck-lsp--handle-notification"
      (it "caches diagnostics and re-triggers the owning buffer's check"
        (flycheck-buttercup-with-temp-buffer
          (setq-local flycheck-mode t)
          (spy-on 'flycheck-buffer-automatically)
          (let* ((server (flycheck-lsp--server-create))
                 (uri "file:///x/a.rb")
                 (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri))))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer))
            (flycheck-lsp--handle-notification
             server 'textDocument/publishDiagnostics
             (list :uri uri :diagnostics (vector '(:severity 1 :message "m"))))
            (expect (length (flycheck-lsp--doc-diags doc)) :to-equal 1)
            (expect 'flycheck-buffer-automatically :to-have-been-called))))
      (it "routes a re-encoded server URI to the same document"
        ;; The buffer is registered under one URI spelling; a push under a
        ;; different spelling of the same file must still reach it.
        (flycheck-buttercup-with-temp-buffer
          (setq-local flycheck-mode t)
          (spy-on 'flycheck-buffer-automatically)
          (let* ((server (flycheck-lsp--server-create))
                 (doc (flycheck-lsp--document
                       server (flycheck-lsp--doc-key "file:///x/a.rb"))))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer))
            (flycheck-lsp--handle-notification
             server 'textDocument/publishDiagnostics
             (list :uri "file://localhost/x/a.rb"
                   :diagnostics (vector '(:severity 1 :message "m"))))
            (expect 'flycheck-buffer-automatically :to-have-been-called))))
      (it "does not re-trigger while suppressed"
        (flycheck-buttercup-with-temp-buffer
          (setq-local flycheck-mode t)
          (spy-on 'flycheck-buffer-automatically)
          (let* ((server (flycheck-lsp--server-create))
                 (uri "file:///x/a.rb")
                 (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri)))
                 (flycheck-lsp--suppress-recheck t))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer))
            (flycheck-lsp--handle-notification
             server 'textDocument/publishDiagnostics
             (list :uri uri :diagnostics (vector '(:severity 1 :message "m"))))
            (expect 'flycheck-buffer-automatically :not :to-have-been-called)))))

    (describe "flycheck-lsp--sync-document"
      (it "sends didOpen first, then didChange only when the text changed"
        (flycheck-buttercup-with-temp-buffer
          (insert "content")
          (let ((server (flycheck-lsp--server-create :connection 'conn))
                (doc (flycheck-lsp--doc-create))
                (uri "file:///x/a.rb")
                (methods nil))
            (cl-letf (((symbol-function 'jsonrpc-notify)
                       (lambda (_conn method _params) (push method methods))))
              ;; first sync -> didOpen
              (flycheck-lsp--sync-document server doc uri "ruby")
              ;; unchanged buffer -> no message
              (flycheck-lsp--sync-document server doc uri "ruby")
              ;; change the buffer -> didChange
              (goto-char (point-max)) (insert "!")
              (flycheck-lsp--sync-document server doc uri "ruby"))
            (expect (nreverse methods)
                    :to-equal '(textDocument/didOpen textDocument/didChange))))))

    (describe "flycheck-lsp--start"
      (it "reports nothing when the mode has no configured server"
        (flycheck-buttercup-with-temp-buffer
          (let ((flycheck-lsp-servers nil)
                (buffer-file-name "/x/a.rb")
                (reported 'unset))
            (flycheck-lsp--start 'lsp (lambda (status &optional data)
                                        (setq reported (cons status data))))
            (expect reported :to-equal '(finished)))))
      (it "reports nothing but registers the doc while the server initializes"
        (flycheck-buttercup-with-temp-buffer
          (setq-local major-mode 'ruby-mode)
          (let* ((buffer-file-name "/x/a.rb")
                 (flycheck-lsp-servers '((ruby-mode "rubocop" "--lsp")))
                 (server (flycheck-lsp--server-create)) ; initialized nil
                 (reported 'unset))
            (cl-letf (((symbol-function 'flycheck-lsp--ensure-server)
                       (lambda (&rest _) server))
                      ((symbol-function 'flycheck-lsp--sync-document)
                       (lambda (&rest _) (error "must not sync before init"))))
              (flycheck-lsp--start 'lsp (lambda (status &optional data)
                                          (setq reported (cons status data)))))
            (expect reported :to-equal '(finished))
            ;; the document is registered so the init callback can recheck it
            (expect (flycheck-lsp--doc-buffer
                     (gethash (expand-file-name "/x/a.rb")
                              (flycheck-lsp--server-documents server)))
                    :to-be (current-buffer))))))

    (describe "flycheck-lsp--on-initialized"
      (it "stores caps, marks the server ready, and rechecks waiting buffers"
        (flycheck-buttercup-with-temp-buffer
          (setq-local flycheck-mode t)
          (spy-on 'flycheck-buffer-automatically)
          (let* ((server (flycheck-lsp--server-create :connection 'conn))
                 (doc (flycheck-lsp--document server "/x/a.rb")))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer))
            (cl-letf (((symbol-function 'jsonrpc-notify) #'ignore)
                      ((symbol-function 'jsonrpc-running-p) (lambda (_) t)))
              (flycheck-lsp--on-initialized server '(:capabilities (:x t))))
            (expect (flycheck-lsp--server-initialized server) :to-be-truthy)
            (expect (flycheck-lsp--server-capabilities server) :to-equal '(:x t))
            (expect 'flycheck-buffer-automatically :to-have-been-called)))))

    (describe "flycheck-lsp--init-failed"
      (it "shuts the server down and drops it from the registry"
        (let* ((server (flycheck-lsp--server-create
                        :root "/p/" :command '("rubocop" "--lsp")))
               (key (flycheck-lsp--server-key server))
               (flycheck-lsp--servers (make-hash-table :test 'equal)))
          (puthash key server flycheck-lsp--servers)
          (cl-letf (((symbol-function 'flycheck-lsp--shutdown-server) #'ignore))
            (flycheck-lsp--init-failed server "timeout"))
          (expect (gethash key flycheck-lsp--servers) :to-be nil))))

    (describe "flycheck-lsp--capable"
      (it "walks the capability plist"
        (let ((server (flycheck-lsp--server-create
                       :capabilities '(:codeActionProvider (:resolveProvider t)))))
          (expect (flycheck-lsp--capable server :codeActionProvider)
                  :to-equal '(:resolveProvider t))
          (expect (flycheck-lsp--capable
                   server :codeActionProvider :resolveProvider)
                  :to-be t)))
      (it "is nil past a boolean capability, and for a missing one"
        (let ((server (flycheck-lsp--server-create
                       :capabilities '(:codeActionProvider t))))
          (expect (flycheck-lsp--capable
                   server :codeActionProvider :resolveProvider)
                  :to-be nil)
          (expect (flycheck-lsp--capable server :hoverProvider) :to-be nil)))
      (it "treats a JSON false capability as absent"
        ;; jsonrpc decodes JSON `false' to `:json-false', which is truthy.
        (let ((server (flycheck-lsp--server-create
                       :capabilities '(:codeActionProvider :json-false))))
          (expect (flycheck-lsp--capable server :codeActionProvider)
                  :to-be nil))))

    (describe "flycheck-lsp--fix-provider"
      (it "is a function when enabled and the server is capable"
        (let ((server (flycheck-lsp--server-create
                       :capabilities '(:codeActionProvider t)))
              (flycheck-lsp-code-actions t))
          (expect (functionp
                   (flycheck-lsp--fix-provider server "file:///a" '(:range nil)))
                  :to-be-truthy)))
      (it "is nil when the server has no code actions"
        (let ((server (flycheck-lsp--server-create :capabilities nil))
              (flycheck-lsp-code-actions t))
          (expect (flycheck-lsp--fix-provider server "file:///a" nil) :to-be nil)))
      (it "is nil when the feature is off"
        (let ((server (flycheck-lsp--server-create
                       :capabilities '(:codeActionProvider t)))
              (flycheck-lsp-code-actions nil))
          (expect (flycheck-lsp--fix-provider server "file:///a" nil) :to-be nil))))

    (describe "flycheck-lsp--code-action-fix"
      (it "requests, prefers the isPreferred action, and builds a fix"
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/proj/a.rb")
          (let ((server (flycheck-lsp--server-create :connection 'conn))
                (edit '(:documentChanges
                        [(:textDocument (:uri "/proj/a.rb")
                          :edits [(:range (:start (:line 0 :character 0)
                                           :end (:line 0 :character 1))
                                   :newText "Y")])])))
            (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
                      ((symbol-function 'flycheck-lsp--sync-document) #'ignore)
                      ((symbol-function 'flycheck-same-files-p) #'equal)
                      ((symbol-function 'flycheck-lsp--request)
                       (lambda (_s method _p)
                         (when (eq method 'textDocument/codeAction)
                           (vector (list :title "skip")
                                   (list :title "do it" :isPreferred t
                                         :edit edit))))))
              (let ((fix (flycheck-lsp--code-action-fix
                          server "file:///proj/a.rb" '(:range nil))))
                (expect (flycheck-fix-description fix) :to-equal "do it")
                (expect (length (flycheck-fix-edits fix)) :to-equal 1))))))
      (it "resolves an action that has data but no edit"
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/proj/a.rb")
          (let ((server (flycheck-lsp--server-create
                         :connection 'conn
                         :capabilities '(:codeActionProvider (:resolveProvider t))))
                (edit '(:documentChanges
                        [(:textDocument (:uri "/proj/a.rb")
                          :edits [(:range (:start (:line 0 :character 0)
                                           :end (:line 0 :character 1))
                                   :newText "Z")])])))
            (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
                      ((symbol-function 'flycheck-lsp--sync-document) #'ignore)
                      ((symbol-function 'flycheck-same-files-p) #'equal)
                      ((symbol-function 'flycheck-lsp--request)
                       (lambda (_s method _p)
                         (pcase method
                           ('textDocument/codeAction
                            (vector (list :title "lazy" :data "d")))
                           ('codeAction/resolve
                            (list :title "lazy" :data "d" :edit edit))))))
              (let ((fix (flycheck-lsp--code-action-fix
                          server "file:///proj/a.rb" '(:range nil))))
                (expect (flycheck-fix-description fix) :to-equal "lazy")
                (expect (length (flycheck-fix-edits fix)) :to-equal 1)))))))

    (describe "the lsp generic checker"
      (it "is a registered generic checker"
        (expect (flycheck-valid-checker-p 'lsp) :to-be-truthy)
        (expect (flycheck-checker-get 'lsp 'start) :to-be #'flycheck-lsp--start)))))

;;; test-lsp.el ends here
