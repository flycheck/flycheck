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

(require 'cl-lib)
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
      (expect (flycheck-lsp--uri-to-path "untitled:1") :to-equal "untitled:1"))
    ;; A server on a remote host names files as that host sees them, so
    ;; the answer has to be put back on that host to name a file Emacs
    ;; can open or compare against the buffer's name.
    (it "puts the remote host back when given one"
      (expect (flycheck-lsp--uri-to-path "file:///home/u/a.rb" "/ssh:host:")
              :to-equal "/ssh:host:/home/u/a.rb"))
    (it "leaves a Windows drive alone on a remote host"
      ;; A drive letter is a local spelling; a remote path keeps its slash.
      (expect (flycheck-lsp--uri-to-path "file:///c:/x/y.rb" "/ssh:host:")
              :to-equal "/ssh:host:/c:/x/y.rb")))

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
              :to-equal (expand-file-name "/tmp/café.rb")))
    ;; The server knows nothing of Emacs's remote file names.
    (it "names a remote path as the server's host sees it"
      (expect (flycheck-lsp--path-to-uri "/ssh:host:/home/u/a.rb")
              :to-equal "file:///home/u/a.rb"))
    (it "round-trips a remote path through its host"
      (expect (flycheck-lsp--uri-to-path
               (flycheck-lsp--path-to-uri "/ssh:host:/home/u/a.rb")
               "/ssh:host:")
              :to-equal "/ssh:host:/home/u/a.rb")))

  (describe "flycheck-lsp--tags"
    (it "maps the LSP tag codes onto Flycheck's symbols"
      (expect (flycheck-lsp--tags '(:tags [1])) :to-equal '(unnecessary))
      (expect (flycheck-lsp--tags '(:tags [2])) :to-equal '(deprecated))
      (expect (flycheck-lsp--tags '(:tags [1 2]))
              :to-equal '(unnecessary deprecated)))

    (it "is nil for a diagnostic with no tags"
      (expect (flycheck-lsp--tags '(:message "x")) :to-be nil)
      (expect (flycheck-lsp--tags '(:tags [])) :to-be nil))

    (it "drops a code it has no rendering for"
      ;; passing it through would only produce a symbol nothing acts on
      (expect (flycheck-lsp--tags '(:tags [99])) :to-be nil)
      (expect (flycheck-lsp--tags '(:tags [1 99])) :to-equal '(unnecessary))))

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

  (describe "project diagnostics for unvisited documents"

    ;; A push is cached for any document, opened or not; the bridge
    ;; surfaces the unowned ones in the error list's project scope.

    (cl-flet ((server-with-doc (root path diags &optional buffer)
                (let ((server (flycheck-lsp--server-create
                               :root root :command '("srv"))))
                  (let ((doc (flycheck-lsp--document server path)))
                    (setf (flycheck-lsp--doc-diags doc) diags)
                    (setf (flycheck-lsp--doc-buffer doc) buffer))
                  (puthash (cons root '("srv")) server flycheck-lsp--servers)
                  server)))

      (let ((project (file-name-as-directory (expand-file-name "/proj")))
            (diag '(:range (:start (:line 27 :character 1)
                            :end (:line 27 :character 2))
                    :severity 1 :code "UndeclaredName"
                    :message "undefined: a")))

        (it "surfaces cached diagnostics of documents no buffer owns"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
                (path (expand-file-name "box/box.go" project)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (server-with-doc project path (list diag))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-lsp-mode t)
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be-equal-flycheck-errors
                      (list (flycheck-error-new-at
                             28 2 'error "undefined: a"
                             :id "UndeclaredName" :checker 'flycheck-lsp
                             :filename path :buffer nil))))))

        (it "surfaces them for a buffer the preference serves too"
          ;; `flycheck-lsp-prefer-server' puts a document on a server
          ;; without the mode, so the project view should show that
          ;; server's findings just the same.
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
                (path (expand-file-name "box/box.go" project)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (spy-on 'flycheck-lsp--preferred-p :and-return-value t)
            (server-with-doc project path (list diag))
            (flycheck-buttercup-with-temp-buffer
              ;; The mode is off; only the preference is serving.
              (expect (bound-and-true-p flycheck-lsp-mode) :to-be nil)
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be-equal-flycheck-errors
                      (list (flycheck-error-new-at
                             28 2 'error "undefined: a"
                             :id "UndeclaredName" :checker 'flycheck-lsp
                             :filename path :buffer nil))))))

        (it "shows nothing when neither the mode nor the preference serves"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
                (path (expand-file-name "box/box.go" project)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (server-with-doc project path (list diag))
            (flycheck-buttercup-with-temp-buffer
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be nil))))

        (it "skips a document a live buffer owns"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
                (owner (generate-new-buffer " owner")))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (unwind-protect
                (progn
                  (server-with-doc project (expand-file-name "owned" project)
                                   (list diag) owner)
                  (flycheck-buttercup-with-temp-buffer
                    (setq-local flycheck-lsp-mode t)
                    (expect (flycheck-lsp--project-extra-errors project nil)
                            :to-be nil)))
              (kill-buffer owner))))

        (it "skips a server that died"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value nil)
            (server-with-doc project (expand-file-name "x" project) (list diag))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-lsp-mode t)
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be nil))))

        (it "skips a server rooted outside the project"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
                (other (file-name-as-directory (expand-file-name "/elsewhere"))))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (server-with-doc other (expand-file-name "x" other) (list diag))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-lsp-mode t)
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be nil))))

        (it "skips a document outside the project on a server inside it"
          ;; e.g. a server diagnosing a dependency it was told about
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (server-with-doc project (expand-file-name "/elsewhere/dep")
                             (list diag))
            (flycheck-buttercup-with-temp-buffer
              (setq-local flycheck-lsp-mode t)
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be nil))))

        (it "skips a file some buffer visits with the bridge off"
          ;; That buffer reports the file's problems its own way; showing
          ;; the cached push too would state the same problems twice.
          (let* ((flycheck-lsp--servers (make-hash-table :test 'equal))
                 (path (expand-file-name "visited.rb" project))
                 (visiting (generate-new-buffer " visiting")))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (unwind-protect
                (progn
                  (with-current-buffer visiting
                    (set-visited-file-name path 'no-query))
                  (server-with-doc project path (list diag))
                  (flycheck-buttercup-with-temp-buffer
                    (setq-local flycheck-lsp-mode t)
                    (expect (flycheck-lsp--project-extra-errors project nil)
                            :to-be nil)))
              (with-current-buffer visiting
                (set-buffer-modified-p nil)
                (kill-buffer)))))

        (it "contributes nothing while the bridge is off everywhere"
          (let ((flycheck-lsp--servers (make-hash-table :test 'equal)))
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (server-with-doc project (expand-file-name "x" project) (list diag))
            (flycheck-buttercup-with-temp-buffer
              (expect (flycheck-lsp--project-extra-errors project nil)
                      :to-be nil)))))))

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

    (it "builds a fix in a buffer on a remote host"
      ;; The server reports the file it edits by its own path; unless that
      ;; is put back on the buffer's host it never matches the buffer's
      ;; name, and every quickfix is silently declined.
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/ssh:flycheck-nonexistent-host:/proj/a.el"
              default-directory "/ssh:flycheck-nonexistent-host:/proj/")
        (let ((fix (flycheck-lsp--workspace-edit-fix
                    '(:documentChanges
                      [(:textDocument (:uri "file:///proj/a.el")
                        :edits [(:range (:start (:line 0 :character 0)
                                         :end (:line 0 :character 3))
                                 :newText "X")])])
                    "Fix it")))
          (expect (flycheck-fix-p fix) :to-be-truthy)
          (expect (length (flycheck-fix-edits fix)) :to-equal 1))))

    (it "builds a fix from a legacy `changes' WorkspaceEdit"
      ;; jsonrpc decodes the changes object's URI keys to keywords
      ;; (`:file:///...'); the fix must strip the leading colon.  Ruff and
      ;; other servers use this form, so this path must work.
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
          (let ((fix (flycheck-lsp--workspace-edit-fix
                      '(:changes
                        (:file:///proj/a.el
                         [(:range (:start (:line 0 :character 0)
                                   :end (:line 1 :character 0))
                           :newText "")]))
                      "Remove import")))
            (expect (flycheck-fix-description fix) :to-equal "Remove import")
            (expect (length (flycheck-fix-edits fix)) :to-equal 1)
            (expect (flycheck-fix-edit-replacement (car (flycheck-fix-edits fix)))
                    :to-equal "")))))

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

    (describe "flycheck-lsp--available-command"
      (it "caches its result per mode instead of re-probing the executable"
        (flycheck-buttercup-with-temp-buffer
          (setq-local major-mode 'ruby-mode)
          (let ((flycheck-lsp-servers '((ruby-mode "rubocop" "--lsp")))
                (flycheck-executable-find (lambda (_p) "/usr/bin/rubocop")))
            ;; first call caches the positive result
            (expect (flycheck-lsp--available-command 'ruby-mode)
                    :to-equal '("rubocop" "--lsp"))
            ;; even once the program looks absent, the cached command stands
            (setq flycheck-executable-find (lambda (_p) nil))
            (expect (flycheck-lsp--available-command 'ruby-mode)
                    :to-equal '("rubocop" "--lsp"))))))

    (describe "the default flycheck-lsp-servers"
      (it "maps every mode to a non-empty command of strings"
        (dolist (entry flycheck-lsp-servers)
          (expect (symbolp (car entry)) :to-be-truthy)
          (expect (cdr entry) :to-be-truthy)
          (expect (seq-every-p #'stringp (cdr entry)) :to-be-truthy)))
      (it "covers the shipped languages"
        (dolist (mode '(ruby-mode python-mode js-ts-mode css-mode markdown-mode))
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
            (expect (flycheck-error-checker err) :to-be 'flycheck-lsp)
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
            (flycheck-lsp--start 'flycheck-lsp (lambda (status &optional data)
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
              (flycheck-lsp--start 'flycheck-lsp (lambda (status &optional data)
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
          (expect (flycheck-lsp--fix-provider server "file:///a" nil) :to-be nil)))
      (it "returns an inline fix eagerly, not a lazy provider"
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/proj/a.rb")
          (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
            (let* ((server (flycheck-lsp--server-create))  ; no codeActionProvider
                   (flycheck-lsp-code-actions t)
                   (lsp '(:data (:correctable t :code_actions
                                 [(:title "Autocorrect" :kind "quickfix" :isPreferred t
                                   :edit (:documentChanges
                                          [(:textDocument (:uri "/proj/a.rb")
                                            :edits [(:range (:start (:line 0 :character 0)
                                                     :end (:line 0 :character 1))
                                                     :newText "")])]))])))
                   (fix (flycheck-lsp--fix-provider server "file:///proj/a.rb" lsp)))
              (expect (flycheck-fix-p fix) :to-be-truthy)
              (expect (flycheck-fix-description fix) :to-equal "Autocorrect"))))))

    (describe "flycheck-lsp--inline-fix"
      (it "builds a fix from the isPreferred inline code action"
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/proj/a.rb")
          (cl-letf (((symbol-function 'flycheck-same-files-p) #'equal))
            (let ((fix (flycheck-lsp--inline-fix
                        '(:data (:correctable t :code_actions
                                 [(:title "Autocorrect X" :kind "quickfix" :isPreferred t
                                   :edit (:documentChanges
                                          [(:textDocument (:uri "/proj/a.rb")
                                            :edits [(:range (:start (:line 0 :character 0)
                                                     :end (:line 0 :character 1))
                                                     :newText "")])]))
                                  (:title "Disable X" :kind "quickfix"
                                   :edit (:documentChanges
                                          [(:textDocument (:uri "/proj/a.rb")
                                            :edits [(:range (:start (:line 0 :character 0)
                                                     :end (:line 0 :character 0))
                                                     :newText "# disable\n")])]))])))))
              (expect (flycheck-fix-description fix) :to-equal "Autocorrect X")))))
      (it "is nil when no inline action is preferred (only disable)"
        (expect (flycheck-lsp--inline-fix
                 '(:data (:correctable :json-false :code_actions
                          [(:title "Disable X" :kind "quickfix"
                            :edit (:documentChanges []))])))
                :to-be nil))
      (it "is nil when the diagnostic carries no inline actions"
        (expect (flycheck-lsp--inline-fix '(:code "X")) :to-be nil)))

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

    (describe "pull-model diagnostics"

      (it "advertises the diagnostic capabilities"
        (let ((caps (plist-get (flycheck-lsp--initialize-params "/proj/")
                               :capabilities)))
          (expect (plist-get (plist-get caps :textDocument) :diagnostic)
                  :to-be-truthy)
          (expect (plist-get (plist-get (plist-get caps :workspace) :diagnostics)
                             :refreshSupport)
                  :to-be t)))

      (it "ignores a report about a version the buffer has moved past"
        (flycheck-buttercup-with-temp-buffer
          (setq-local flycheck-mode t)
          (spy-on 'flycheck-buffer-automatically)
          (let* ((server (flycheck-lsp--server-create))
                 (uri "file:///x/a.rb")
                 (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri))))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer)
                  (flycheck-lsp--doc-version doc) 3)
            (flycheck-lsp--accept-diagnostics
             server uri (vector '(:severity 1 :message "old")) 2 "r1")
            (expect (flycheck-lsp--doc-diags doc) :to-be nil)
            (expect (flycheck-lsp--doc-result-id doc) :to-be nil)
            (expect 'flycheck-buffer-automatically :not :to-have-been-called)
            (flycheck-lsp--accept-diagnostics
             server uri (vector '(:severity 1 :message "new")) 3 "r2")
            (expect (length (flycheck-lsp--doc-diagnostics doc)) :to-equal 1)
            (expect (flycheck-lsp--doc-result-id doc) :to-equal "r2")
            (expect 'flycheck-buffer-automatically :to-have-been-called))))

      (it "re-pulls the open documents on a refresh request, and the workspace after a pull"
        (flycheck-buttercup-with-temp-buffer
          (let* ((server (flycheck-lsp--server-create))
                 (uri "file:///x/a.rb")
                 (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri))))
            (setf (flycheck-lsp--doc-buffer doc) (current-buffer)
                  (flycheck-lsp--doc-version doc) 1)
            (spy-on 'flycheck-lsp--pull-document)
            (spy-on 'flycheck-lsp--pull-workspace)
            (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
            (expect (flycheck-lsp--handle-request
                     server 'workspace/diagnostic/refresh nil)
                    :to-be nil)
            (expect 'flycheck-lsp--pull-document :to-have-been-called)
            (expect 'flycheck-lsp--pull-workspace :not :to-have-been-called)
            (setf (flycheck-lsp--server-workspace-pulled server) t)
            (flycheck-lsp--handle-request server 'workspace/diagnostic/refresh nil)
            (expect 'flycheck-lsp--pull-workspace :to-have-been-called-with server))))

      (it "keeps pushed and pulled diagnostics apart, showing both"
        (let* ((server (flycheck-lsp--server-create))
               (uri "file:///x/a.rb")
               (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri))))
          (flycheck-lsp--accept-diagnostics
           server uri (vector '(:severity 1 :message "pushed")) nil nil 'pushed)
          (flycheck-lsp--accept-diagnostics
           server uri (vector '(:severity 1 :message "pulled")) nil "r1")
          (expect (mapcar (lambda (d) (plist-get d :message))
                          (flycheck-lsp--doc-diagnostics doc))
                  :to-equal '("pushed" "pulled"))
          ;; An empty pull answer clears only what was pulled
          (flycheck-lsp--accept-diagnostics server uri [] nil "r2")
          (expect (mapcar (lambda (d) (plist-get d :message))
                          (flycheck-lsp--doc-diagnostics doc))
                  :to-equal '("pushed"))))

      (it "refers to the last result id when pulling a document again"
        (let* ((server (flycheck-lsp--server-create))
               (uri "file:///x/a.rb")
               (doc (flycheck-lsp--document server (flycheck-lsp--doc-key uri))))
          (setf (flycheck-lsp--doc-version doc) 2)
          (spy-on 'jsonrpc-async-request)
          (flycheck-lsp--pull-document server doc uri)
          (expect (plist-get (nth 2 (spy-calls-args-for 'jsonrpc-async-request 0))
                             :previousResultId)
                  :to-be nil)
          (expect (flycheck-lsp--doc-pulled-version doc) :to-equal 2)
          (setf (flycheck-lsp--doc-result-id doc) "r7")
          (flycheck-lsp--pull-document server doc uri)
          (expect (plist-get (nth 2 (spy-calls-args-for 'jsonrpc-async-request 1))
                             :previousResultId)
                  :to-equal "r7")))

      (it "forgets the unvisited documents under the project on clearing"
        (flycheck-buttercup-with-temp-buffer
          (let* ((flycheck-lsp--servers (make-hash-table :test 'equal))
                 (project (file-name-as-directory (expand-file-name "/proj")))
                 (server (flycheck-lsp--server-create :root project
                                                      :command '("srv")))
                 (visited (flycheck-lsp--document server "/proj/a.rb"))
                 (unvisited (flycheck-lsp--document server "/proj/b.rb")))
            (setf (flycheck-lsp--doc-buffer visited) (current-buffer)
                  (flycheck-lsp--doc-diags unvisited) '((:message "m")))
            (puthash (cons project '("srv")) server flycheck-lsp--servers)
            (flycheck-lsp--clear-project project)
            (expect (gethash "/proj/a.rb" (flycheck-lsp--server-documents server))
                    :to-be visited)
            (expect (gethash "/proj/b.rb" (flycheck-lsp--server-documents server))
                    :to-be nil))))

      (it "pulls the workspace of the capable servers under the project"
        (flycheck-buttercup-with-temp-buffer
         (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
               (project (file-name-as-directory (expand-file-name "/proj"))))
          (spy-on 'flycheck-lsp--server-live-p :and-return-value t)
          (spy-on 'flycheck-lsp--pull-workspace)
          (cl-flet ((add (root command caps)
                      (let ((server (flycheck-lsp--server-create
                                     :root root :command command
                                     :capabilities caps :initialized t)))
                        ;; A buffer on the server, or nothing would show
                        ;; what it finds
                        (setf (flycheck-lsp--doc-buffer
                               (flycheck-lsp--document server "/proj/a.rb"))
                              (current-buffer))
                        (puthash (cons root command) server
                                 flycheck-lsp--servers))))
            (add project '("ws")
                 '(:diagnosticProvider (:workspaceDiagnostics t)))
            (add project '("doc-only")
                 '(:diagnosticProvider (:workspaceDiagnostics :json-false)))
            (add (file-name-as-directory (expand-file-name "/elsewhere")) '("far")
                 '(:diagnosticProvider (:workspaceDiagnostics t)))
            (expect (flycheck-lsp--check-project project)
                    :to-equal '("ws (workspace diagnostics)"))
            (expect (spy-calls-count 'flycheck-lsp--pull-workspace)
                    :to-equal 1))))))

    (describe "the flycheck-lsp generic checker"
      (it "is a registered generic checker"
        (expect (flycheck-valid-checker-p 'flycheck-lsp) :to-be-truthy)
        (expect (flycheck-checker-get 'flycheck-lsp 'start) :to-be #'flycheck-lsp--start))

      (describe "preferring a resident server"

        ;; A command checker spawns its linter afresh on every check; the
        ;; linter's own server stays resident.  The substitution happens
        ;; only where automatic selection chose the superseded checker.

        ;; Registering the mode mutates a property shared by every
        ;; buffer, so put it back rather than leaving it for later specs.
        (before-each
          (setq flycheck-test--lsp-modes
                (flycheck-checker-get 'flycheck-lsp 'modes)))
        (after-each
          (setf (flycheck-checker-get 'flycheck-lsp 'modes)
                flycheck-test--lsp-modes))

        (defvar flycheck-test--lsp-modes nil)

        (defun flycheck-test--preferred-checker (&rest bindings)
          "Return the checker chosen in a Ruby buffer under BINDINGS."
          (let ((dir (file-name-as-directory
                      (make-temp-file "flycheck-prefer" t))))
            (unwind-protect
                (with-temp-buffer
                  (delay-mode-hooks (ruby-mode))
                  (setq buffer-file-name (expand-file-name "a.rb" dir)
                        default-directory dir)
                  (let ((flycheck-executable-find
                         (if (plist-member bindings :find)
                             (plist-get bindings :find)
                           #'identity))
                        (flycheck-checkers
                         (or (plist-get bindings :checkers)
                             '(ruby-rubocop flycheck-lsp)))
                        (flycheck-disabled-checkers
                         (plist-get bindings :disabled))
                        (flycheck-lsp-servers
                         (or (plist-get bindings :servers)
                             flycheck-lsp-servers))
                        (flycheck-lsp-checker-servers
                         (or (plist-get bindings :checker-servers)
                             flycheck-lsp-checker-servers)))
                    (if (plist-member bindings :prefer)
                        (let ((flycheck-lsp-prefer-server
                               (plist-get bindings :prefer))
                              (flycheck-checker (plist-get bindings :selected)))
                          (flycheck-get-checker-for-buffer))
                      ;; No binding at all, so the default is under test.
                      (flycheck-get-checker-for-buffer))))
              (delete-directory dir t))))

        (it "leaves the command checker alone out of the box"
          ;; Deliberately does not bind the option: the default is what
          ;; ships, and flipping it must fail this.
          (expect (default-value 'flycheck-lsp-prefer-server) :to-be nil)
          (expect (flycheck-test--preferred-checker) :to-be 'ruby-rubocop))

        (it "uses the server when asked to, and it is usable"
          (let ((dir (file-name-as-directory
                      (make-temp-file "flycheck-prefer" t))))
            (unwind-protect
                (with-temp-buffer
                  (delay-mode-hooks (ruby-mode))
                  (setq buffer-file-name (expand-file-name "a.rb" dir)
                        default-directory dir)
                  (let ((flycheck-executable-find #'identity)
                        (flycheck-checkers '(ruby-rubocop flycheck-lsp))
                        (flycheck-lsp-prefer-server t))
                    (expect (flycheck-get-checker-for-buffer)
                            :to-be 'flycheck-lsp)
                    ;; Returning the symbol is not enough: selection has
                    ;; always yielded a checker that may actually run.
                    (expect (flycheck-may-use-checker 'flycheck-lsp)
                            :to-be-truthy)
                    ;; Which is the predicate's second reason, not the mode.
                    (expect (bound-and-true-p flycheck-lsp-mode) :to-be nil)
                    (expect (flycheck-lsp--preferred-p) :to-be-truthy)))
              (delete-directory dir t))))

        (it "honours a list of checkers"
          (expect (flycheck-test--preferred-checker :prefer '(ruby-rubocop))
                  :to-be 'flycheck-lsp)
          (expect (flycheck-test--preferred-checker :prefer '(python-ruff))
                  :to-be 'ruby-rubocop))

        (it "does not stand one linter in for another"
          ;; The manual tells people to point a mode at standardrb's
          ;; server.  Substituting RuboCop's checker with it would report
          ;; a different linter's diagnostics under RuboCop's name.
          (expect (flycheck-test--preferred-checker
                   :prefer t :servers '((ruby-mode "standardrb" "--lsp")))
                  :to-be 'ruby-rubocop))

        (it "sees through a wrapper and an absolute path"
          ;; `bundle exec rubocop --lsp' is how Ruby projects run it.
          (expect (flycheck-test--preferred-checker
                   :prefer t
                   :servers '((ruby-mode "bundle" "exec" "rubocop" "--lsp")))
                  :to-be 'flycheck-lsp)
          (expect (flycheck-test--preferred-checker
                   :prefer t
                   :servers '((ruby-mode "/opt/rubocop" "--lsp")))
                  :to-be 'flycheck-lsp))

        (it "never overrides a checker the user selected"
          (expect (flycheck-test--preferred-checker
                   :prefer t :selected 'ruby-rubocop)
                  :to-be 'ruby-rubocop))

        (it "serves a remote buffer too"
          ;; The server runs on that host, so the preference applies
          ;; there as well.  Asks the substitution directly: running the
          ;; whole of selection would have the command checker reach for
          ;; the host.
          (with-temp-buffer
            (delay-mode-hooks (ruby-mode))
            (setq buffer-file-name "/ssh:host:/proj/a.rb"
                  default-directory "/ssh:host:/proj/")
            (let ((flycheck-executable-find #'identity)
                  (flycheck-checkers '(ruby-rubocop flycheck-lsp))
                  (flycheck-lsp-prefer-server t))
              ;; The preference is active here, and a remote buffer is
              ;; served like any other now.
              (expect (flycheck-lsp--preferred-p) :to-be-truthy)
              (expect (flycheck-lsp--enabled-p) :to-be-truthy))))

        (it "respects disabling and unregistering the checker"
          (expect (flycheck-test--preferred-checker
                   :prefer t :disabled '(flycheck-lsp))
                  :to-be 'ruby-rubocop)
          (expect (flycheck-test--preferred-checker
                   :prefer t :checkers '(ruby-rubocop))
                  :to-be 'ruby-rubocop))

        (it "refuses a malformed value instead of reading it as t"
          ;; `ruby-rubocop' rather than `(ruby-rubocop)' is the likely
          ;; typo, and must not turn the preference on for everything.
          (expect (flycheck-test--preferred-checker :prefer 'ruby-rubocop)
                  :to-be 'ruby-rubocop))

        (it "declines when the server program is not installed"
          ;; Only the server is missing: the command checker's own
          ;; executable is still found, so it stays selectable.
          (expect (flycheck-test--preferred-checker
                   :prefer t
                   :servers '((ruby-mode "absent-server" "--lsp"))
                   :checker-servers '((ruby-rubocop "absent-server" "--lsp"))
                   :find (lambda (x) (unless (equal x "absent-server") x)))
                  :to-be 'ruby-rubocop))

        (it "registers the mode once, however often it is asked"
          ;; `flycheck-add-mode' is a bare push and this runs on every
          ;; check, so an unguarded call grows a global list without end.
          (let ((dir (file-name-as-directory
                      (make-temp-file "flycheck-prefer" t))))
            (unwind-protect
                (with-temp-buffer
                  (delay-mode-hooks (ruby-mode))
                  (setq buffer-file-name (expand-file-name "a.rb" dir)
                        default-directory dir)
                  (let ((flycheck-executable-find #'identity)
                        (flycheck-checkers '(ruby-rubocop flycheck-lsp))
                        (flycheck-lsp-prefer-server t))
                    (dotimes (_ 5) (flycheck-get-checker-for-buffer))
                    (expect (seq-count (lambda (m) (eq m 'ruby-mode))
                                       (flycheck-checker-get 'flycheck-lsp
                                                             'modes))
                            :to-equal 1)))
              (delete-directory dir t))))

        (it "stops substituting as soon as the option goes off"
          ;; Nothing is remembered per buffer, so a buffer that used a
          ;; server is not left flagged.
          (let ((dir (file-name-as-directory
                      (make-temp-file "flycheck-prefer" t))))
            (unwind-protect
                (with-temp-buffer
                  (delay-mode-hooks (ruby-mode))
                  (setq buffer-file-name (expand-file-name "a.rb" dir)
                        default-directory dir)
                  (let ((flycheck-executable-find #'identity)
                        (flycheck-checkers '(ruby-rubocop flycheck-lsp)))
                    (let ((flycheck-lsp-prefer-server t))
                      (expect (flycheck-get-checker-for-buffer)
                              :to-be 'flycheck-lsp))
                    (let ((flycheck-lsp-prefer-server nil))
                      (expect (flycheck-get-checker-for-buffer)
                              :to-be 'ruby-rubocop)
                      (expect (flycheck-may-use-checker 'flycheck-lsp)
                              :to-be nil))))
              (delete-directory dir t)))))

      (describe "listing and stopping servers"

        ;; A server outlives the buffers it serves and even the mode
        ;; being turned off, so without these a wedged one can only be
        ;; cleared by restarting Emacs.

        (defun flycheck-test--fake-server (root command &rest buffers)
          "Register a server for ROOT running COMMAND holding BUFFERS.
A nil in BUFFERS stands for a document no buffer visits, as a
workspace pull leaves behind."
          (let ((server (flycheck-lsp--server-create
                         :root root :command command))
                (n 0))
            (dolist (buffer buffers)
              (puthash (format "/doc%d" (setq n (1+ n)))
                       (flycheck-lsp--doc-create :buffer buffer)
                       (flycheck-lsp--server-documents server)))
            (puthash (cons root command) server flycheck-lsp--servers)
            server))

        (after-each (clrhash flycheck-lsp--servers))

        (it "counts only the documents a live buffer still owns"
          ;; A workspace pull registers a document per reported file, so
          ;; the table size is not the number of buffers.
          (let* ((buffer (generate-new-buffer "held"))
                 (server (flycheck-test--fake-server
                          "/proj/" '("rubocop" "--lsp") buffer nil)))
            (expect (hash-table-count (flycheck-lsp--server-documents server))
                    :to-equal 2)
            (expect (flycheck-lsp--server-buffer-count server) :to-equal 1)
            (kill-buffer buffer)
            (expect (flycheck-lsp--server-buffer-count server) :to-equal 0)))

        (it "lists what is running, documents apart from buffers"
          (let ((buffer (generate-new-buffer "held")))
            (unwind-protect
                (progn
                  (flycheck-test--fake-server "/proj/" '("rubocop" "--lsp")
                                              buffer nil)
                  (let ((entry (car (flycheck-lsp--server-list-entries))))
                    (expect (append (cadr entry) nil)
                            :to-equal (list (abbreviate-file-name "/proj/")
                                            "rubocop --lsp" "dead" "2" "1"))))
              (kill-buffer buffer))))

        (it "finds the server that serves the buffer, not merely one
holding its document"
          ;; Roots nest, so a document can sit in several servers.  Only
          ;; the one keyed on this buffer's root actually checks it.
          (flycheck-test--fake-server "/p/" '("ruby-lsp"))
          (flycheck-test--fake-server "/p/sub/" '("ruby-lsp"))
          (flycheck-buttercup-with-temp-buffer
            (setq buffer-file-name "/p/sub/a.rb"
                  default-directory "/p/sub/")
            (cl-letf (((symbol-function 'flycheck-lsp--root) (lambda () "/p/sub/"))
                      ((symbol-function 'flycheck-lsp--command)
                       (lambda (_mode) '("ruby-lsp"))))
              (expect (flycheck-lsp--server-for-buffer)
                      :to-equal '("/p/sub/" "ruby-lsp")))))

        (it "shuts the buffer's server down and asks for a fresh check"
          (let ((stopped nil))
            (spy-on 'flycheck-lsp--shutdown-server
                    :and-call-fake (lambda (s) (push s stopped)))
            (spy-on 'flycheck-buffer-deferred)
            (flycheck-test--fake-server "/p/" '("ruby-lsp"))
            (flycheck-buttercup-with-temp-buffer
              (setq buffer-file-name "/p/a.rb" default-directory "/p/")
              (cl-letf (((symbol-function 'flycheck-lsp--root) (lambda () "/p/"))
                        ((symbol-function 'flycheck-lsp--command)
                         (lambda (_mode) '("ruby-lsp"))))
                (let ((flycheck-mode t))
                  (flycheck-lsp-restart-server))))
            (expect (length stopped) :to-equal 1)
            (expect (hash-table-count flycheck-lsp--servers) :to-equal 0)
            (expect 'flycheck-buffer-deferred :to-have-been-called)))

        (it "drops a server from the registry before stopping it"
          ;; Stopping pumps process output, and a check triggered by that
          ;; can register a fresh server under the same key.  Removing
          ;; afterwards would delete that one and leak its process.
          (let ((registered-during-shutdown nil))
            (spy-on 'flycheck-lsp--shutdown-server
                    :and-call-fake
                    (lambda (_server)
                      (setq registered-during-shutdown
                            (hash-table-count flycheck-lsp--servers))))
            (flycheck-test--fake-server "/p/" '("ruby-lsp"))
            (flycheck-lsp-shutdown-servers 'all)
            (expect registered-during-shutdown :to-equal 0)))

        (it "shuts down this project's servers, or all of them"
          (spy-on 'flycheck-lsp--shutdown-server)
          (flycheck-test--fake-server "/a/" '("rubocop" "--lsp"))
          (flycheck-test--fake-server "/a/nested/" '("ruff" "server"))
          (flycheck-test--fake-server "/b/" '("ruff" "server"))
          (flycheck-buttercup-with-temp-buffer
            (setq default-directory "/a/")
            (cl-letf (((symbol-function 'flycheck-lsp--root) (lambda () "/a/")))
              (flycheck-lsp-shutdown-servers)))
          ;; The nested root belongs to this project too.
          (expect 'flycheck-lsp--shutdown-server :to-have-been-called-times 2)
          (expect (hash-table-count flycheck-lsp--servers) :to-equal 1)
          (flycheck-lsp-shutdown-servers 'all)
          (expect (hash-table-count flycheck-lsp--servers) :to-equal 0))

        (it "refuses to restart when no server serves the buffer"
          ;; A server exists, but not for this buffer.
          (flycheck-test--fake-server "/other/" '("ruby-lsp"))
          (flycheck-buttercup-with-temp-buffer
            (setq buffer-file-name "/p/a.rb" default-directory "/p/")
            (cl-letf (((symbol-function 'flycheck-lsp--root) (lambda () "/p/"))
                      ((symbol-function 'flycheck-lsp--command)
                       (lambda (_mode) '("ruby-lsp"))))
              (expect (flycheck-lsp-restart-server) :to-throw 'user-error))))

        (it "prints a list buffer with a row per server"
          (flycheck-test--fake-server "/p/" '("ruby-lsp"))
          (unwind-protect
              (progn
                (flycheck-lsp-list-servers)
                (with-current-buffer "*Flycheck LSP servers*"
                  (expect (derived-mode-p 'flycheck-lsp-server-list-mode)
                          :to-be-truthy)
                  (expect (buffer-string) :to-match "ruby-lsp")))
            (kill-buffer "*Flycheck LSP servers*"))))

      (it "starts a remote server on the buffer's own host"
        ;; Without a file handler `make-process' ignores a remote
        ;; `default-directory' and starts here, against files the server
        ;; cannot see.
        (let (spawned)
          (spy-on 'make-process :and-call-fake
                  (lambda (&rest args) (setq spawned args) nil))
          (let ((default-directory "/ssh:host:/srv/app/"))
            (ignore-errors (flycheck-lsp--spawn "srv" '("rubocop" "--lsp")
                                                (current-buffer))))
          (expect (plist-get spawned :file-handler) :to-be t)
          (expect (plist-get spawned :command)
                  :to-equal (flycheck-lsp--remote-command
                             '("rubocop" "--lsp")))))

      (it "starts a local server plainly"
        (let (spawned)
          (spy-on 'make-process :and-call-fake
                  (lambda (&rest args) (setq spawned args) nil))
          (let ((default-directory "/srv/app/"))
            (ignore-errors (flycheck-lsp--spawn "srv" '("rubocop" "--lsp")
                                                (current-buffer))))
          (expect (plist-get spawned :file-handler) :to-be nil)
          (expect (plist-get spawned :command) :to-equal '("rubocop" "--lsp"))))

      (it "keys a document by the host its server runs on"
        ;; Every doc-key call site passes the server's host; if that
        ;; stopped happening, two hosts would share one key.
        (let* ((server (flycheck-lsp--server-create
                        :root "/ssh:host:/srv/app/" :command '("srv")))
               (doc (flycheck-lsp--document
                     server (flycheck-lsp--doc-key
                             "file:///srv/app/a.rb"
                             (flycheck-lsp--server-remote server)))))
          (expect (flycheck-lsp--server-remote server) :to-equal "/ssh:host:")
          (expect (hash-table-keys (flycheck-lsp--server-documents server))
                  :to-equal '("/ssh:host:/srv/app/a.rb"))
          (expect doc :to-be-truthy)))

      (it "closes a document only on its own host's server"
        ;; Two hosts can hold the same path.  Closing a buffer on one
        ;; must not tell the other to drop a file it is still serving.
        (let ((flycheck-lsp--servers (make-hash-table :test 'equal))
              (local (flycheck-lsp--server-create
                      :root "/srv/app/" :command '("srv")))
              (remote (flycheck-lsp--server-create
                       :root "/ssh:host:/srv/app/" :command '("srv"))))
          (flycheck-lsp--document local (flycheck-lsp--doc-key
                                         "file:///srv/app/a.rb"))
          (flycheck-lsp--document remote (flycheck-lsp--doc-key
                                          "file:///srv/app/a.rb" "/ssh:host:"))
          (puthash '("/srv/app/" "srv") local flycheck-lsp--servers)
          (puthash '("/ssh:host:/srv/app/" "srv") remote flycheck-lsp--servers)
          ;; Closing the local buffer leaves the remote server alone.
          (flycheck-buttercup-with-temp-buffer
            (setq buffer-file-name "/srv/app/a.rb")
            (flycheck-lsp--close-buffer))
          (expect (hash-table-count (flycheck-lsp--server-documents local))
                  :to-equal 0)
          (expect (hash-table-count (flycheck-lsp--server-documents remote))
                  :to-equal 1)
          ;; And the other way round: put the local one back, then close
          ;; the remote buffer and check only the remote server loses it.
          (flycheck-lsp--document local (flycheck-lsp--doc-key
                                         "file:///srv/app/a.rb"))
          (flycheck-buttercup-with-temp-buffer
            (setq buffer-file-name "/ssh:host:/srv/app/a.rb")
            (flycheck-lsp--close-buffer))
          (expect (hash-table-count (flycheck-lsp--server-documents remote))
                  :to-equal 0)
          (expect (hash-table-count (flycheck-lsp--server-documents local))
                  :to-equal 1)))

      (it "wraps a remote server so the framing survives"
        ;; TRAMP's shared shell turns a carriage return into a newline,
        ;; and the direct methods spawn through a pty, either of which
        ;; destroys the CRLF framing LSP headers use.
        (expect (flycheck-lsp--remote-command '("rubocop" "--lsp"))
                :to-equal '("sh" "-c" "stty raw > /dev/null 2>&1; exec rubocop --lsp"))
        ;; Arguments are quoted, so a path with a space survives.
        (expect (nth 2 (flycheck-lsp--remote-command '("srv" "a b")))
                :to-match (regexp-quote "a\\ b")))

      (it "tells a remote server nothing about our process"
        ;; A pid on this machine means nothing on the server's host, and
        ;; an older server may want the path rather than the URI.
        (let ((params (flycheck-lsp--initialize-params "/ssh:host:/srv/app/")))
          ;; The field is required by LSP, so it must be present and
          ;; null rather than simply absent.
          (expect (plist-member params :processId) :to-be-truthy)
          (expect (plist-get params :processId) :to-be nil)
          (expect (plist-get params :rootPath) :to-equal "/srv/app/")
          (expect (plist-get params :rootUri) :to-equal "file:///srv/app/"))
        (let ((params (flycheck-lsp--initialize-params "/srv/app/")))
          (expect (plist-get params :processId) :to-equal (emacs-pid))))

      (it "keeps a remote document apart from a local file of that path"
        ;; Both name "/etc/hosts" to their own server; the key is what
        ;; distinguishes them, so closing one must not close the other.
        (expect (flycheck-lsp--doc-key "file:///etc/hosts" "/ssh:host:")
                :not :to-equal (flycheck-lsp--doc-key "file:///etc/hosts")))

      (it "serves a remote buffer"
        ;; The server runs on the buffer's own host, so a remote buffer
        ;; is checked like a local one.  The local assertion is the control: without it a mode with no
        ;; configured server would pass this either way.
        (flycheck-buttercup-with-temp-buffer
          (delay-mode-hooks (ruby-mode))
          (let ((flycheck-lsp-mode t)
                (flycheck-executable-find #'identity))
            (setq buffer-file-name "/proj/a.rb" default-directory "/proj/")
            (expect (flycheck-lsp--enabled-p) :to-be-truthy)
            ;; A remote buffer is served too now: the server runs on
            ;; that host.
            (setq buffer-file-name "/ssh:host:/proj/a.rb"
                  default-directory "/ssh:host:/proj/")
            (expect (flycheck-lsp--enabled-p) :to-be-truthy))))

      (it "names a remote file as the server's host sees it"
        ;; The URI carries no host; the document key does, so a remote
        ;; buffer and a local file of the same path stay distinct.
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/ssh:host:/etc/hosts")
          (expect (flycheck-lsp--buffer-uri) :to-equal "file:///etc/hosts")
          (expect (flycheck-lsp--doc-key (flycheck-lsp--buffer-uri) "/ssh:host:")
                  :to-equal "/ssh:host:/etc/hosts")
          (expect (flycheck-lsp--doc-key (flycheck-lsp--buffer-uri))
                  :to-equal "/etc/hosts"))
        (flycheck-buttercup-with-temp-buffer
          (setq buffer-file-name "/etc/hosts")
          ;; Windows expands this against the current drive, so match the
          ;; tail rather than the whole URI.
          (let ((uri (flycheck-lsp--buffer-uri)))
            (expect uri :to-be-truthy)
            (expect (string-suffix-p "/etc/hosts" uri) :to-be-truthy))))

      (it "cleans up after a server program that is not installed"
        ;; The spawn has to fail inside the handler that tears the stderr
        ;; buffer down, and the caller has to see nil rather than a signal.
        (let ((before (length (buffer-list))))
          (expect (flycheck-lsp--start-server
                   default-directory '("flycheck-no-such-program-xyz"))
                  :to-be nil)
          (expect (length (buffer-list)) :to-equal before)))

      (it "leaves the current buffer's own process alone when that fails"
        ;; The spawn never happened, so `proc' is nil, and
        ;; `delete-process' reads nil as the current buffer's process.
        (let* ((buffer (generate-new-buffer "flycheck-lsp-victim"))
               (victim (start-process "flycheck-lsp-victim" buffer
                                      "sleep" "30")))
          (set-process-query-on-exit-flag victim nil)
          (unwind-protect
              (with-current-buffer buffer
                (expect (flycheck-lsp--start-server
                         default-directory '("flycheck-no-such-program-xyz"))
                        :to-be nil)
                (expect (process-live-p victim) :to-be-truthy))
            (ignore-errors (delete-process victim))
            (kill-buffer buffer))))

      (it "does not define an `lsp' checker, so it never clobbers lsp-mode's"
        ;; lsp-mode has owned the `lsp' checker name for years; a colliding
        ;; definition broke its integration (issue #2226), so the native
        ;; checker is named `flycheck-lsp' instead.
        (expect (flycheck-valid-checker-p 'lsp) :to-be nil)))))

;;; test-lsp.el ends here
