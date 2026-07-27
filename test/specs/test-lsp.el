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
    (it "strips the leading slash of a Windows drive URI"
      (expect (flycheck-lsp--uri-to-path "file:///c:/x/y.rb")
              :to-equal "c:/x/y.rb"))
    (it "returns a non-file URI unchanged"
      (expect (flycheck-lsp--uri-to-path "untitled:1") :to-equal "untitled:1")))

  (describe "flycheck-lsp--path-to-uri"
    (it "round-trips with uri-to-path"
      (expect (flycheck-lsp--uri-to-path
               (flycheck-lsp--path-to-uri "/tmp/a b.rb"))
              :to-equal "/tmp/a b.rb")))

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
                :to-equal 10)))))

;;; test-lsp.el ends here
