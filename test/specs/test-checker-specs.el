;;; test-checker-specs.el --- Flycheck Specs: the checker specs -*- lexical-binding: t; -*-

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

;; Specs about the checker specs.
;;
;; A language spec skips itself when its tool is missing, which is the
;; only way the suite can run anywhere.  The cost is that a spec which
;; quietly stops testing anything looks exactly like one that had nothing
;; to do, so the things that make a spec vacuous are worth asserting.

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)
(require 'seq)

(defconst flycheck-spec--language-spec-files
  (directory-files
   (expand-file-name "languages" (expand-file-name "specs" flycheck-test-directory))
   'full "\\.el\\'")
  "Every language spec file.")

(defun flycheck-spec--declared-checkers (file)
  "Return the checkers named by `def-checker-test' forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (names)
      (while (re-search-forward
              "(flycheck-buttercup-def-checker-test[ \t\n]+" nil t)
        (let ((form (read (current-buffer))))
          (setq names (append names (if (listp form) form (list form))))))
      names)))

(describe "The checker specs"

  (it "name checkers that exist"
    ;; The skip guard asks whether the named checker has a command before
    ;; looking for its executable.  A name that is not a checker at all
    ;; therefore skips nothing, and the spec runs with the tool missing.
    (let (bogus)
      (dolist (file flycheck-spec--language-spec-files)
        (dolist (checker (flycheck-spec--declared-checkers file))
          (unless (flycheck-valid-checker-p checker)
            (push (format "%s names `%s'"
                          (file-name-nondirectory file) checker)
                  bogus))))
      (expect (nreverse bogus) :to-equal nil)))

  (it "keep every recorded fixture attached to a checker"
    ;; A renamed checker leaves its recordings behind, where they look
    ;; like coverage and are never read again
    (let ((root flycheck-record-fixture-directory))
      (when (file-directory-p root)
        (let (orphans)
          (dolist (sub (directory-files root nil "\\`[^.]"))
            (let ((checker (intern (replace-regexp-in-string "_" "/" sub))))
              (unless (flycheck-valid-checker-p checker)
                (push sub orphans))))
          (expect (nreverse orphans) :to-equal nil)))))

  (it "record every recording under the resource it was made from"
    ;; The resource is read back off the recording's own path rather than
    ;; searched for by name, which several of them share
    (let (astray)
      (dolist (recording (directory-files-recursively
                          flycheck-record-fixture-directory "\\.txt\\'"))
        (let ((resource (flycheck-record-fixture-resource-of recording)))
          (unless (and resource
                       (file-exists-p (flycheck-record-fixture--resource resource)))
            (push (file-relative-name recording flycheck-record-fixture-directory)
                  astray))))
      (expect (nreverse astray) :to-equal nil))))

;;; test-checker-specs.el ends here
