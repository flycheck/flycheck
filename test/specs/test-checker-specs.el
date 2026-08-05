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

(defconst flycheck-spec--all-spec-files
  (directory-files-recursively
   (expand-file-name "specs" flycheck-test-directory) "\\.el\\'")
  "Every spec file, language specs included.")

(defun flycheck-spec--forms (file)
  "Return every form in FILE, read."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms form)
      (while (setq form (condition-case nil (read (current-buffer)) (end-of-file)))
        (push form forms))
      (nreverse forms))))

(defun flycheck-spec--dropped-body-p (form)
  "Whether FORM is a spec that builds a function and drops it.

`it' wraps whatever it is given in a function of its own, so a body
form that is itself a `lambda' is made, never called, and takes every
assertion inside it along.  The spec then passes without having run,
which is how 307 of them passed for six months.

Any such form counts, not only a body that is nothing else: the
mistake once lived in a macro that put the `lambda' after the forms
that skip the spec."
  (and (consp form)
       (memq (car form) '(it xit))
       (seq-some (lambda (subform)
                   (and (consp subform)
                        (memq (car subform) '(lambda closure function))))
                 (cddr form))))

(defun flycheck-spec--find-dropped-bodies (form)
  "Return the specs under FORM whose body would be dropped.

The spine is walked rather than mapped, because a spec file holds
plenty of dotted pairs and long lists, and neither survives `mapcar'
and recursion respectively."
  (cond
   ((not (consp form)) nil)
   ((flycheck-spec--dropped-body-p form) (list form))
   (t (let ((tail form) found)
        (while (consp tail)
          (setq found (nconc found (flycheck-spec--find-dropped-bodies (car tail)))
                tail (cdr tail)))
        found))))

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
    ;; A renamed or removed checker leaves its recordings behind, where
    ;; they look like coverage and are never read again.  An empty
    ;; directory is not that: the recorder makes one before it knows
    ;; whether the checker reads anything, and leaves it where it refused.
    (let ((root flycheck-record-fixture-directory))
      (when (file-directory-p root)
        (let (orphans)
          (dolist (sub (directory-files root nil "\\`[^.]"))
            (let ((dir (expand-file-name sub root))
                  (checker (intern (replace-regexp-in-string "_" "/" sub))))
              (when (and (file-directory-p dir)
                         (directory-files-recursively dir "\\.txt\\'")
                         (not (flycheck-valid-checker-p checker)))
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
      (expect (nreverse astray) :to-equal nil)))

  (it "give buttercup a body to run, rather than one to drop"
    ;; Written out, this is a spec whose whole body is `(lambda () ...)'
    (let (dropped)
      (dolist (file flycheck-spec--all-spec-files)
        (dolist (form (flycheck-spec--forms file))
          (dolist (spec (flycheck-spec--find-dropped-bodies form))
            (push (format "%s: %S" (file-name-nondirectory file) (cadr spec))
                  dropped))))
      (expect (nreverse dropped) :to-equal nil)))

  (it "reach the spec through the macros that define them"
    ;; The same mistake once lived in the macros instead, where no
    ;; reading of the spec files could see it: they expand to the `it'
    ;; whose body was being dropped.  Both of ours are checked by
    ;; expanding them, since that is the only place it shows.
    (let ((expansions
           (list (macroexpand-1
                  '(flycheck-buttercup-def-checker-test emacs-lisp emacs-lisp nil
                     (flycheck-spec--canary)))
                 (macroexpand-1
                  '(flycheck-buttercup-def-parse-test emacs-lisp "warnings.el"
                     '(1 1 error "x"))))))
      (dolist (expansion expansions)
        (expect (car expansion) :to-be 'it)
        (expect (flycheck-spec--dropped-body-p expansion) :to-be nil))
      ;; and the body given is the body that arrives
      (expect (car expansions) :to-contain '(flycheck-spec--canary)))))

;;; test-checker-specs.el ends here
