;;; test-project-diagnostics.el --- Flycheck Specs: Project diagnostics -*- lexical-binding: t; -*-

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

;; Specs for the project-wide diagnostics store and the error list's
;; project scope.

;;; Code:

(require 'flycheck-buttercup)

;; Track buffers created by the specs so they never leak into the suite.
(defvar flycheck-test--project-buffers nil)

(defun flycheck-test--project-key (dir)
  "Return the project key Flycheck computes for DIR.
Expanding here matches `flycheck--project-directory' on every
platform (Windows `expand-file-name' prepends the current drive)."
  (file-name-as-directory (expand-file-name dir)))

(defun flycheck-test--project-buffer (name dir)
  "Return a fresh buffer named NAME whose project key is DIR's."
  (let ((buffer (generate-new-buffer name)))
    (push buffer flycheck-test--project-buffers)
    (with-current-buffer buffer
      (setq default-directory (flycheck-test--project-key dir)))
    buffer))

(defun flycheck-test--project-errors (dir)
  "Return the diagnostics recorded for DIR's project."
  (flycheck--project-errors (flycheck-test--project-key dir)))

(describe "Project-wide diagnostics"

  (progn

    (before-each
      ;; The store is global; start every spec from a clean slate.
      (clrhash flycheck--project-error-store)
      (setq flycheck-test--project-buffers nil))

    (after-each
      (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
            flycheck-test--project-buffers))

    (describe "flycheck--project-directory"

      (it "falls back to default-directory when there is no project"
        (spy-on 'project-current :and-return-value nil)
        (let ((default-directory "/tmp/some/dir/"))
          (expect (flycheck--project-directory)
                  :to-equal (flycheck-test--project-key "/tmp/some/dir/"))))

      (it "uses the project root when a project is found"
        (spy-on 'project-current :and-return-value '(transient . "/proj/"))
        (spy-on 'project-root :and-return-value "/proj/root/")
        ;; The project root wins over `default-directory'.
        (let ((default-directory "/elsewhere/"))
          (expect (flycheck--project-directory)
                  :to-equal (flycheck-test--project-key "/proj/root/")))))

    (describe "the error store"

      ;; Key the store by default-directory so the specs control the project.
      (before-each
        (spy-on 'project-current :and-return-value nil))

      (it "records the full error set a buffer contributes"
        (with-current-buffer (flycheck-test--project-buffer " s" "/proj/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "a" :filename "/proj/x")
                 (flycheck-error-new-at 2 1 'warning "b" :filename "/proj/y")))
          (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 2)))

      (it "aggregates across buffers of the same project"
        (dotimes (i 2)
          (with-current-buffer (flycheck-test--project-buffer
                                (format " p%d" i) "/proj/")
            (flycheck--project-record-errors
             (list (flycheck-error-new-at (1+ i) 1 'error (format "e%d" i)
                                          :filename (format "/proj/f%d" i))))))
        (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 2))

      (it "keeps projects separate"
        (with-current-buffer (flycheck-test--project-buffer " a" "/proj-a/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "a" :filename "/proj-a/x"))))
        (with-current-buffer (flycheck-test--project-buffer " b" "/proj-b/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "b" :filename "/proj-b/x"))))
        (expect (length (flycheck-test--project-errors "/proj-a/")) :to-equal 1)
        (expect (length (flycheck-test--project-errors "/proj-b/")) :to-equal 1))

      (it "deduplicates identical errors for the same file"
        ;; e.g. one `cargo check' diagnostic reported once per open crate file.
        (dolist (name '(" d1" " d2"))
          (with-current-buffer (flycheck-test--project-buffer name "/proj/")
            (flycheck--project-record-errors
             (list (flycheck-error-new-at 5 3 'error "dup"
                                          :filename "/proj/shared"
                                          :checker 'x)))))
        (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 1))

      (it "does not merge identical errors from different fileless buffers"
        ;; Two unsaved buffers reporting the same diagnostic are distinct.
        (dolist (name '(" u1" " u2"))
          (with-current-buffer (flycheck-test--project-buffer name "/proj/")
            (flycheck--project-record-errors
             (list (flycheck-error-new-at 1 1 'error "syntax" :checker 'x)))))
        (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 2))

      (it "keeps duplicate errors reported within one buffer"
        ;; Deduplication is across buffers, not within a single one.
        (with-current-buffer (flycheck-test--project-buffer " dup" "/proj/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "twice"
                                        :filename "/proj/x" :checker 'x)
                 (flycheck-error-new-at 1 1 'error "twice"
                                        :filename "/proj/x" :checker 'x)))
          (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 2)))

      (it "drops errors without a line number"
        (with-current-buffer (flycheck-test--project-buffer " n" "/proj/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at nil nil 'error "no line"
                                        :filename "/proj/x")
                 (flycheck-error-new-at 3 1 'error "has line"
                                        :filename "/proj/x")))
          (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 1)))

      (it "accumulates successive checkers of a chain in one buffer"
        (with-current-buffer (flycheck-test--project-buffer " c" "/proj/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "a" :filename "/proj/x")))
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 2 1 'error "b" :filename "/proj/y")))
          (expect (length (flycheck-test--project-errors "/proj/")) :to-equal 2)))

      (it "forgets a buffer's contribution"
        (with-current-buffer (flycheck-test--project-buffer " g" "/proj/")
          (flycheck--project-record-errors
           (list (flycheck-error-new-at 1 1 'error "a" :filename "/proj/x")))
          (flycheck--project-forget-buffer)
          (expect (flycheck-test--project-errors "/proj/") :to-be nil)))

      (it "prunes and ignores errors of killed buffers"
        (let ((buffer (flycheck-test--project-buffer " gone" "/proj/")))
          (with-current-buffer buffer
            (flycheck--project-record-errors
             (list (flycheck-error-new-at 1 1 'error "a" :filename "/proj/x"))))
          (kill-buffer buffer)
          (expect (flycheck-test--project-errors "/proj/") :to-be nil)
          ;; The dead entry is pruned, not merely skipped.
          (expect (gethash buffer flycheck--project-error-store) :to-be nil))))

    (describe "capture during a syntax check"

      (before-each
        (spy-on 'project-current :and-return-value nil)
        (flycheck-define-generic-checker 'test-project-checker
          "A checker reporting an own-file and a cross-file error."
          :start (lambda (checker callback)
                   (funcall
                    callback 'finished
                    (list (flycheck-error-new-at 1 1 'error "own"
                                                 :checker checker
                                                 :filename (buffer-file-name))
                          (flycheck-error-new-at 1 1 'warning "cross"
                                                 :checker checker
                                                 :filename
                                                 (expand-file-name
                                                  "other.txt")))))
          :modes '(text-mode)))

      (after-each
        (setf (symbol-plist 'test-project-checker) nil))

      (it "keeps cross-file errors the buffer view drops"
        (let* ((file (make-temp-file "flycheck-proj-" nil ".txt" "hi\n"))
               (buffer (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buffer
                (text-mode)
                (let ((flycheck-checkers '(test-project-checker))
                      (flycheck-check-syntax-automatically nil))
                  (flycheck-mode)
                  (flycheck-buffer)
                  ;; The buffer view keeps only the own-file error.
                  (expect (length flycheck-current-errors) :to-equal 1)
                  ;; The project store keeps both.
                  (expect (length (flycheck--project-errors
                                   (flycheck--project-directory)))
                          :to-equal 2)))
            (kill-buffer buffer)
            (ignore-errors (delete-file file)))))

      (it "replaces a buffer's contribution on re-check"
        ;; The checker reports a distinct error each run, so a broken
        ;; retraction would leave both runs' errors in the store.
        (let* ((file (make-temp-file "flycheck-proj-" nil ".txt" "hi\n"))
               (buffer (find-file-noselect file))
               (run 0))
          (unwind-protect
              (with-current-buffer buffer
                (text-mode)
                (flycheck-define-generic-checker 'test-project-counter
                  "Reports one error whose message changes each run."
                  :start (lambda (checker callback)
                           (setq run (1+ run))
                           (funcall
                            callback 'finished
                            (list (flycheck-error-new-at
                                   1 1 'error (format "run-%d" run)
                                   :checker checker
                                   :filename (buffer-file-name)))))
                  :modes '(text-mode))
                (let ((flycheck-checkers '(test-project-counter))
                      (flycheck-check-syntax-automatically nil))
                  (flycheck-mode)
                  (flycheck-buffer)
                  (flycheck-buffer)
                  (let ((errors (flycheck--project-errors
                                 (flycheck--project-directory))))
                    ;; Only the second run's error remains.
                    (expect (length errors) :to-equal 1)
                    (expect (flycheck-error-message (car errors))
                            :to-equal "run-2"))))
            (setf (symbol-plist 'test-project-counter) nil)
            (kill-buffer buffer)
            (ignore-errors (delete-file file))))))

    (describe "error list project scope"

      (before-each
        (spy-on 'project-current :and-return-value nil))

      (it "defaults to buffer scope"
        (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
          (delay-mode-hooks (flycheck-error-list-mode))
          (expect flycheck-error-list-scope :to-be 'buffer)
          (kill-buffer flycheck-error-list-buffer)))

      (it "reads the project store in project scope"
        (let ((source (flycheck-test--project-buffer " source" "/proj/")))
          (with-current-buffer source
            (flycheck--project-record-errors
             (list (flycheck-error-new-at 1 1 'error "own" :filename "/proj/x")
                   (flycheck-error-new-at 2 1 'warning "cross"
                                          :filename "/proj/y"))))
          (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
            (delay-mode-hooks (flycheck-error-list-mode))
            (setq flycheck-error-list-source-buffer source)
            (setq flycheck-error-list-scope 'project)
            (expect (length (flycheck-error-list-current-errors)) :to-equal 2)
            (kill-buffer flycheck-error-list-buffer))))

      (it "toggles scope with flycheck-error-list-toggle-scope"
        (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
          (delay-mode-hooks (flycheck-error-list-mode))
          (expect flycheck-error-list-scope :to-be 'buffer)
          (flycheck-error-list-toggle-scope)
          (expect flycheck-error-list-scope :to-be 'project)
          (flycheck-error-list-toggle-scope)
          (expect flycheck-error-list-scope :to-be 'buffer)
          (kill-buffer flycheck-error-list-buffer))))))

(provide 'test-project-diagnostics)

;;; test-project-diagnostics.el ends here
