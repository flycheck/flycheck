;;; test-quick-fixes.el --- Flycheck Specs: Quick fixes -*- lexical-binding: t; -*-

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

;; Specs for the quick-fix API: the fix data model, the applicator, the
;; at-point command, and fix extraction in the eslint/SARIF/rustc parsers.

;;; Code:

(require 'flycheck-buttercup)

(defun flycheck-test--edit (line col end-line end-col replacement)
  "Return a `flycheck-fix' with a single edit."
  (flycheck-fix-new
   :edits (list (flycheck-fix-edit-new
                 :line line :column col
                 :end-line end-line :end-column end-col
                 :replacement replacement))))

(describe "Quick fixes"

  (describe "flycheck-error-resolve-fix"
    (it "returns a stored fix as-is"
      (let* ((fix (flycheck-test--edit 1 1 1 2 "x"))
             (err (flycheck-error-new-at 1 1 'error "e" :fix fix)))
        (expect (flycheck-error-resolve-fix err) :to-be fix)))
    (it "calls a lazy provider with the error and returns its fix"
      (let* ((fix (flycheck-test--edit 1 1 1 2 "x"))
             (seen nil)
             (err (flycheck-error-new-at
                   1 1 'error "e"
                   :fix (lambda (e) (setq seen e) fix))))
        (expect (flycheck-error-resolve-fix err) :to-be fix)
        (expect seen :to-be err)))
    (it "returns nil when the provider yields no fix"
      (let ((err (flycheck-error-new-at 1 1 'error "e" :fix (lambda (_) nil))))
        (expect (flycheck-error-resolve-fix err) :to-be nil))))

  (describe "flycheck-fix-edit-new-at-pos"
    (it "converts buffer positions to one-based lines and columns"
      (flycheck-buttercup-with-temp-buffer
        (insert "first line\nsecond line\n")
        ;; "second" is positions 12-18: line 2, columns 1-7 right-open
        (let ((edit (flycheck-fix-edit-new-at-pos 12 18 "next")))
          (expect (flycheck-fix-edit-line edit) :to-equal 2)
          (expect (flycheck-fix-edit-column edit) :to-equal 1)
          (expect (flycheck-fix-edit-end-line edit) :to-equal 2)
          (expect (flycheck-fix-edit-end-column edit) :to-equal 7)
          (expect (flycheck-fix-edit-replacement edit) :to-equal "next"))))

    (it "builds an insertion from an empty region"
      (flycheck-buttercup-with-temp-buffer
        (insert "ab\n")
        (let ((edit (flycheck-fix-edit-new-at-pos 2 2 "x")))
          (expect (flycheck-fix-edit-line edit) :to-equal 1)
          (expect (flycheck-fix-edit-column edit) :to-equal 2)
          (expect (flycheck-fix-edit-end-line edit) :to-equal 1)
          (expect (flycheck-fix-edit-end-column edit) :to-equal 2))))

    (it "builds the same edit flycheck-fix-edit-new would"
      (flycheck-buttercup-with-temp-buffer
        (insert "first line\nsecond line\n")
        (expect (flycheck-fix-edit-new-at-pos 12 18 "next")
                :to-equal (flycheck-fix-edit-new
                           :line 2 :column 1 :end-line 2 :end-column 7
                           :replacement "next"))))

    (it "applies like any other edit"
      (flycheck-buttercup-with-temp-buffer
        (insert "first line\nsecond line\n")
        (let ((fix (flycheck-fix-new
                    :edits (list (flycheck-fix-edit-new-at-pos 12 18 "next"))
                    :tick (buffer-chars-modified-tick))))
          (flycheck-apply-fix fix)
          (expect (buffer-string) :to-equal "first line\nnext line\n")))))

  (describe "flycheck-error-known-fix-p"
    (it "is true for a stored fix"
      (let* ((fix (flycheck-test--edit 1 1 1 2 "x"))
             (err (flycheck-error-new-at 1 1 'error "e" :fix fix)))
        (expect (flycheck-error-known-fix-p err) :to-be-truthy)))
    (it "is false for a lazy provider, which may yield nothing"
      (let ((err (flycheck-error-new-at 1 1 'error "e" :fix (lambda (_) nil))))
        (expect (flycheck-error-known-fix-p err) :to-be nil)))
    (it "is false for an error with no fix"
      (expect (flycheck-error-known-fix-p (flycheck-error-new-at 1 1 'error "e"))
              :to-be nil)))

  (describe "refusing an edit whose coordinates make no sense"
    ;; Tools do emit ranges that cannot be resolved.  Applying one anyway
    ;; edits somewhere the checker never pointed at, which is the outcome
    ;; a feature that writes to the buffer has to refuse.
    (defun flycheck-test--refuses (label fix)
      (with-temp-buffer
        (insert "abcdef\n")
        (expect (flycheck-apply-fix fix) :to-throw 'user-error)
        ;; and nothing was touched on the way to refusing
        (expect (buffer-string) :to-equal "abcdef\n")
        (ignore label)))

    (it "refuses a column of zero, where Flycheck counts from one"
      (flycheck-test--refuses "zero column" (flycheck-test--edit 1 0 1 2 "X")))

    (it "refuses a line that is not a line"
      (flycheck-test--refuses "negative" (flycheck-test--edit -1 1 -1 2 "X"))
      (flycheck-test--refuses "nil" (flycheck-test--edit nil 1 nil 2 "X")))

    (it "refuses an end that comes before its start"
      (flycheck-test--refuses "same line" (flycheck-test--edit 1 5 1 2 "X"))
      (flycheck-test--refuses "earlier line" (flycheck-test--edit 2 1 1 1 "X")))

    (it "still allows a position past the end, which appends"
      ;; A fix for a missing trailing newline legitimately names one
      (with-temp-buffer
        (insert "ab\n")
        (flycheck-apply-fix (flycheck-test--edit 9 1 9 1 "X"))
        (expect (buffer-string) :to-equal "ab\nX")))

    (it "still allows a column past the end of its line"
      (with-temp-buffer
        (insert "ab\n")
        (flycheck-apply-fix (flycheck-test--edit 1 1 1 99 "X"))
        (expect (buffer-string) :to-equal "X\n"))))

  (describe "flycheck-apply-fix"

    (it "replaces the edit's region"
      (with-temp-buffer
        (insert "first\nhelo world\nthird\n")
        (flycheck-apply-fix (flycheck-test--edit 2 1 2 5 "hello"))
        (expect (buffer-string) :to-equal "first\nhello world\nthird\n")))

    (it "inserts when the region is empty"
      (with-temp-buffer
        (insert "var x = 1\n")
        (flycheck-apply-fix (flycheck-test--edit 1 10 1 10 ";"))
        (expect (buffer-string) :to-equal "var x = 1;\n")))

    (it "deletes when the replacement is empty"
      (with-temp-buffer
        (insert "a  b\n")
        (flycheck-apply-fix (flycheck-test--edit 1 2 1 4 ""))
        (expect (buffer-string) :to-equal "ab\n")))

    (it "applies several edits from the bottom up"
      ;; A later edit must not shift the positions of an earlier one.
      (with-temp-buffer
        (insert "aaa\nbbb\nccc\n")
        (flycheck-apply-fix
         (flycheck-fix-new
          :edits (list (flycheck-fix-edit-new
                        :line 1 :column 1 :end-line 1 :end-column 4
                        :replacement "XXXXX")
                       (flycheck-fix-edit-new
                        :line 3 :column 1 :end-line 3 :end-column 4
                        :replacement "Z"))))
        (expect (buffer-string) :to-equal "XXXXX\nbbb\nZ\n")))

    (it "applies as a single undoable change"
      (with-temp-buffer
        (buffer-enable-undo)
        (insert "helo\n")
        (undo-boundary)
        (flycheck-apply-fix (flycheck-test--edit 1 1 1 5 "hello"))
        (expect (buffer-string) :to-equal "hello\n")
        (primitive-undo 1 buffer-undo-list)
        (expect (buffer-string) :to-equal "helo\n")))

    (it "refuses to edit a read-only buffer"
      (with-temp-buffer
        (insert "helo\n")
        (setq buffer-read-only t)
        (expect (flycheck-apply-fix (flycheck-test--edit 1 1 1 5 "hello"))
                :to-throw 'user-error)))

    (it "refuses to apply in a dead buffer"
      (let ((buffer (generate-new-buffer " gone")))
        (kill-buffer buffer)
        (expect (flycheck-apply-fix (flycheck-test--edit 1 1 1 1 "x") buffer)
                :to-throw 'user-error)))

    (it "applies when the buffer is unchanged since the check"
      (with-temp-buffer
        (insert "helo\n")
        (flycheck-apply-fix
         (flycheck-fix-new
          :tick (buffer-chars-modified-tick)
          :edits (list (flycheck-fix-edit-new
                        :line 1 :column 1 :end-line 1 :end-column 5
                        :replacement "hello"))))
        (expect (buffer-string) :to-equal "hello\n")))

    (it "refuses a fix when the buffer changed since the check"
      ;; The tick guard is what stops an apply after the user edits from
      ;; corrupting the buffer, whatever the text happens to be.
      (with-temp-buffer
        (insert "helo\n")
        (let ((fix (flycheck-fix-new
                    :tick (buffer-chars-modified-tick)
                    :edits (list (flycheck-fix-edit-new
                                  :line 1 :column 1 :end-line 1 :end-column 5
                                  :replacement "hello")))))
          (goto-char (point-max))
          (insert "more\n")            ; bumps `buffer-chars-modified-tick'
          (expect (flycheck-apply-fix fix) :to-throw 'user-error))))

    (it "refuses to apply the same fix twice"
      ;; Applying the fix bumps the tick, so the stale second apply is refused
      ;; even though the error and its fix linger until the async re-check.
      (with-temp-buffer
        (insert "helo\n")
        (let ((fix (flycheck-fix-new
                    :tick (buffer-chars-modified-tick)
                    :edits (list (flycheck-fix-edit-new
                                  :line 1 :column 1 :end-line 1 :end-column 5
                                  :replacement "hello")))))
          (flycheck-apply-fix fix)
          (expect (buffer-string) :to-equal "hello\n")
          (expect (flycheck-apply-fix fix) :to-throw 'user-error))))

    (it "refuses a fix whose own edits overlap"
      (with-temp-buffer
        (insert "abcdef\n")
        (expect (flycheck-apply-fix
                 (flycheck-fix-new
                  :edits (list (flycheck-fix-edit-new
                                :line 1 :column 1 :end-line 1 :end-column 4
                                :replacement "X")
                               (flycheck-fix-edit-new
                                :line 1 :column 3 :end-line 1 :end-column 6
                                :replacement "Y"))))
                :to-throw 'user-error)
        (expect (buffer-string) :to-equal "abcdef\n"))))

  (describe "flycheck-fix-error-at-point"

    (it "applies the fix of the fixable error at point"
      (flycheck-buttercup-with-temp-buffer
        (insert "helo world\n")
        (goto-char (point-min))
        (let* ((err (flycheck-error-new-at
                     1 1 'error "typo"
                     :end-line 1 :end-column 5
                     :fix (flycheck-test--edit 1 1 1 5 "hello")))
               (flycheck-current-errors (list err)))
          (spy-on 'flycheck-overlay-errors-at :and-return-value (list err))
          (flycheck-fix-error-at-point)
          (expect (buffer-string) :to-equal "hello world\n"))))

    (it "resolves and applies a lazy fix provider"
      (flycheck-buttercup-with-temp-buffer
        (insert "helo world\n")
        (goto-char (point-min))
        (let ((err (flycheck-error-new-at
                    1 1 'error "typo" :buffer (current-buffer)
                    :fix (lambda (_) (flycheck-test--edit 1 1 1 5 "hello")))))
          (spy-on 'flycheck-overlay-errors-at :and-return-value (list err))
          (flycheck-fix-error-at-point)
          (expect (buffer-string) :to-equal "hello world\n"))))

    (it "signals when a lazy provider yields no fix"
      (flycheck-buttercup-with-temp-buffer
        (insert "helo\n")
        (let ((err (flycheck-error-new-at 1 1 'error "typo"
                                          :buffer (current-buffer)
                                          :fix (lambda (_) nil))))
          (spy-on 'flycheck-overlay-errors-at :and-return-value (list err))
          (expect (flycheck-fix-error-at-point) :to-throw 'user-error))))

    (it "applies a fix whose error is for the current file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (let ((err (flycheck-error-new-at 1 1 'error "x"
                                          :filename "/proj/a.el"
                                          :buffer (current-buffer))))
          (expect (flycheck--error-fix-buffer err) :to-be (current-buffer)))))

    (it "refuses a fix whose error is for another file"
      ;; A cross-file error's positions are for another file; applying its fix
      ;; in this buffer would corrupt it.
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name "/proj/a.el")
        (let ((err (flycheck-error-new-at 1 1 'error "x"
                                          :filename "/proj/b.el"
                                          :buffer (current-buffer))))
          (expect (flycheck--error-fix-buffer err) :to-be nil))))

    (it "signals when no error at point has a fix"
      (flycheck-buttercup-with-temp-buffer
        (insert "helo\n")
        (let ((err (flycheck-error-new-at 1 1 'error "no fix")))
          (spy-on 'flycheck-overlay-errors-at :and-return-value (list err))
          (expect (flycheck-fix-error-at-point) :to-throw 'user-error)))))

  (describe "flycheck-apply-fixes"

    (it "applies several non-overlapping fixes in one pass"
      (flycheck-buttercup-with-temp-buffer
        (insert "aaa bbb ccc\n")
        (expect (flycheck-apply-fixes
                 (list (flycheck-test--edit 1 1 1 4 "X")
                       (flycheck-test--edit 1 9 1 12 "Y")))
                :to-equal 2)
        (expect (buffer-string) :to-equal "X bbb Y\n")))

    (it "skips a fix that conflicts with another, applying just one"
      (flycheck-buttercup-with-temp-buffer
        (insert "aaa bbb\n")
        (expect (flycheck-apply-fixes
                 (list (flycheck-test--edit 1 1 1 4 "X")
                       (flycheck-test--edit 1 1 1 4 "Z")))
                :to-equal 1)))

    (it "prefers many small fixes over one wide fix that conflicts"
      ;; classic interval scheduling: spans [1,10], [2,3], [4,5] -> keep the
      ;; two small ones, not the single wide one
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdefghij\n")
        (expect (flycheck-apply-fixes
                 (list (flycheck-test--edit 1 1 1 10 "A")
                       (flycheck-test--edit 1 2 1 3 "B")
                       (flycheck-test--edit 1 4 1 5 "C")))
                :to-equal 2)))

    (it "refuses when a fix is stale"
      (flycheck-buttercup-with-temp-buffer
        (insert "aaa\n")
        (let ((fix (flycheck-fix-new
                    :tick (buffer-chars-modified-tick)
                    :edits (list (flycheck-fix-edit-new
                                  :line 1 :column 1 :end-line 1 :end-column 4
                                  :replacement "X")))))
          (insert "zzz")            ; mutate the buffer, advancing the tick
          (expect (flycheck-apply-fixes (list fix)) :to-throw 'user-error)))))

  (describe "flycheck-fix-all-errors"

    (it "applies the fixes of every fixable error in the buffer"
      (flycheck-buttercup-with-temp-buffer
        (insert "aaa bbb ccc\n")
        (let ((flycheck-current-errors
               (list (flycheck-error-new-at 1 1 'error "a"
                                            :buffer (current-buffer)
                                            :fix (flycheck-test--edit 1 1 1 4 "X"))
                     (flycheck-error-new-at 1 5 'warning "b" ; no fix
                                            :buffer (current-buffer))
                     (flycheck-error-new-at 1 9 'error "c"
                                            :buffer (current-buffer)
                                            :fix (flycheck-test--edit 1 9 1 12 "Y")))))
          (flycheck-fix-all-errors)
          (expect (buffer-string) :to-equal "X bbb Y\n"))))

    (it "signals when the buffer has no fixable errors"
      (flycheck-buttercup-with-temp-buffer
        (insert "aaa\n")
        (let ((flycheck-current-errors
               (list (flycheck-error-new-at 1 1 'error "a"
                                            :buffer (current-buffer)))))
          (expect (flycheck-fix-all-errors) :to-throw 'user-error)))))

  (describe "fix extraction in parsers"

    (it "reads an ESLint fix"
      (let ((json "[{\"filePath\":\"/f.js\",\"messages\":[\
{\"ruleId\":\"semi\",\"severity\":2,\"message\":\"Missing semicolon.\",\
\"line\":1,\"column\":10,\"endLine\":1,\"endColumn\":11,\
\"fix\":{\"range\":[9,9],\"text\":\";\"}}]}]"))
        (with-temp-buffer
          (insert "var x = 1\n")
          (let* ((err (car (flycheck-parse-eslint json 'javascript-eslint
                                                  (current-buffer))))
                 (fix (flycheck-error-fix err))
                 (edit (car (flycheck-fix-edits fix))))
            ;; Character offset 9 maps to line 1, column 10.
            (expect (flycheck-fix-edit-line edit) :to-equal 1)
            (expect (flycheck-fix-edit-column edit) :to-equal 10)
            (expect (flycheck-fix-edit-replacement edit) :to-equal ";")))))

    (it "reads a SARIF fix"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"rules\":[]}},\"results\":[\
{\"ruleId\":\"R1\",\"level\":\"warning\",\"message\":{\"text\":\"bad\"},\
\"locations\":[{\"physicalLocation\":{\"artifactLocation\":{\"uri\":\"f.txt\"},\
\"region\":{\"startLine\":3,\"startColumn\":2,\"endLine\":3,\"endColumn\":6}}}],\
\"fixes\":[{\"description\":{\"text\":\"do it\"},\"artifactChanges\":[\
{\"artifactLocation\":{\"uri\":\"f.txt\"},\"replacements\":[{\"deletedRegion\":\
{\"startLine\":3,\"startColumn\":2,\"endLine\":3,\"endColumn\":6},\
\"insertedContent\":{\"text\":\"good\"}}]}]}]}]}]}"))
        (let* ((err (car (flycheck-parse-sarif sarif 'checker (current-buffer))))
               (fix (flycheck-error-fix err))
               (edit (car (flycheck-fix-edits fix))))
          (expect (flycheck-fix-description fix) :to-equal "do it")
          (expect (flycheck-fix-edit-line edit) :to-equal 3)
          (expect (flycheck-fix-edit-column edit) :to-equal 2)
          (expect (flycheck-fix-edit-end-column edit) :to-equal 6)
          (expect (flycheck-fix-edit-replacement edit) :to-equal "good"))))

    (it "reads a machine-applicable rustc suggestion"
      (let ((diag "{\"message\":\"help\",\"code\":{\"code\":\"E1\"},\"level\":\"warning\",\
\"spans\":[{\"file_name\":\"a.rs\",\"is_primary\":true,\
\"line_start\":2,\"line_end\":2,\"column_start\":1,\"column_end\":4,\
\"suggested_replacement\":\"bar\",\"suggestion_applicability\":\"MachineApplicable\"}],\
\"children\":[]}"))
        (let* ((err (car (flycheck-parse-rustc-diagnostic
                          (car (flycheck-parse-json diag)) 'checker
                          (current-buffer))))
               (fix (flycheck-error-fix err))
               (edit (car (flycheck-fix-edits fix))))
          (expect (flycheck-fix-edit-line edit) :to-equal 2)
          (expect (flycheck-fix-edit-replacement edit) :to-equal "bar"))))

    (it "applies a multi-span rustc suggestion in full"
      ;; A child suggestion may touch several spans (insert `(' and `)');
      ;; both edits must land, in the right order.
      (let ((diag "{\"message\":\"m\",\"code\":{\"code\":\"E\"},\"level\":\"warning\",\
\"spans\":[{\"file_name\":\"a.rs\",\"is_primary\":true,\
\"line_start\":1,\"line_end\":1,\"column_start\":1,\"column_end\":2}],\
\"children\":[{\"message\":\"help\",\"spans\":[\
{\"file_name\":\"a.rs\",\"line_start\":1,\"column_start\":1,\"line_end\":1,\"column_end\":1,\
\"suggested_replacement\":\"(\",\"suggestion_applicability\":\"MachineApplicable\"},\
{\"file_name\":\"a.rs\",\"line_start\":1,\"column_start\":5,\"line_end\":1,\"column_end\":5,\
\"suggested_replacement\":\")\",\"suggestion_applicability\":\"MachineApplicable\"}]}]}"))
        (with-temp-buffer
          (insert "abcd\n")
          (let* ((errors (flycheck-parse-rustc-diagnostic
                          (car (flycheck-parse-json diag)) 'checker
                          (current-buffer)))
                 (fix (seq-some #'flycheck-error-fix errors)))
            (expect (length (flycheck-fix-edits fix)) :to-equal 2)
            (flycheck-apply-fix fix)
            (expect (buffer-string) :to-equal "(abcd)\n")))))

    (it "converts ESLint UTF-16 offsets past an astral character"
      ;; An emoji is one Emacs character but two UTF-16 code units, so an
      ;; offset after it must map back one character to the left.
      (let ((json "[{\"messages\":[{\"ruleId\":\"semi\",\"severity\":2,\
\"message\":\"m\",\"line\":1,\"column\":1,\"fix\":{\"range\":[5,5],\"text\":\";\"}}]}]"))
        (with-temp-buffer
          ;; "x=" then an astral char (2 code units) then "y" -> UTF-16 offset
          ;; 5 is right after "y", i.e. Emacs position 5 (treating the emoji as
          ;; one character it would wrongly land at position 6).
          (insert "x=")
          (insert (char-to-string #x1F600))
          (insert "y\n")
          (let* ((err (car (flycheck-parse-eslint json 'javascript-eslint
                                                  (current-buffer))))
                 (edit (car (flycheck-fix-edits (flycheck-error-fix err)))))
            ;; Position 5 is line 1, column 5 (x = emoji y | ).
            (expect (flycheck-fix-edit-column edit) :to-equal 5)))))

    (it "drops a rustc suggestion span in another file"
      ;; A suggestion reaching into a different file must not edit this buffer.
      (let ((diag "{\"message\":\"m\",\"code\":{\"code\":\"E\"},\"level\":\"warning\",\
\"spans\":[{\"file_name\":\"a.rs\",\"is_primary\":true,\
\"line_start\":1,\"line_end\":1,\"column_start\":1,\"column_end\":2}],\
\"children\":[{\"message\":\"help\",\"spans\":[\
{\"file_name\":\"other.rs\",\"line_start\":9,\"column_start\":1,\"line_end\":9,\"column_end\":2,\
\"suggested_replacement\":\"z\",\"suggestion_applicability\":\"MachineApplicable\"}]}]}"))
        (let ((errors (flycheck-parse-rustc-diagnostic
                       (car (flycheck-parse-json diag)) 'checker
                       (current-buffer))))
          (expect (seq-some #'flycheck-error-fix errors) :to-be nil))))

    (it "drops a SARIF fix change for another file"
      (let ((sarif "{\"runs\":[{\"tool\":{\"driver\":{\"rules\":[]}},\"results\":[\
{\"ruleId\":\"R1\",\"level\":\"warning\",\"message\":{\"text\":\"bad\"},\
\"locations\":[{\"physicalLocation\":{\"artifactLocation\":{\"uri\":\"f.txt\"},\
\"region\":{\"startLine\":3,\"startColumn\":2}}}],\
\"fixes\":[{\"artifactChanges\":[\
{\"artifactLocation\":{\"uri\":\"other.txt\"},\"replacements\":[{\"deletedRegion\":\
{\"startLine\":3,\"startColumn\":2,\"endLine\":3,\"endColumn\":6},\
\"insertedContent\":{\"text\":\"good\"}}]}]}]}]}]}"))
        (let ((err (car (flycheck-parse-sarif sarif 'checker (current-buffer)))))
          (expect (flycheck-error-fix err) :to-be nil))))

    (it "ignores a non-machine-applicable rustc suggestion"
      (let ((diag "{\"message\":\"help\",\"code\":{\"code\":\"E1\"},\"level\":\"warning\",\
\"spans\":[{\"file_name\":\"a.rs\",\"is_primary\":true,\
\"line_start\":2,\"line_end\":2,\"column_start\":1,\"column_end\":4,\
\"suggested_replacement\":\"bar\",\"suggestion_applicability\":\"MaybeIncorrect\"}],\
\"children\":[]}"))
        (let ((err (car (flycheck-parse-rustc-diagnostic
                         (car (flycheck-parse-json diag)) 'checker
                         (current-buffer)))))
          (expect (flycheck-error-fix err) :to-be nil))))))

(defvar flycheck-test--fix-dir nil
  "The directory of files the cross-file fix specs edit.")

(defun flycheck-test--fix-file (name content)
  "Write CONTENT to NAME in `flycheck-test--fix-dir' and return the file."
  (let ((file (expand-file-name name flycheck-test--fix-dir)))
    (with-temp-file file (insert content))
    file))

(defun flycheck-test--record-run (err time)
  "Pretend a project run reported ERR, having started at TIME."
  (let* ((key (cons flycheck-test--fix-dir 'test-run))
         (state (gethash key flycheck--project-runs)))
    (puthash key (list :process nil
                       :errors (cons err (plist-get state :errors))
                       :time time)
             flycheck--project-runs)))

(defun flycheck-test--fix-error (file &optional fix)
  "Return an error about FILE with FIX, as a project checker reports one.

The error is recorded against a run that started just now, which is what
bounds the fix of a file the checker read from disk."
  (let ((err (flycheck-error-new-at
              1 1 'error "typo" :filename file :buffer nil
              :fix (or fix (flycheck-test--edit 1 1 1 5 "hello")))))
    (flycheck-test--record-run err (current-time))
    err))

(describe "Cross-file fixes"

  (before-each
    (setq flycheck-test--fix-dir
          (file-name-as-directory
           (file-truename (make-temp-file "flycheck-fix" t)))))

  (after-each
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (when (string-prefix-p flycheck-test--fix-dir file)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))))
    (clrhash flycheck--project-runs)
    (ignore-errors (delete-directory flycheck-test--fix-dir t)))

  (describe "flycheck--error-fix-target"

    (it "resolves an error about its own buffer's file to that buffer"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (flycheck-test--fix-file "a.el" "helo\n"))
        (let ((err (flycheck-error-new-at 1 1 'error "x")))
          (expect (flycheck--error-fix-target err) :to-be (current-buffer)))))

    (it "resolves a cross-file error to the buffer already visiting its file"
      (let* ((file (flycheck-test--fix-file "b.el" "helo\n"))
             (buffer (find-file-noselect file))
             (err (flycheck-test--fix-error file)))
        (expect (flycheck--error-fix-target err) :to-be buffer)))

    (it "opens the file only when asked to"
      (let* ((file (flycheck-test--fix-file "b.el" "helo\n"))
             (err (flycheck-test--fix-error file)))
        (expect (flycheck--error-fix-target err) :to-be nil)
        (expect (buffer-file-name (flycheck--error-fix-target err 'visit))
                :to-equal file)))

    (it "resolves nothing for a file that is not there"
      (let ((err (flycheck-test--fix-error
                  (expand-file-name "gone.el" flycheck-test--fix-dir))))
        (expect (flycheck--error-fix-target err 'visit) :to-be nil))))

  (describe "flycheck--apply-error-fix"

    (it "applies a fix in the file the error names, opening it"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file)))
        (expect (flycheck--apply-error-fix err 'visit) :to-be-truthy)
        (with-current-buffer (find-buffer-visiting file)
          (expect (buffer-string) :to-equal "hello world\n")
          ;; Left for review and undo rather than written out.
          (expect (buffer-modified-p) :to-be t))))

    (it "refuses to open a file it was not allowed to visit"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file)))
        (expect (flycheck--apply-error-fix err) :to-throw 'user-error)
        (expect (find-buffer-visiting file) :to-be nil)))

    (it "refuses a fix for a buffer with unsaved changes"
      ;; What the checker read is no longer what the buffer holds, so the
      ;; fix's line and column numbers may point anywhere.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (buffer (find-file-noselect file))
             (err (flycheck-test--fix-error file)))
        (with-current-buffer buffer (goto-char (point-min)) (insert "\n"))
        (expect (flycheck--apply-error-fix err 'visit) :to-throw 'user-error)
        (with-current-buffer buffer
          (expect (buffer-string) :to-equal "\nhelo world\n"))))

    (it "refuses a fix whose file changed under its buffer"
      ;; Standing in for the file being rewritten behind the buffer's back,
      ;; which a file system with a coarse clock would not always show.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (buffer (find-file-noselect file))
             (err (flycheck-test--fix-error file)))
        (with-current-buffer buffer (set-visited-file-modtime '(0 0)))
        (expect (flycheck--apply-error-fix err 'visit) :to-throw 'user-error)
        (with-current-buffer buffer
          (expect (buffer-string) :to-equal "helo world\n"))))

    (it "refuses a fix whose file was written after the check ran"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file)))
        (flycheck-test--record-run err (time-add (current-time) -60))
        (expect (flycheck--apply-error-fix err 'visit) :to-throw 'user-error)))

    (it "refuses a fix nothing says the state of"
      ;; Neither a run nor a live buffer stands behind this one, so there is
      ;; no telling whether its positions still describe the file.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-error-new-at
                   1 1 'error "typo" :filename file :buffer nil
                   :fix (flycheck-test--edit 1 1 1 5 "hello"))))
        (expect (condition-case signalled
                    (flycheck--apply-error-fix err 'visit)
                  (user-error (error-message-string signalled)))
                :to-match "Nothing says which state")))

    (it "refuses a buffer check's fix for a file written since that check"
      ;; rustc names a suggestion in another crate file this way: the fix
      ;; carries the checked buffer's tick, which says nothing about the
      ;; file it edits, so the check's time is what bounds it.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (checked (generate-new-buffer " checked"))
             (err (with-current-buffer checked
                    (setq flycheck--syntax-check-start-time
                          (time-add (current-time) -60))
                    (flycheck-error-new-at
                     1 1 'error "typo" :filename file
                     :fix (flycheck-test--edit 1 1 1 5 "hello")))))
        (unwind-protect
            (expect (flycheck--apply-error-fix err 'visit) :to-throw
                    'user-error)
          (kill-buffer checked))))

    (it "applies a buffer check's fix for a file untouched since that check"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (checked (generate-new-buffer " checked"))
             (err (with-current-buffer checked
                    (setq flycheck--syntax-check-start-time
                          (time-add (current-time) 60))
                    (flycheck-error-new-at
                     1 1 'error "typo" :filename file
                     :fix (flycheck-test--edit 1 1 1 5 "hello")))))
        (unwind-protect
            (progn
              (expect (flycheck--apply-error-fix err 'visit) :to-be-truthy)
              (with-current-buffer (find-buffer-visiting file)
                (expect (buffer-string) :to-equal "hello world\n")))
          (kill-buffer checked))))

    (it "applies a fix a provider computed against the file just now"
      ;; A lazy provider answers for the file as it is when asked, so no
      ;; earlier check has to vouch for it.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-error-new-at
                   1 1 'error "typo" :filename file :buffer nil
                   :fix (lambda (_err) (flycheck-test--edit 1 1 1 5 "hello")))))
        (expect (flycheck--apply-error-fix err 'visit) :to-be-truthy)
        (with-current-buffer (find-buffer-visiting file)
          (expect (buffer-string) :to-equal "hello world\n"))))

    (it "applies a fix whose file has not been written since the check"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file)))
        (flycheck-test--record-run err (time-add (current-time) 60))
        (expect (flycheck--apply-error-fix err 'visit) :to-be-truthy)
        (with-current-buffer (find-buffer-visiting file)
          (expect (buffer-string) :to-equal "hello world\n"))))

    (it "does not weigh a file on another host against the local clock"
      ;; Its timestamp comes from that host's clock; a machine running a
      ;; little ahead would otherwise refuse every fix.
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file)))
        (find-file-noselect file)
        (flycheck-test--record-run err (time-add (current-time) -60))
        (cl-letf (((symbol-function 'file-remote-p)
                   (lambda (name &rest _) (and (equal name file) "/ssh:h:"))))
          (expect (flycheck--apply-error-fix err 'visit) :to-be-truthy))
        (with-current-buffer (find-buffer-visiting file)
          (expect (buffer-string) :to-equal "hello world\n"))))

    (it "returns nothing when a lazy provider comes up empty"
      (let* ((file (flycheck-test--fix-file "b.el" "helo world\n"))
             (err (flycheck-test--fix-error file (lambda (_err) nil))))
        (expect (flycheck--apply-error-fix err 'visit) :to-be nil))))

  (describe "flycheck--project-error-time"

    (it "returns the time of the run that reported the error"
      (let ((err (flycheck-error-new-at 1 1 'error "x" :filename "/p/a.el"
                                        :buffer nil))
            (time (current-time)))
        (puthash (cons "/p/" 'test-run)
                 (list :process nil :errors (list err) :time time)
                 flycheck--project-runs)
        (expect (flycheck--project-error-time err) :to-equal time)))

    (it "returns nothing for an error no run reported"
      (expect (flycheck--project-error-time
               (flycheck-error-new-at 1 1 'error "x" :filename "/p/a.el"
                                      :buffer nil))
              :to-be nil))))

(provide 'test-quick-fixes)

;;; test-quick-fixes.el ends here
