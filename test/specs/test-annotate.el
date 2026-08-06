;;; test-annotate.el --- Flycheck Specs: Inline display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Flycheck contributors

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

;; Specs for the inline display of errors (`flycheck-annotate-mode').

;;; Code:

(require 'flycheck-buttercup)

(defun test-annotate/setup ()
  "Fill the current buffer with four lines and errors on lines 2 and 4.

Line 2 gets an error and a warning; line 4 gets a warning."
  (insert "line one here\nline two here\nline three ok\nline four here\n")
  (setq-local flycheck-mode t)
  (let ((e2 (flycheck-error-new-at 2 6 'error "bad thing" :id "E1"
                                   :buffer (current-buffer) :checker 'emacs-lisp))
        (e2b (flycheck-error-new-at 2 1 'warning "also warn"
                                    :buffer (current-buffer) :checker 'emacs-lisp))
        (e4 (flycheck-error-new-at 4 3 'warning "watch out"
                                   :buffer (current-buffer) :checker 'emacs-lisp)))
    (setq flycheck-current-errors (list e2 e2b e4))
    (mapc #'flycheck-add-overlay flycheck-current-errors)))

(defun test-annotate/text (ov)
  "Return OV's annotation string, minus the leading cursor-anchor space.
Return nil for overlays that carry no annotation, such as tint overlays,
so it doubles as a predicate for the message overlays."
  (when-let* ((s (overlay-get ov 'before-string)))
    (if (get-text-property 0 'cursor s) (substring s 1) s)))

(defun test-annotate/sideline (code msg-width win-width &optional gutter)
  "Render a MSG-WIDTH error beside CODE in a fringeless WIN-WIDTH window.

Stubs the window geometry: no fringes (so one column is reserved for
the continuation glyph) and a line-number gutter of GUTTER columns
(default none).  Return the rendered annotation string, cursor-anchor
space stripped."
  (insert code "\n")
  (let* ((errs (list (flycheck-error-new-at 1 1 'error (make-string msg-width ?m)
                                            :checker 'emacs-lisp)))
         (flycheck-annotate--overlays nil))
    (cl-letf (((symbol-function 'window-text-width) (lambda (&rest _) win-width))
              ((symbol-function 'window-fringes) (lambda (&rest _) '(0 0 nil nil)))
              ((symbol-function 'line-number-display-width)
               (lambda (&rest _) (or gutter 0))))
      (test-annotate/text
       (flycheck-annotate-sideline-style
        errs (save-excursion (goto-char (point-min)) (line-end-position)) nil)))))

(defun test-annotate/anchors ()
  "Return an alist mapping each message overlay's code line to its text.
A `below'-style block is hung off the start of the line after the code it
annotates (so it doesn't disturb visual-line motion); such a block ends in
a newline, so key it back under the code line."
  (mapcar (lambda (ov)
            (let* ((text (test-annotate/text ov))
                   (below (string-suffix-p "\n" text)))
              (cons (- (line-number-at-pos (overlay-start ov)) (if below 1 0))
                    (substring-no-properties text))))
          (seq-filter #'test-annotate/text flycheck-annotate--overlays)))

(defun test-annotate/tints ()
  "Return an alist mapping each tinted line to its background face."
  (mapcar (lambda (ov)
            (cons (line-number-at-pos (overlay-start ov))
                  (overlay-get ov 'face)))
          (seq-filter (lambda (ov) (overlay-get ov 'face))
                      flycheck-annotate--overlays)))

(describe "Inline display"

  (describe "the default format function"
    (it "renders the message and the id"
      (expect (substring-no-properties
               (funcall flycheck-annotate-format-function
                        (flycheck-error-new-at 1 1 'error "boom" :id "X9")))
              :to-match "boom")
      (expect (substring-no-properties
               (funcall flycheck-annotate-format-function
                        (flycheck-error-new-at 1 1 'error "boom" :id "X9")))
              :to-match "X9")))

  (describe "flycheck-annotate--level-face"
    (it "maps the built-in levels to the inline faces"
      (expect (flycheck-annotate--level-face 'error) :to-be 'flycheck-annotate-error)
      (expect (flycheck-annotate--level-face 'warning) :to-be 'flycheck-annotate-warning)
      (expect (flycheck-annotate--level-face 'info) :to-be 'flycheck-annotate-info)))

  (describe "flycheck-annotate--filter-levels"
    (it "keeps every error when levels is t"
      (let ((errs (list (flycheck-error-new-at 1 1 'error)
                        (flycheck-error-new-at 1 1 'warning))))
        (expect (flycheck-annotate--filter-levels errs t) :to-equal errs)))
    (it "keeps only errors of the listed levels"
      (let* ((err (flycheck-error-new-at 1 1 'error))
             (warn (flycheck-error-new-at 1 1 'warning)))
        (expect (flycheck-annotate--filter-levels (list err warn) '(error))
                :to-equal (list err)))))

  (describe "flycheck-annotate--effective-levels"
    (it "inherits flycheck-annotate-levels when the tier is t"
      (let ((flycheck-annotate-levels '(error warning)))
        (expect (flycheck-annotate--effective-levels t)
                :to-equal '(error warning))))
    (it "uses the tier's own levels when it is a list"
      (let ((flycheck-annotate-levels t))
        (expect (flycheck-annotate--effective-levels '(error))
                :to-equal '(error)))))

  (describe "flycheck-annotate-eol-style"
    (it "shows the most severe message and a count of the rest"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((errs (list (flycheck-error-new-at 1 1 'error "big"
                                                  :checker 'emacs-lisp)
                           (flycheck-error-new-at 1 2 'warning "small"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-eol-style errs (line-end-position) nil))
               (s (substring-no-properties (test-annotate/text ov))))
          (expect s :to-match "big")
          (expect s :to-match (regexp-quote "(+1)"))
          (expect (string-prefix-p "\n" s) :to-be nil)))))

  (describe "flycheck-annotate--one-line"
    (it "folds a message that spans lines"
      (expect (flycheck-annotate--one-line "unexpected newline\nexpecting number")
              :to-equal "unexpected newline expecting number"))

    (it "folds the indentation a wrapped message carries"
      (expect (flycheck-annotate--one-line "expected:\n    a number\n    a date")
              :to-equal "expected: a number a date"))

    (it "folds a run of blank lines to one space"
      (expect (flycheck-annotate--one-line "a\n\n\nb") :to-equal "a b"))

    (it "trims the edges"
      (expect (flycheck-annotate--one-line "\nboom\n") :to-equal "boom"))

    (it "leaves a single-line message alone"
      (expect (flycheck-annotate--one-line "boom") :to-equal "boom")))

  (describe "the compact styles with a multi-line message"
    ;; They hang the message off the end of the code, so a newline that
    ;; reaches the screen gives the line extra rows: `eol' stops being
    ;; after the line and `sideline' loses its alignment.
    (dolist (style '(eol sideline))
      (it (format "keeps the %s annotation on one line" style)
        (flycheck-buttercup-with-temp-buffer
          (insert "abcdef\n")
          (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
                 (errs (list (flycheck-error-new-at
                              1 3 'error "unexpected newline\nexpecting number"
                              :checker 'emacs-lisp)))
                 (flycheck-annotate--overlays nil)
                 (render (cdr (assq style flycheck-annotate-style-functions)))
                 (ov (funcall render errs eol t))
                 (text (substring-no-properties (test-annotate/text ov))))
            (expect text :not :to-match "\n")
            (expect text :to-match "unexpected newline expecting number"))))))

  (describe "flycheck-annotate-below-style"
    (defun test-annotate/faces-at (ov pos)
      "Return the face property at POS of OV's annotation string, as a list."
      (let ((face (get-text-property pos 'face (test-annotate/text ov))))
        (if (listp face) face (list face))))

    (it "carries the line tint through the whole block"
      ;; The block hangs off the next line, outside the range the tint
      ;; overlay covers, so it has to carry the tint itself or the code
      ;; line and its messages look like separate regions.  See #2276.
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\nghijkl\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at 1 3 'error "boom"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate-background t)
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (text (test-annotate/text ov)))
          ;; every character, the trailing newline included, or the tint
          ;; would stop short of the window edge
          (dotimes (i (length text))
            (expect (test-annotate/faces-at ov i)
                    :to-contain 'flycheck-annotate-error-background))
          ;; and the message keeps its own colour on top
          (expect (test-annotate/faces-at ov (string-match "boom" text))
                  :to-contain 'flycheck-annotate-error))))

    (it "leaves the block untinted when the tint is off"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\nghijkl\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at 1 3 'error "boom"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate-background nil)
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (text (test-annotate/text ov)))
          (dotimes (i (length text))
            (expect (test-annotate/faces-at ov i)
                    :not :to-contain 'flycheck-annotate-error-background)))))

    (it "stacks each message on its own line under the code"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at 1 3 'error "one"
                                                  :checker 'emacs-lisp)
                           (flycheck-error-new-at 1 1 'warning "two"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (s (substring-no-properties (test-annotate/text ov))))
          ;; the block hangs off the next line, so it ends (not begins) with a
          ;; newline, keeping visual-line motion off the annotated line
          (expect (string-suffix-p "\n" s) :to-be t)
          (expect (string-prefix-p "\n" s) :to-be nil)
          (expect s :to-match "one")
          (expect s :to-match "two")
          ;; one line per error, then the trailing newline
          (expect (length (split-string s "\n")) :to-equal 3))))

    (it "aligns the connector to the error's display column past a tab"
      (flycheck-buttercup-with-temp-buffer
        (setq-local tab-width 8)
        (insert "\tx = 1\n")               ; one tab, then code
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               ;; column 2 is the "x" right after the tab
               (errs (list (flycheck-error-new-at 1 2 'error "m"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (s (test-annotate/text ov)))
          ;; the leading pad aligns to display column 8, not 1
          (expect (get-text-property 0 'display s)
                  :to-equal '(space :align-to 8)))))

    (it "adds no pad for an error in the first column"
      (flycheck-buttercup-with-temp-buffer
        (insert "abc\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at 1 1 'error "m"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (s (test-annotate/text ov)))
          ;; char 0 is the connector itself, no stretch space
          (expect (get-text-property 0 'display s) :to-be nil))))

    (it "trails an error's related locations on their own lines"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at
                            1 1 'error "redefined" :checker 'emacs-lisp
                            :relations
                            (list (flycheck-related-location-new
                                   :line 5 :column 2 :message "first here")))))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (s (substring-no-properties (test-annotate/text ov))))
          (expect s :to-match "redefined")
          (expect s :to-match "↳ first here (5:2)")
          ;; the message line, the related-location line, then the trailing newline
          (expect (length (split-string s "\n")) :to-equal 3))))

    (it "hangs the block off the start of the next line"
      ;; Regression guard: anchoring the multi-line block on the following
      ;; line's buffer position (rather than the annotated line's newline)
      ;; keeps `next-line' and `evil-next-visual-line' from stalling on -- or,
      ;; under Evil, getting stuck before -- the annotation.
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\nghijkl\n")
        (let* ((eol (save-excursion (goto-char (point-min)) (line-end-position)))
               (errs (list (flycheck-error-new-at 1 1 'error "boom"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t)))
          (expect (line-number-at-pos (overlay-start ov)) :to-equal 2)
          (expect (overlay-start ov)
                  :to-equal (save-excursion (goto-char (point-min))
                                            (line-beginning-position 2))))))

    (it "falls back to a leading newline on the buffer's last line"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef")             ; no trailing newline: line 1 is the last
        (let* ((eol (line-end-position))
               (errs (list (flycheck-error-new-at 1 1 'error "boom"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-below-style errs eol t))
               (raw (overlay-get ov 'before-string)))
          ;; no following line to hang off: keep it on this line, with a
          ;; leading newline and the cursor-anchoring space
          (expect (overlay-start ov) :to-equal (point-max))
          (expect (get-text-property 0 'cursor raw) :to-be t)
          (expect (string-prefix-p " \n" (substring-no-properties raw))
                  :to-be t)))))

  (describe "flycheck-annotate-sideline-style"
    (it "right-aligns the compact message with an align-to spacer"
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((errs (list (flycheck-error-new-at 1 1 'error "big"
                                                  :checker 'emacs-lisp)
                           (flycheck-error-new-at 1 2 'warning "small"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (ov (flycheck-annotate-sideline-style errs (line-end-position) nil))
               (s (test-annotate/text ov)))
          ;; most severe message plus a count, no leading newline
          (expect (substring-no-properties s) :to-match "big")
          (expect (substring-no-properties s) :to-match (regexp-quote "(+1)"))
          (expect (string-prefix-p "\n" s) :to-be nil)
          ;; the leading char is a right-aligning stretch of whitespace
          (expect (car (get-text-property 0 'display s)) :to-be 'space)
          (expect (get-text-property 0 'display s)
                  :to-equal
                  ;; the width of "big (+1)", plus whatever the right edge
                  ;; reserves where this spec is running
                  `(space :align-to
                          (- right ,(+ 8 (flycheck-annotate--reserved-columns))))))))

    (it "keeps off the column the continuation glyph needs"
      ;; Aligned flush to `right' with no fringe to draw the glyph in, the
      ;; last character lands in the column it needs and wraps.  See #2292.
      (flycheck-buttercup-with-temp-buffer
        (insert "abcdef\n")
        (let* ((errs (list (flycheck-error-new-at 1 1 'error "hi"
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil))
          (cl-letf (((symbol-function 'window-fringes) (lambda (&rest _) '(8 8 nil nil))))
            (expect (flycheck-annotate--reserved-columns) :to-equal 0)
            (expect (get-text-property
                     0 'display
                     (test-annotate/text
                      (flycheck-annotate-sideline-style
                       errs (line-end-position) nil)))
                    :to-equal '(space :align-to (- right 2))))
          (setq flycheck-annotate--overlays nil)
          (cl-letf (((symbol-function 'window-fringes) (lambda (&rest _) '(0 0 nil nil))))
            (expect (flycheck-annotate--reserved-columns) :to-equal 1)
            (expect (get-text-property
                     0 'display
                     (test-annotate/text
                      (flycheck-annotate-sideline-style
                       errs (line-end-position) nil)))
                    :to-equal '(space :align-to (- right 3)))))))

    (it "truncates a message too wide for the room beside the code"
      ;; A message wider than the gap between code and window edge would
      ;; land after the code and wrap, taking back the single line the
      ;; style promises.  See #2312.
      (flycheck-buttercup-with-temp-buffer
        ;; room = 40 wide - 1 reserved - 6 code - 2 slack
        (let* ((s (test-annotate/sideline "abcdef" 60 40))
               (text (substring s 1)))
          (expect (string-width text) :to-equal 31)
          (expect (substring-no-properties text) :to-match "\\`mmm")
          (expect (get-text-property 0 'display s)
                  :to-equal '(space :align-to (- right 32)))
          ;; the ellipsis keeps the message's face
          (expect (get-text-property (1- (length text)) 'face text)
                  :to-equal (get-text-property 0 'face text)))))

    (it "counts the line-number gutter against the room"
      ;; The gutter is drawn inside the text area, so `window-text-width'
      ;; includes columns the code cannot use.
      (flycheck-buttercup-with-temp-buffer
        ;; room = 40 wide - 4 gutter - 1 reserved - 6 code - 2 slack
        (let ((s (test-annotate/sideline "abcdef" 60 40 4.0)))
          (expect (string-width (substring s 1)) :to-equal 27)
          (expect (get-text-property 0 'display s)
                  :to-equal '(space :align-to (- right 28))))))

    (it "keeps a message that exactly fills the room whole"
      (flycheck-buttercup-with-temp-buffer
        (let ((s (test-annotate/sideline "abcdef" 31 40)))
          (expect (substring-no-properties (substring s 1))
                  :to-equal (make-string 31 ?m))
          (expect (get-text-property 0 'display s)
                  :to-equal '(space :align-to (- right 32))))))

    (it "leaves a message whole when the code leaves almost no room"
      ;; Truncated to a couple of columns the message would be mostly
      ;; ellipsis; trailing the code complete is the lesser evil.
      (flycheck-buttercup-with-temp-buffer
        ;; room = 20 - 1 - 15 - 2 = 2, under the useful minimum
        (let ((s (test-annotate/sideline "abcdefghijklmno" 60 20)))
          (expect (substring-no-properties (substring s 1))
                  :to-equal (make-string 60 ?m)))))

    (it "is registered as a built-in style"
      (expect (cdr (assq 'sideline flycheck-annotate-style-functions))
              :to-be 'flycheck-annotate-sideline-style))

    (it "renders on the current line when selected as the style"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 1)              ; line 2
          (let ((flycheck-annotate-current-line-style 'sideline)
                (flycheck-annotate-other-lines-style nil))
            (flycheck-annotate-mode 1)
            (let* ((ov (seq-find (lambda (o) (test-annotate/text o))
                                 flycheck-annotate--overlays))
                   (s (test-annotate/text ov)))
              (expect (car (get-text-property 0 'display s)) :to-be 'space)))))))

  (describe "the fix marker"
    (let ((mkfix (lambda ()
                   (flycheck-fix-new
                    :edits (list (flycheck-fix-edit-new
                                  :line 1 :column 1 :end-line 1 :end-column 2
                                  :replacement "x"))))))
      (it "prefixes a fixable error's compact text"
        (let ((flycheck-annotate-fix-marker "[fix] ")
              (err (flycheck-error-new-at 1 1 'error "boom"
                                          :fix (funcall mkfix))))
          (expect (substring-no-properties
                   (flycheck-annotate--compact-text (list err)))
                  :to-match (regexp-quote "[fix] "))))
      (it "omits the marker for a non-fixable error"
        (let ((flycheck-annotate-fix-marker "[fix] ")
              (err (flycheck-error-new-at 1 1 'error "boom")))
          (expect (substring-no-properties
                   (flycheck-annotate--compact-text (list err)))
                  :not :to-match (regexp-quote "[fix]"))))
      (it "omits the marker when disabled"
        (let ((flycheck-annotate-fix-marker nil)
              (err (flycheck-error-new-at 1 1 'error "boom"
                                          :fix (funcall mkfix))))
          (expect (flycheck-annotate--fix-marker err) :to-equal "")))))

  (describe "flycheck-annotate--make-overlay"
    (it "tags and tracks the overlay"
      (flycheck-buttercup-with-temp-buffer
        (insert "abc\n")
        (let* ((flycheck-annotate--overlays nil)
               (ov (flycheck-annotate--make-overlay (point-min) "x")))
          (expect (overlay-get ov 'flycheck-annotate) :to-be t)
          (expect (memq ov flycheck-annotate--overlays) :not :to-be nil)))))

  (describe "flycheck-annotate--suppresses-echo-p"
    (it "suppresses the echo message for an error rendered inline"
      (flycheck-buttercup-with-temp-buffer
        (test-annotate/setup)
        (goto-char (point-min))
        (forward-line 3)                ; line 4 warning at column 3
        (forward-char 2)                ; onto the warning
        (let ((flycheck-annotate-mode t)
              (flycheck-annotate-suppress-echo t)
              (flycheck-annotate-current-line-style 'below)
              (flycheck-annotate-levels t))
          (expect (flycheck-annotate--suppresses-echo-p) :to-be t))))
    (it "is nil when the current line is not annotated"
      (let ((flycheck-annotate-mode t)
            (flycheck-annotate-suppress-echo t)
            (flycheck-annotate-current-line-style nil))
        (expect (flycheck-annotate--suppresses-echo-p) :to-be nil)))
    (it "is nil when suppression is disabled"
      (let ((flycheck-annotate-mode t)
            (flycheck-annotate-suppress-echo nil)
            (flycheck-annotate-current-line-style 'below))
        (expect (flycheck-annotate--suppresses-echo-p) :to-be nil)))
    (it "does not suppress an error the inline display would filter out"
      (flycheck-buttercup-with-temp-buffer
        (test-annotate/setup)
        (goto-char (point-min))
        (forward-line 3)                ; line 4 has a warning at column 3
        (forward-char 2)                ; onto the warning
        (let ((flycheck-annotate-mode t)
              (flycheck-annotate-suppress-echo t)
              (flycheck-annotate-current-line-style 'below)
              (flycheck-annotate-levels '(error)))
          ;; the only error at point is a warning, excluded by the level
          ;; filter, so it must still reach the echo area
          (expect (flycheck-annotate--suppresses-echo-p) :to-be nil))))

    (it "does not suppress an error whose sideline message was truncated"
      ;; A truncated rendering is an incomplete one, so the full text
      ;; must still reach the echo area.
      (flycheck-buttercup-with-temp-buffer
        (test-annotate/setup)
        (goto-char (point-min))
        (forward-line 3)                ; line 4 warning at column 3
        (forward-char 2)                ; onto the warning
        (let* ((msg (make-string 60 ?m))
               (errs (list (flycheck-error-new-at 4 3 'warning msg
                                                  :checker 'emacs-lisp)))
               (flycheck-annotate--overlays nil)
               (flycheck-annotate-mode t)
               (flycheck-annotate-suppress-echo t)
               (flycheck-annotate-current-line-style 'sideline)
               (flycheck-annotate-levels t))
          (cl-letf (((symbol-function 'window-text-width) (lambda (&rest _) 30))
                    ((symbol-function 'window-fringes) (lambda (&rest _) '(0 0 nil nil))))
            ;; room = 30 - 1 reserved - 14 code - 2 slack = 13, message 60
            (flycheck-annotate-sideline-style errs (line-end-position) t)
            (expect (flycheck-annotate--truncated-at-point-p) :to-be t)
            (expect (flycheck-annotate--suppresses-echo-p) :to-be nil))))))

  (describe "the two-tier layout"
    (it "uses the below style on the line at point and eol elsewhere"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 1)              ; line 2
          (flycheck-annotate-mode 1)
          (let ((anchors (test-annotate/anchors)))
            ;; line 2 is focused -> below (a multi-line block ending in a newline)
            (expect (string-suffix-p "\n" (cdr (assq 2 anchors))) :to-be t)
            ;; line 4 is unfocused -> eol (a single line)
            (expect (string-suffix-p "\n" (cdr (assq 4 anchors))) :to-be nil)))))

    (it "swaps the styles as point moves between lines"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 3)              ; line 4
          (flycheck-annotate-mode 1)
          (let ((anchors (test-annotate/anchors)))
            (expect (string-suffix-p "\n" (cdr (assq 4 anchors))) :to-be t)
            (expect (string-suffix-p "\n" (cdr (assq 2 anchors))) :to-be nil))))))

  (describe "per-tier level filtering"
    (it "can restrict other lines to a stricter level set than point"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "aa\nbb\ncc\ndd\n")
          (setq-local flycheck-mode t)
          (let ((w2 (flycheck-error-new-at 2 1 'warning "w2"
                                           :buffer (current-buffer)
                                           :checker 'emacs-lisp))
                (w4 (flycheck-error-new-at 4 1 'warning "w4"
                                           :buffer (current-buffer)
                                           :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list w2 w4))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-min))
          (forward-line 1)              ; point on line 2
          (let ((flycheck-annotate-current-line-style 'eol)
                (flycheck-annotate-other-lines-style 'eol)
                (flycheck-annotate-other-lines-levels '(error)))
            (flycheck-annotate-mode 1)
            (let ((anchors (test-annotate/anchors)))
              ;; the current line's warning shows; the other line's warning,
              ;; excluded by the stricter other-lines filter, does not
              (expect (assq 2 anchors) :not :to-be nil)
              (expect (assq 4 anchors) :to-be nil)))))))

  (describe "flycheck-annotate-mode"
    (it "clears the overlays when disabled"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (flycheck-annotate-mode 1)
          (expect flycheck-annotate--overlays :not :to-be nil)
          (flycheck-annotate-mode -1)
          (expect flycheck-annotate--overlays :to-be nil))))

    (it "drops the overlays on a manual clear"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (flycheck-annotate-mode 1)
          (expect flycheck-annotate--overlays :not :to-be nil)
          (flycheck-clear)
          (expect flycheck-annotate--overlays :to-be nil))))

    (it "anchors the cursor to the end of the code for the single-line styles"
      ;; The single-line `eol' and `sideline' overlays span the trailing newline
      ;; and render their text as a `before-string' whose first character is a
      ;; plain space carrying a `cursor' property.  That keeps C-e and typing
      ;; parked at the end of the code instead of the end of the annotation.  A
      ;; plain space is essential: the `cursor' property lands at the far end of
      ;; the `sideline' `:align-to' stretch.  The multi-line `below' style keeps
      ;; the cursor on the code line by hanging its block off the next line
      ;; instead (see its own specs), so it is excluded here.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (dolist (style '(eol sideline))
            (let ((flycheck-annotate-current-line-style style)
                  (flycheck-annotate-other-lines-style style))
              (flycheck-annotate-mode 1)
              (let ((msg-ovs (seq-filter #'test-annotate/text
                                         flycheck-annotate--overlays)))
                (expect msg-ovs :not :to-be nil)
                (dolist (ov msg-ovs)
                  (let ((s (overlay-get ov 'before-string)))
                    ;; spans exactly the trailing newline
                    (expect (overlay-end ov) :to-equal (1+ (overlay-start ov)))
                    ;; plain-space cursor anchor: cursor prop, a real space,
                    ;; and no `display' spec that would move the cursor away
                    (expect (get-text-property 0 'cursor s) :to-be t)
                    (expect (aref s 0) :to-equal ?\s)
                    (expect (get-text-property 0 'display s) :to-be nil))))
              (flycheck-annotate-mode -1))))))

    (it "keeps a last-line annotation with no trailing newline"
      ;; At end of buffer there is no newline to span, so the overlay is empty;
      ;; it must not evaporate, or the annotation would disappear.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "code")               ; deliberately no trailing newline
          (setq-local flycheck-mode t)
          (let ((e (flycheck-error-new-at 1 1 'error "boom"
                                          :buffer (current-buffer)
                                          :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list e))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-max))
          (let ((flycheck-annotate-current-line-style 'below)
                (flycheck-annotate-other-lines-style 'below))
            (flycheck-annotate-mode 1)
            (let ((ov (seq-find #'test-annotate/text flycheck-annotate--overlays)))
              (expect ov :not :to-be nil)
              (expect (overlay-start ov) :to-equal (point-max))
              (expect (overlay-get ov 'evaporate) :to-be nil))))))

    (it "leaves the code editable at the end of the line"
      ;; Typing at end of line inserts into the code, not into the annotation.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "code\n")
          (setq-local flycheck-mode t)
          (let ((e (flycheck-error-new-at 1 1 'error "boom"
                                          :buffer (current-buffer)
                                          :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list e))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-min))
          (let ((flycheck-annotate-current-line-style 'below)
                (flycheck-annotate-other-lines-style 'below))
            (flycheck-annotate-mode 1)
            (end-of-line)
            (insert "X")
            (expect (buffer-substring-no-properties
                     (point-min) (line-end-position))
                    :to-equal "codeX")))))

    (it "rebuilds when the window scrolls"
      ;; `post-command-hook' runs before redisplay, so a jump that sends
      ;; point off screen rebuilds while `window-start' still describes
      ;; where the window was, and the line jumped to is not yet part of
      ;; what looks visible.  Scrolling has to rebuild too.  See #2293.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (dotimes (i 400) (ignore i) (insert "a line of code\n"))
          (setq-local flycheck-mode t)
          (let ((e (flycheck-error-new-at 380 1 'error "far away"
                                          :buffer (current-buffer)
                                          :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list e))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-min))
          (let ((flycheck-annotate-current-line-style 'eol)
                (flycheck-annotate-other-lines-style 'eol))
            (flycheck-annotate-mode 1)
            ;; Nothing is annotated while the error is far off screen
            (expect (seq-filter #'test-annotate/text flycheck-annotate--overlays)
                    :to-be nil)
            ;; Jump there without redisplay, the way a command does
            (goto-char (point-min))
            (forward-line 379)
            (flycheck-annotate--post-command)
            ;; The window has not scrolled yet, so this found nothing
            (expect (seq-filter #'test-annotate/text flycheck-annotate--overlays)
                    :to-be nil)
            ;; Redisplay scrolls and reports it, which is what rebuilds
            (set-window-start (selected-window) (line-beginning-position))
            (flycheck-annotate--after-scroll (selected-window) (window-start))
            (expect (seq-filter #'test-annotate/text flycheck-annotate--overlays)
                    :to-be-truthy)
            (flycheck-annotate-mode -1)))))

    (it "does not rebuild from inside its own rebuild"
      ;; Laying out an annotation can scroll the window, and the scroll hook
      ;; runs during redisplay, so it has to refuse to come back round
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "abcdef\n")
          (setq-local flycheck-mode t)
          (let ((calls 0))
            (cl-letf (((symbol-function 'flycheck-annotate--post-command)
                       (lambda () (cl-incf calls)
                         (flycheck-annotate--after-scroll
                          (selected-window) (window-start)))))
              (flycheck-annotate--after-scroll (selected-window) (window-start))
              (expect calls :to-equal 1))))))

    (it "rebuilds after an edit that leaves point on its line"
      ;; An edit such as `open-line' changes the buffer without moving point
      ;; off its line or scrolling the window, so the line/window check alone
      ;; would not rebuild.  The buffer-change (tick) check must catch it.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "abcdef\n")
          (setq-local flycheck-mode t)
          (let ((e (flycheck-error-new-at 1 1 'error "boom"
                                          :buffer (current-buffer)
                                          :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list e))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-min))
          (let ((flycheck-annotate-current-line-style 'eol)
                (flycheck-annotate-other-lines-style 'eol))
            (flycheck-annotate-mode 1)
            (let ((line0 flycheck-annotate--last-line-start)
                  (win0 flycheck-annotate--last-window-start))
              ;; edit later on the same line, leaving point put
              (save-excursion (goto-char (+ (point-min) 3)) (insert "X"))
              ;; the line/window check would skip: neither has changed
              (expect (line-beginning-position) :to-equal line0)
              (expect (window-start) :to-equal win0)
              ;; but the buffer changed, so post-command must rebuild
              (flycheck-annotate--post-command)
              (expect flycheck-annotate--last-tick
                      :to-equal (buffer-chars-modified-tick))))))))

    (it "does not rebuild crossing between two clean lines"
      ;; Which line has point only matters through the current-line tier,
      ;; and a line without errors renders nothing under any tier.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)          ; errors on lines 2 and 4
          (goto-char (point-min))        ; line 1, clean
          (flycheck-annotate-mode 1)
          (spy-on 'flycheck-annotate--refresh)
          (goto-char (point-max))        ; past line 4, clean line 5
          (flycheck-annotate--post-command)
          (expect 'flycheck-annotate--refresh :not :to-have-been-called))))

    (it "rebuilds crossing from a clean line onto an error line"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)          ; errors on lines 2 and 4
          (goto-char (point-min))        ; line 1, clean
          (flycheck-annotate-mode 1)
          (spy-on 'flycheck-annotate--refresh)
          (goto-char (point-min))
          (forward-line 1)               ; line 2 carries an error
          (flycheck-annotate--post-command)
          (expect 'flycheck-annotate--refresh :to-have-been-called))))

    (it "rebuilds leaving an error line for a clean one"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)          ; errors on lines 2 and 4
          (goto-char (point-min))
          (forward-line 1)               ; line 2 carries an error
          (flycheck-annotate-mode 1)
          (spy-on 'flycheck-annotate--refresh)
          (goto-char (point-min))        ; back to clean line 1
          (flycheck-annotate--post-command)
          (expect 'flycheck-annotate--refresh :to-have-been-called))))

    (it "counts an error anchored at the line's very end as the line's"
      ;; A missing-semicolon style error is reported past the last
      ;; character, so its overlay starts exactly at eol; the line must
      ;; not pass for clean, or the annotation sticks in the wrong tier.
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (insert "ab\ncd\n")
          (setq-local flycheck-mode t)
          (let ((e (flycheck-error-new-at 1 3 'error "missing ;"
                                          :buffer (current-buffer)
                                          :checker 'emacs-lisp)))
            (setq flycheck-current-errors (list e))
            (mapc #'flycheck-add-overlay flycheck-current-errors))
          (goto-char (point-min))
          (forward-line 1)               ; line 2, clean
          (flycheck-annotate-mode 1)
          (spy-on 'flycheck-annotate--refresh)
          (goto-char (point-min))        ; onto the annotated line 1
          (flycheck-annotate--post-command)
          (expect 'flycheck-annotate--refresh :to-have-been-called))))

  (describe "the background tint"
    (it "adds no tint when disabled"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (let ((flycheck-annotate-background nil))
            (flycheck-annotate-mode 1)
            (expect (test-annotate/tints) :to-be nil)))))

    (it "tints each error line with its most severe level"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (let ((flycheck-annotate-background t))
            (flycheck-annotate-mode 1)
            (let ((tints (test-annotate/tints)))
              ;; line 2 has an error and a warning -> error wins
              (expect (cdr (assq 2 tints))
                      :to-be 'flycheck-annotate-error-background)
              (expect (cdr (assq 4 tints))
                      :to-be 'flycheck-annotate-warning-background))))))

    (it "tints error lines even when their message style is nil"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (goto-char (point-min))
          (forward-line 1)              ; point on line 2
          (let ((flycheck-annotate-background t)
                (flycheck-annotate-other-lines-style nil))
            (flycheck-annotate-mode 1)
            ;; line 4 gets no message, but is still tinted
            (expect (assq 4 (test-annotate/anchors)) :to-be nil)
            (expect (cdr (assq 4 (test-annotate/tints)))
                    :to-be 'flycheck-annotate-warning-background)))))

    (it "spans the whole line so the tint extends past the text"
      (flycheck-buttercup-with-temp-buffer
        (save-window-excursion
          (set-window-buffer (selected-window) (current-buffer))
          (test-annotate/setup)
          (let ((flycheck-annotate-background t))
            (flycheck-annotate-mode 1)
            (let ((ov (seq-find
                       (lambda (o)
                         (and (overlay-get o 'face)
                              (= 4 (line-number-at-pos (overlay-start o)))))
                       flycheck-annotate--overlays)))
              (goto-char (overlay-start ov))
              (expect (overlay-start ov) :to-equal (line-beginning-position))
              (expect (overlay-end ov) :to-equal (1+ (line-end-position))))))))))

(describe "global-flycheck-annotate-mode"
  (after-each
    ;; The globalized mode toggles the minor mode across all buffers and
    ;; installs a global hook, so make sure it never leaks between specs.
    (global-flycheck-annotate-mode -1))

  (it "enables flycheck-annotate-mode in buffers Flycheck may check"
    (let ((buffer (get-buffer-create "test-annotate-global-eligible")))
      (unwind-protect
          (with-current-buffer buffer
            (text-mode)
            (global-flycheck-annotate-mode 1)
            (expect flycheck-annotate-mode :to-be-truthy))
        (kill-buffer buffer))))

  (it "leaves buffers Flycheck skips alone"
    (let ((buffer (get-buffer-create "test-annotate-global-special")))
      (unwind-protect
          (with-current-buffer buffer
            (special-mode)
            (global-flycheck-annotate-mode 1)
            (expect flycheck-annotate-mode :to-be nil))
        (kill-buffer buffer)))))

;;; test-annotate.el ends here
