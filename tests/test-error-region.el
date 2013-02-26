;;; test-error-region.el --- Tests for error region -*- lexical-binding: t; -*-

;; Copyright (c) 2013 Sebastian Wiesner <lunaryorn@gmail.com>
;;
;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; URL: https://github.com/lunaryorn/flycheck

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

;; Test calculations of error regions.

;;; Code:

(require 'ert)
(require 's)
(require 'flycheck)

(defun flycheck-should-region (text line-no col-no beg end &optional region-text)
  "Test the region of an error."
  (with-temp-buffer
    (insert text)
    (let* ((err (flycheck-error-new :buffer (current-buffer)
                                    :line line-no
                                    :column col-no))
           (region (flycheck-error-region err))
           (pos (flycheck-error-pos err)))
      (should (= pos (car region)))
      (should (= (car region) beg))
      (should (= (cdr region) end))
      (if region-text
          (should (string= (buffer-substring beg end) region-text))
        (should (s-blank? (s-trim (buffer-substring beg end))))))))

(ert-deftest flycheck-error-region-no-column ()
  "Test an error region without columns"
  (flycheck-should-region "Hello\n    World" 2 nil 11 16 "World"))

(ert-deftest flycheck-error-region-no-column-eof ()
  "Test an error region without column at the end of the file."
  (flycheck-should-region "Hello\n    World\n" 3 nil 16 17))

(ert-deftest flycheck-error-region-column ()
  "Test an error region with a column."
  (flycheck-should-region "Hello\n    World" 2 7 13 14 "r"))

(ert-deftest flycheck-error-region-column-eof ()
  "Test an error column at EOF."
  (flycheck-should-region "Hello\n    World\n" 3 1 16 17))

(ert-deftest flycheck-error-region-line-beyond-eof ()
  "Test an error line beyond the end of a file."
  (flycheck-should-region "Hello\n    World\n" 4 1 16 17))

(ert-deftest flycheck-error-region-column-beyond-eol ()
  "Test an error column beyond the end of a line."
  (flycheck-should-region "Hello\n    World\n" 1 10 6 7))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-error-region.el ends here
