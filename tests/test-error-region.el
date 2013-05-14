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

(defmacro flycheck-testsuite-buffer-with-error-at (text line column &rest body)
  "Execute BODY a temporary buffer with TEXT and an error at LINE:COLUMN.

In BODY the error is bound to ERR."
  (declare (indent 3))
  `(with-temp-buffer
     (insert ,text)
     (let ((err (flycheck-error-new :buffer (current-buffer)
                                    :line ,line :column ,column)))
       ,@body)))

(ert-deftest flycheck-error-region-no-column ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World" 2 nil
    (let ((region '(11 . 16)))
      (should-not (flycheck-error-column-region err))
      (should (equal (flycheck-error-line-region err) region))
      (should (equal (flycheck-error-region err) region)))))

(ert-deftest flycheck-error-region-no-column-eof ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World\n" 3 nil
    (let ((region '(16 . 17)))
      (should-not (flycheck-error-column-region err))
      (--each '(flycheck-error-line-region flycheck-error-region)
        (should (equal (funcall it err) region))))))

(ert-deftest flycheck-error-region-column ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World" 2 7
    (let ((region '(13 . 14)))
      (should (equal (flycheck-error-line-region err) '(11 . 16)))
      (--each '(flycheck-error-column-region flycheck-error-region)
        (should (equal (funcall it err) region))))))

(ert-deftest flycheck-error-region-column-eof ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World\n" 3 1
    (let ((region '(16 . 17)))
      (--each '(flycheck-error-column-region
                flycheck-error-line-region
                flycheck-error-region)
        (should (equal (funcall it err) region))))))

(ert-deftest flycheck-error-region-line-beyond-eof ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World\n" 4 1
    (let ((region '(16 . 17)))
      (--each '(flycheck-error-column-region
                flycheck-error-line-region
                flycheck-error-region)
        (should (equal (funcall it err) region))))))

(ert-deftest flycheck-error-region-column-beyond-eol ()
  (flycheck-testsuite-buffer-with-error-at "Hello\n    World\n" 1 10
    (let ((region '(6 . 7)))
      (should (equal (flycheck-error-line-region err) '(1 . 6)))
      (--each '(flycheck-error-column-region flycheck-error-region)
        (should (equal (funcall it err) region))))))

;; Local Variables:
;; coding: utf-8
;; End:

;;; test-error-region.el ends here
