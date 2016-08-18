;;; flycheck-compile.el --- Flycheck byte compiler            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
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

;; This file provides non-interactive byte compilation for Flycheck.
;;
;; It's essentially a wrapper around `batch-byte-compile' which sets some
;; additional byte compiler options for Flycheck.

;;; Code:

(require 'warnings)
(require 'bytecomp)

(unless noninteractive
  (error "This file must not be used interactively"))

(defun flycheck/batch-byte-compile ()
  "Like `batch-byte-compile', but set additional flags.

Specifically set `byte-compile-error-on-warn' to t on Emacs 25."
  ;; Unfortunately `byte-compile-error-on-warn' does not quite what the name
  ;; suggest because for whatever mysterious reason there's also
  ;; `byte-compile-log-warning' used throughout Emacs' code which bypasses
  ;; `byte-compile-error-on-warn' and instead logs an Emacs warning with
  ;; `display-warning'.  These warnings don't trigger errors even if
  ;; `byte-compile-error-on-warn' is non-nil, which is… well, at least a very
  ;; _unusual_ design decision, which leads the whole purpose of
  ;; `byte-compile-error-on-warn' ad absurdum.
  ;;
  ;; To work around this mess (I'm sorry) we check the size of
  ;; `byte-compile-log-buffer' after each file to check if any warnings end up
  ;; there and exit with a non-zero code if the buffer is not empty.
  ;;
  ;; Unfortunately this means that we can't use `batch-byte-compile' (which is
  ;; the proper API) and instead have to call the undocumented internal function
  ;; `batch-byte-compile-file'.  Yay, so now proper byte compilation of Flycheck
  ;; depends on Emacs' internals, and much evil is accomplished.  Can't get any
  ;; worse, can it?
  (let ((byte-compile-error-on-warn (version<= "25" emacs-version)))
    (while command-line-args-left
      (let ((filename (pop command-line-args-left)))
        (unless (batch-byte-compile-file filename)
          ;; Exit if compilation failed
          (kill-emacs 1))
        (when (and byte-compile-error-on-warn
                   (get-buffer byte-compile-log-buffer)
                   (> (buffer-size (get-buffer byte-compile-log-buffer)) 0))
          ;; If there's anything in the log buffer (from the idiocy that is
          ;; `byte-compile-log-warning') exit as well to _ALL_ warnings, really
          ;; ALL WARNINGS. Got it, Emacs?  Why are making my life so hard?  At
          ;; least we don't have to print the contents explicitly because
          ;; `display-warnings' writes to standard whatever stream in batch
          ;; mode.
          (kill-emacs 1)))))
  (kill-emacs 0))

;;; flycheck-compile.el ends here
