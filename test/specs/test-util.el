;;; test-util.el --- Flycheck Specs: Utilities       -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016 Sebastian Wiesner and Flycheck contributors

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

;; Specs for utility functions.

;;; Code:

(require 'flycheck-buttercup)
(require 'epa-file)                     ; To test encrypted buffers

(defun flycheck/encrypt-string-to-file (string passphrase filename)
  "Encrypt STRING with PASSPHRASE and write to FILENAME.

This function is ABSOLUTELY INSECURE, use only and exclusively for testing."
  ;; Encrypt string via GPG, passing it on standard input.  Enforce a pipe for
  ;; communication with `process-connection-type nil', otherwise gpg tries to
  ;; read beyond EOF and never finishes.
  (let* ((process-connection-type nil)
         (gpg (start-process "flycheck-buttercup-gpg" nil
                             "gpg" "--batch" "--no-tty" "-c" "--passphrase"
                             passphrase "-o" filename "-")))
    (process-send-string gpg string)
    (process-send-eof gpg)
    ;; Wait until GPG exists
    (while (process-live-p gpg)
      (accept-process-output)
      (sleep-for 0.1))))

(defun flycheck/gpg-available-p ()
  "Whether GPG is available or not."
  ;; `epg-check-configuration' errors if the configuration is invalid, and
  ;; otherwise returns nil, hence ignore errors and default to `t' to get a
  ;; proper truthy result
  (ignore-errors (or (epg-check-configuration (epg-configuration)) t)))

(describe "Utilities"

  (describe "flycheck-encrypted-buffer-p"

    (it "considers a temporary buffer as unencrypted"
      (with-temp-buffer
        (expect (flycheck-encrypted-buffer-p) :not :to-be-truthy)))

    (it "considers a file buffer as unencrypted"
      (let ((file-name (make-temp-file "flycheck-file")))
        (unwind-protect
            (with-temp-buffer
              (insert-file-contents file-name 'visit)
              (set-visited-file-name file-name 'no-query)
              (expect (flycheck-encrypted-buffer-p) :not :to-be-truthy))
          (ignore-errors (delete-file file-name)))))

    (it "recognizes an encrypted buffer"
      (assume (flycheck/gpg-available-p) "gpg not installed")

      ;; Create a temporary file name.  Do NOT use `make-temp-file' here,
      ;; because that hangs with the extension `.gpg'.
      (let* ((file-name (expand-file-name
                         (concat (make-temp-name "flycheck-encrypted-file")
                                 ".txt.gpg")
                         temporary-file-directory))
             (passphrase "spam with eggs")
             ;; Teach EPA about the passphrase for our file to decrypt without
             ;; any user interaction.  `epa-file-passphrase-alist' stores
             ;; canonical file names, hence we pass the temporary file name
             ;; through `file-truename' to remove any symlinks in the path.
             (epa-file-cache-passphrase-for-symmetric-encryption t)
             (epa-file-passphrase-alist (list (cons (file-truename file-name)
                                                    passphrase))))
        (unwind-protect
            (with-temp-buffer
              (flycheck/encrypt-string-to-file "Hello world"
                                               passphrase file-name)
              (let ((inhibit-message t))
                ;; Silence "Decrypting ..." messages to keep buttercup output
                ;; clean
                (insert-file-contents file-name 'visit))
              (set-visited-file-name file-name 'no-query)

              (expect (flycheck-encrypted-buffer-p) :to-be-truthy))
          (ignore-errors (delete-file file-name)))))))

;;; test-util.el ends here
