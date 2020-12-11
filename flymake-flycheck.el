;;; flymake-flycheck.el --- A Flymake backend for Flycheck checkers -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "26.1"))
;; Package-Version: 0.1
;; Author: Tom Fitzhenry <tomfitzhenry@google.com>

;;; Commentary:

;; Flymake is the built-in Emacs package to support on-the-fly syntax checking.
;; This library adds support for Flymake to use Flycheck's checkers.

;; To add all of Flycheck's checkers:
;;
;; (add-hook 'flymake-diagnostic-functions 'flymake-flycheck-diagnostic-function)'.

;; To use a single Flycheck checker (e.g. 'sh-shellcheck):
;;
;; (add-hook
;;   'flymake-diagnostic-functions
;;   (lambda () (flymake-flycheck-make-diagnostic-function 'sh-shellcheck)))

;; TODO: Add ert tests.

;;; Code:

(require 'flycheck)

;;;###autoload
(defun flymake-flycheck-make-diagnostic-function (checker)
  "A `flymake-diagnostic-functions' function to use Flycheck CHECKER."
  (lambda (report-fn &rest pairs)
    (flymake-flycheck--do checker report-fn pairs)))

;;;###autoload
(defun flymake-flycheck-diagnostic-function (report-fn &rest pairs)
  "A Flymake diagnostic function to use the checker that Flycheck would.

REPORT-FN and PAIRS are as described in `flymake-diagnostic-functions'."
  (funcall
   (flymake-flycheck-make-diagnostic-function
    (flycheck-get-checker-for-buffer))
   report-fn pairs))

(defun flymake-flycheck--do (checker report-fn &rest pairs)
  "Run CHECKER and propagate its callback to the Flymake callback REPORT-FN.

PAIRS is as described in `flymake-diagnostic-functions'."
  ;; TODO: Cancel the process on successive flymake calls, to avoid "Obsolete report from backend" errors.
  (when (flycheck-may-use-checker checker)
    (flymake-log :debug "Starting Flycheck %s checker." checker)
    (funcall report-fn (list)) ;; indicate running
    (flycheck-start-command-checker
     checker
     (lambda (status data)
       (pcase status
         (`suspicious
          (flymake-log :warning "Flycheck %s checker behaved suspiciously: %s" checker (or data "UNKNOWN!")))
         (`errored
          (flymake-log :error "Flycheck %s checker returned an error: %s" checker (or data "UNKNOWN!")))
         (`interrupted
          (flymake-log :warning "Flycheck %s checker was interrupted: %s" checker (or data "UNKNOWN!")))
         (`finished
          (flymake-log :debug "Flycheck %s checker finished successfully with %d errors." checker (length data))
          (funcall report-fn
                   (mapcar 'flymake-flycheck--error-to-diagnostic data))))))))

(defun flymake-flycheck--error-to-diagnostic (err)
  "Return a Flymake diagnostic for the Flycheck ERR."
  ;; TODO: Handle flycheck-error-foo returning nil.
  (pcase-let*
      ((buf (flycheck-error-buffer err))
       (`(,beg . ,end) (flymake-diag-region buf (flycheck-error-line err) (flycheck-error-column err))))
    ;; TODO: Convert Flycheck 'error into Flymake :error.
    (flymake-make-diagnostic buf beg end (flycheck-error-level err) (flycheck-error-message err))))

(provide 'flymake-flycheck)
;;; flymake-flycheck.el ends here
