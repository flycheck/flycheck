;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'flycheck-ert)

(defun flycheck-ert-should-syntax-check (resource-file modes &rest errors)
  "Test a syntax check in RESOURCE-FILE with MODES.

RESOURCE-FILE is the file to check.  MODES is a single major mode
symbol or a list thereof, specifying the major modes to syntax
check with.  If more than one major mode is specified, the test
is run for each mode separately, so if you give three major
modes, the entire test will run three times.  ERRORS is the list
of expected errors, as in `flycheck-ert-should-errors'.  If
omitted, the syntax check must not emit any errors.  The errors
are cleared after each test.

The syntax checker is selected via standard syntax checker
selection.  To test a specific checker, you need to set
`flycheck-checker' or `flycheck-disabled-checkers' accordingly
before using this predicate, depending on whether you want to use
manual or automatic checker selection.

During the syntax check, configuration files of syntax checkers
are also searched in the `config-files' sub-directory of the
resource directory."
  (when (symbolp modes)
    (setq modes (list modes)))
  (dolist (mode modes)
    (unless (fboundp mode)
      (ert-skip (format "%S missing" mode)))
    (flycheck-ert-with-resource-buffer resource-file
      (funcall mode)
      ;; Load safe file-local variables because some tests depend on them
      (let ((enable-local-variables :safe)
            ;; Disable all hooks at this place, to prevent 3rd party packages
            ;; from interfering
            (hack-local-variables-hook))
        (hack-local-variables))
      ;; Configure config file locating for unit tests
      (let ((process-hook-called 0))
        (add-hook 'flycheck-process-error-functions
                  (lambda (_err)
                    (setq process-hook-called (1+ process-hook-called))
                    nil)
                  nil :local)
        (add-hook 'flycheck-status-changed-functions
                  (lambda (status)
                    (when (eq status 'suspicious)
                      (signal 'flycheck-ert-suspicious-checker nil))))
        (flycheck-ert-buffer-sync)
        (apply #'flycheck-ert-regen-errors errors))
      (flycheck-ert-ensure-clear))))

(defun flycheck-ert-regen-errors (&rest expected-errors)
  "Replace errors in flycheck-test.el.

Find EXPECTED-ERRORS in flycheck-test.el and replace them by
flycheck-current-errors."
  (let ((test-filename (expand-file-name
                        "flycheck-test.el"
                        (locate-dominating-file default-directory "flycheck-test.el")))
        (expected-errors-re-string
         (mapconcat
          (lambda (err)
            (replace-regexp-in-string " +" "[\n ]+"
                                      (regexp-quote (format "'%S" err))
                                      'fixedcase 'literal))
          expected-errors "[\n ]+"))
        (replacement-errors
         (flycheck-ert-errors-to-string flycheck-current-errors)))
    ;;(message "%S" expected-errors-re-string)
    (with-current-buffer (find-file test-filename)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward expected-errors-re-string nil 'noerror)
          (let ((replace-start (match-beginning 0))
                (replace-end   (match-end 0)))
            (delete-region replace-start replace-end)
            (insert replacement-errors)
            ;;(message "Replaced %s to %s" replace-start replace-end)
            (save-buffer)))))))

(defun flycheck-ert-errors-to-string (errors)
  "Convert ERRORS objects to string."
  (let ((group-counter 0))
    (mapconcat
     (lambda (err)
       (format "'%S"
               ;; TODO: do not add properties that are nil
               `(,(flycheck-error-line err)
                 ,(flycheck-error-column err)
                 ,(flycheck-error-level err)
                 ,(flycheck-error-message err)
                 :checker ,(flycheck-error-checker err)
                 :id ,(flycheck-error-id err)
                 :group ,(when (flycheck-error-group err)
                           (unless (get (flycheck-error-group err) 'id)
                             (put (flycheck-error-group err) 'id (setq group-counter (1+ group-counter))))
                           (get (flycheck-error-group err) 'id))
                 :end-line ,(flycheck-error-end-line err)
                 :end-column ,(flycheck-error-end-column err)
                 )))
     errors "\n")))

(provide 'setup-regen)

;;; setup-regen ends here
