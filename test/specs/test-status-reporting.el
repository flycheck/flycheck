;;; test-status-reporting.el --- Flycheck Specs: Status Reporting -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Status reporting"

  (describe "flycheck-report-status"

    (it "runs functions"
      (flycheck-buttercup-with-temp-buffer
        (let* ((was-called nil)
               (flycheck-status-changed-functions
                (list (lambda (status) (setq was-called status)))))
          (flycheck-report-status 'running)
          (expect was-called :to-be 'running)))))

  (describe "flycheck-report-failed-syntax-check"

    (it "runs hook"
      (flycheck-buttercup-with-temp-buffer
        (let* ((was-called nil)
               (flycheck-syntax-check-failed-hook
                (list (lambda () (setq was-called t)))))
          (flycheck-report-failed-syntax-check)
          (expect was-called :to-be-truthy))))

    (it "clears errors"
      (flycheck-buttercup-with-temp-buffer
        (let ((flycheck-current-errors (list 'foo)))
          (flycheck-report-failed-syntax-check)
          (expect flycheck-current-errors :not :to-be-truthy)))))

  (describe "flycheck-fill-and-expand-error-file-names"

    (it "fills and expands error file names"
      (flycheck-buttercup-with-resource-buffer "global-mode-dummy.el"
        (let* ((absolute-fn (flycheck-buttercup-resource-filename "substitute-dummy"))
               (cwd (file-name-directory absolute-fn))
               (relative-fn (file-name-nondirectory absolute-fn))
               (errors (list (flycheck-error-new :filename "foo")
                             (flycheck-error-new :filename absolute-fn)
                             (flycheck-error-new :filename relative-fn)
                             (flycheck-error-new :filename nil))))
          (expect (mapcar #'flycheck-error-filename
                          (flycheck-fill-and-expand-error-file-names errors
                                                                     cwd))
                  :to-equal
                  (list (flycheck-buttercup-resource-filename "foo")
                        absolute-fn
                        absolute-fn
                        (flycheck-buttercup-resource-filename
                         "global-mode-dummy.el"))))))))

;;; test-status-reporting.el ends here
