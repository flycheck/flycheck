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
                         "global-mode-dummy.el"))))))

    (describe "on a remote host"

      ;; A checker running on the remote host reports host-local names.
      ;; Expanding those against the remote working directory has to keep
      ;; the host, or Flycheck opens the same path on the local machine.
      (it "keeps the host when expanding an error's file name"
        (let ((errors (list (flycheck-error-new :filename "/home/u/a.c"))))
          (expect (flycheck-error-filename
                   (car (flycheck-fill-and-expand-error-file-names
                         errors "/ssh:host:/home/u/")))
                  :to-equal "/ssh:host:/home/u/a.c")))

      (it "keeps the host when expanding a related location"
        (let* ((relation (flycheck-related-location-new
                          :filename "/home/u/other.c" :line 3))
               (errors (list (flycheck-error-new :filename "/home/u/a.c"
                                                 :relations (list relation)))))
          (flycheck-fill-and-expand-error-file-names errors "/ssh:host:/home/u/")
          (expect (flycheck-related-location-filename relation)
                  :to-equal "/ssh:host:/home/u/other.c")))

      ;; The project-checker twin of the above: cargo reports absolute
      ;; paths for anything outside the workspace, such as a registry
      ;; dependency, and those must not resolve on the local machine.
      (it "keeps the host when expanding a project checker's error"
        (let ((errors (list (flycheck-error-new
                             :filename "/home/u/.cargo/registry/src/x.rs"))))
          (expect (flycheck-error-filename
                   (car (flycheck--project-expand-error-files
                         errors "/ssh:host:/home/u/proj/")))
                  :to-equal "/ssh:host:/home/u/.cargo/registry/src/x.rs"))))))

;;; test-status-reporting.el ends here
