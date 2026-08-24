;;; test-project-runs.el --- Flycheck Specs: Project checkers -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Project checkers"

  (defvar flycheck-test--project-checkers-outside nil)

  (before-each
    (spy-on 'project-current :and-return-value nil)
    (clrhash flycheck--project-error-store)
    (setq flycheck-test--project-checkers-outside flycheck--project-checkers))

  (after-each
    (maphash (lambda (_key state)
               (when-let* ((proc (plist-get state :process)))
                 (when (process-live-p proc)
                   (delete-process proc))))
             flycheck--project-runs)
    (clrhash flycheck--project-runs)
    (setq flycheck--project-checkers flycheck-test--project-checkers-outside))

  (defmacro flycheck-test--with-temp-project (dir-var &rest body)
    "Run BODY in a buffer inside a fresh project, DIR-VAR bound to its key."
    (declare (indent 1))
    `(let ((flycheck-test--project-tmp (make-temp-file "flycheck-prj" t)))
       (unwind-protect
           (with-temp-buffer
             (setq default-directory (file-name-as-directory
                                      flycheck-test--project-tmp))
             (let ((,dir-var (flycheck--project-directory)))
               ,@body))
         ;; Kill any run still going before taking its directory away,
         ;; and give Windows a moment to let go of just-deleted files
         (maphash (lambda (_key state)
                    (when-let* ((proc (plist-get state :process)))
                      (when (process-live-p proc)
                        (delete-process proc))))
                  flycheck--project-runs)
         (let ((attempts 10))
           (while (and (> attempts 0)
                       (not (ignore-errors
                              (delete-directory flycheck-test--project-tmp t)
                              t)))
             (setq attempts (1- attempts))
             (sleep-for 0.1))
           (when (file-directory-p flycheck-test--project-tmp)
             (delete-directory flycheck-test--project-tmp t))))))

  (defun flycheck-test--emacs-command (form)
    "A command list running FORM in a batch Emacs."
    (list (expand-file-name invocation-name invocation-directory)
          "-Q" "--batch" "--eval" (format "%S" form)))

  (defun flycheck-test--define-run-checker (&rest override)
    "Define a fake project checker, with OVERRIDE plist entries."
    (apply #'flycheck-define-project-checker 'test-run "Fake."
           (append override
                   (list :command (flycheck-test--emacs-command
                                   '(princ "gamma.rb:3:boom"))
                         :parser #'flycheck-test--parse-run
                         :enabled (lambda (_dir) t)))))

  (defun flycheck-test--parse-run (output _checker directory)
    "Parse OUTPUT lines of FILE:LINE:MESSAGE under DIRECTORY."
    (delq nil
          (mapcar (lambda (line)
                    (when (string-match "\\`\\(.+\\):\\([0-9]+\\):\\(.+\\)\\'"
                                        line)
                      (flycheck-error-new-at
                       (string-to-number (match-string 2 line)) 1
                       'error (match-string 3 line)
                       :checker 'test-run
                       :filename (expand-file-name (match-string 1 line)
                                                   directory)
                       :buffer nil)))
                  (split-string output "\n" t))))

  (defun flycheck-test--wait-for-run (key)
    "Wait until the run recorded under KEY has finished."
    (let ((tries 200))
      (while (and (> tries 0)
                  (plist-get (gethash key flycheck--project-runs) :process))
        (accept-process-output nil 0.1)
        (setq tries (1- tries)))))

  (describe "flycheck-define-project-checker"

    (it "replaces a checker defined again"
      (flycheck-test--define-run-checker)
      (flycheck-test--define-run-checker :enabled #'ignore)
      (let ((entries (seq-filter (lambda (entry) (eq (car entry) 'test-run))
                                 flycheck--project-checkers)))
        (expect (length entries) :to-equal 1)
        (expect (plist-get (cdr (car entries)) :enabled) :to-be #'ignore)))

    (it "rejects a definition without a parser"
      (expect (flycheck-define-project-checker 'test-broken "Fake."
                :command '("true") :enabled #'ignore)
              :to-throw 'error)))

  (describe "flycheck-check-project"

    (it "feeds the run's diagnostics into the project scope"
      (flycheck-test--define-run-checker)
      (flycheck-test--with-temp-project dir
        (flycheck-check-project)
        (flycheck-test--wait-for-run (cons dir 'test-run))
        (let ((errors (flycheck--project-errors dir)))
          (expect (length errors) :to-equal 1)
          (expect (flycheck-error-filename (car errors))
                  :to-equal (expand-file-name "gamma.rb" dir))
          (expect (flycheck-error-line (car errors)) :to-equal 3)
          (expect (flycheck-error-message (car errors)) :to-equal "boom"))))

    (it "replaces the previous run's results"
      (flycheck-test--define-run-checker)
      (flycheck-test--with-temp-project dir
        (let ((key (cons dir 'test-run)))
          (puthash key (list :process nil
                             :errors (list (flycheck-error-new-at
                                            1 1 'error "stale"
                                            :checker 'test-run
                                            :filename (expand-file-name
                                                       "old.rb" dir))))
                   flycheck--project-runs)
          (flycheck-check-project)
          (flycheck-test--wait-for-run key)
          (let ((errors (flycheck--project-errors dir)))
            (expect (length errors) :to-equal 1)
            (expect (flycheck-error-message (car errors)) :to-equal "boom")))))

    (it "drops the results with the prefix argument"
      (flycheck-test--define-run-checker)
      (flycheck-test--with-temp-project dir
        (puthash (cons dir 'test-run)
                 (list :process nil
                       :errors (list (flycheck-error-new-at
                                      1 1 'error "stale"
                                      :checker 'test-run
                                      :filename (expand-file-name "old.rb"
                                                                  dir))))
                 flycheck--project-runs)
        (let ((generation flycheck--project-diagnostics-generation))
          (flycheck-check-project 'clear)
          (expect (flycheck--project-errors dir) :to-be nil)
          (expect flycheck--project-diagnostics-generation
                  :to-be-greater-than generation))))

    (it "surfaces a parser's error as the run's failure"
      (flycheck-test--define-run-checker
       :parser (lambda (_output _checker _dir)
                 (error "This project needs setting up")))
      (flycheck-test--with-temp-project dir
        (let ((key (cons dir 'test-run)))
          (flycheck-check-project)
          (flycheck-test--wait-for-run key)
          (expect (flycheck--project-errors dir) :to-be nil)
          (expect (plist-get (gethash key flycheck--project-runs) :errors)
                  :to-be nil))))

    (it "says so when no checker applies"
      (flycheck-test--define-run-checker :enabled #'ignore)
      (flycheck-test--with-temp-project _dir
        (should-error (flycheck-check-project) :type 'user-error)))

    (it "says so when the tool is not installed"
      (flycheck-test--define-run-checker
       :command '("flycheck-no-such-tool-anywhere"))
      (flycheck-test--with-temp-project _dir
        (should-error (flycheck-check-project) :type 'user-error))))

  (describe "terraform-validate"

    (it "applies to a directory with Terraform files"
      (let ((enabled (plist-get (alist-get 'terraform-validate
                                           flycheck--project-checkers)
                                :enabled)))
        (flycheck-test--with-temp-project dir
          (expect (funcall enabled dir) :to-be nil)
          (write-region "" nil (expand-file-name "main.tf" dir))
          (expect (funcall enabled dir) :to-be-truthy))))

    (it "reads the recorded validate output"
      (let* ((fixture (expand-file-name
                       "../fixtures/terraform-validate/language/terraform/validate.txt"
                       flycheck-test-resources-directory))
             (output (with-temp-buffer
                       (insert-file-contents fixture)
                       (buffer-string)))
             (errors (flycheck-parse-terraform-validate
                      output 'terraform-validate
                      (expand-file-name "/prj/"))))
        (expect (mapcar #'flycheck-error-line errors) :to-equal '(2 6))
        (expect (flycheck-error-filename (car errors))
                :to-equal (expand-file-name "/prj/outputs.tf"))
        (expect (flycheck-error-column (car errors)) :to-equal 11)
        (expect (flycheck-error-end-column (car errors)) :to-equal 24)
        (expect (flycheck-error-level (car errors)) :to-equal 'error)
        (expect (flycheck-error-message (car errors))
                :to-equal (concat "Reference to undeclared local value: "
                                  "A local value with the name \"missing\" "
                                  "has not been declared."))))

    (it "turns a location-less diagnostic into the run's failure"
      (expect
       (flycheck-parse-terraform-validate
        (concat "{\"valid\": false, \"diagnostics\": "
                "[{\"severity\": \"error\","
                " \"summary\": \"Missing required provider\","
                " \"detail\": \"Run terraform init.\"}]}")
        'terraform-validate "/prj/")
       :to-throw 'error))))

;;; test-project-runs.el ends here
