;;; test-configuration.el --- Flycheck Specs: Configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Configuration files and options"

  (describe "flycheck-locate-config-file-by-path"

    (it "just a base name"
      (flycheck-buttercup-with-temp-buffer
        (cd flycheck-test-directory)
        (expect (flycheck-locate-config-file-by-path "flycheck-test.el"
                                                     'emacs-lisp)
                :not :to-be-truthy)))

    (it "with path"
      (flycheck-buttercup-with-temp-buffer
        (cd flycheck-test-directory)
        (expect (flycheck-locate-config-file-by-path "../Makefile"
                                                     'emacs-lisp)
                :to-equal (expand-file-name "../Makefile" flycheck-test-directory))))

    (it "non-existing file"
      (flycheck-buttercup-with-temp-buffer
        (cd flycheck-test-directory)
        (expect (flycheck-locate-config-file-by-path "../foobar" 'emacs-lisp)
                :not :to-be-truthy))))

  (describe "flycheck-locate-config-file-ancestor-directories"

    (it "not existing file"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                                 flycheck-test-directory))
        (expect (flycheck-locate-config-file-ancestor-directories
                 "foo" 'emacs-lisp)
                :not :to-be-truthy)))

    (it "file on same level"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                                 flycheck-test-directory))
        (expect (flycheck-locate-config-file-ancestor-directories
                 "run.el" 'emacs-lisp)
                :to-equal (expand-file-name "run.el" flycheck-test-directory))))

    (it "file on parent level"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "flycheck-test.el"
                                                 flycheck-test-directory))
        (expect (flycheck-locate-config-file-ancestor-directories
                 "Makefile" 'emacs-lisp)
                :to-equal (expand-file-name "../Makefile"
                                            flycheck-test-directory)))))

  (describe "flycheck-locate-config-file-home"

    (it "not existing file"
      (flycheck-buttercup-with-env (list (cons "HOME" flycheck-test-directory))
        (expect (flycheck-locate-config-file-home "foo" 'emacs-lisp)
                :not :to-be-truthy)))

    (it "existing file in parent directory"
      (flycheck-buttercup-with-env (list (cons "HOME" flycheck-test-directory))
        (expect (flycheck-locate-config-file-home "Makefile" 'emacs-lisp)
                :not :to-be-truthy)))

    (it "existing file in home directory"
      (flycheck-buttercup-with-env (list (cons "HOME" flycheck-test-directory))
        (expect (flycheck-locate-config-file-home
                 "flycheck-test.el" 'emacs-lisp)
                :to-equal (expand-file-name "flycheck-test.el"
                                            flycheck-test-directory)))))

  (describe "flycheck-locate-config-file"

    (it "multiple files"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "specs/test-documentation.el"
                                                 flycheck-test-directory))
        (expect (flycheck-locate-config-file
                 '("test-documentation.el" "flycheck-test.el") 'emacs-lisp)
                :to-equal (expand-file-name "specs/test-documentation.el"
                                            flycheck-test-directory))))

    (it "multiple files ordered"
      (flycheck-buttercup-with-temp-buffer
        (setq buffer-file-name (expand-file-name "specs/test-documentation.el"
                                                 flycheck-test-directory))
        (expect (flycheck-locate-config-file
                 '("flycheck-test.el" "test-documentation.el") 'emacs-lisp)
                :to-equal (expand-file-name "flycheck-test.el" flycheck-test-directory)))))

  (describe "flycheck-option-int"

    (it "pass through nil"
      (expect (flycheck-option-int nil) :not :to-be-truthy))

    (it "integer argument"
      (expect (flycheck-option-int 10) :to-equal "10")))

  (describe "flycheck-option-comma-separated-list"

    (it "empty list"
      (expect (flycheck-option-comma-separated-list nil) :not :to-be-truthy))

    (it "with single nil"
      (expect (flycheck-option-comma-separated-list '(nil)) :not :to-be-truthy))

    (it "filter returns nil"
      (expect (flycheck-option-comma-separated-list '(10 20) nil
                                                    (lambda (_x) nil))
              :not :to-be-truthy))

    (it "default separator"
      (expect (flycheck-option-comma-separated-list '("foo" "bar"))
              :to-equal "foo,bar"))

    (it "custom separator"
      (expect (flycheck-option-comma-separated-list '("foo" "bar") ":")
              :to-equal "foo:bar"))

    (it "custom filter"
      (expect (flycheck-option-comma-separated-list '(10 20) nil
                                                    #'number-to-string)
              :to-equal "10,20"))))

;;; test-configuration.el ends here
