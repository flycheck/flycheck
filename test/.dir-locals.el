;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (eval . (push default-directory flycheck-emacs-lisp-load-path))
  (eval . (require 'test-helper (expand-file-name "test-helper.el" default-directory)))))
