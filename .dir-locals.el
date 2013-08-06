;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode
  (flycheck-emacs-lisp-initialize-packages . t)
  (eval . (setq flycheck-emacs-lisp-package-user-dir
                (expand-file-name (format ".cask/%s/elpa" emacs-version))))))
