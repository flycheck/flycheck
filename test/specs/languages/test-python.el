;;; test-python.el --- Flycheck Specs: Python -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Python"
  (flycheck-buttercup-def-checker-test python-flake8 python syntax-error
    (let ((python-indent-guess-indent-offset nil)
          (flycheck-python-flake8-executable "python3"))
      (flycheck-buttercup-should-syntax-check
       "language/python/syntax-error.py" 'python-mode
       '(3 7 error "SyntaxError: invalid syntax" :id "E999"
           :checker python-flake8))))

  (flycheck-buttercup-def-checker-test python-flake8 python nil
    (let ((flycheck-python-flake8-executable "python3"))
      (flycheck-buttercup-should-syntax-check
       "language/python/test.py" 'python-mode
       '(5 1 warning "'.antigravit' imported but unused" :id "F401"
           :checker python-flake8)
       '(7 1 warning "expected 2 blank lines, found 1" :id "E302"
           :checker python-flake8)
       '(12 29 warning "unexpected spaces around keyword / parameter equals"
            :id "E251" :checker python-flake8)
       '(12 31 warning "unexpected spaces around keyword / parameter equals"
            :id "E251" :checker python-flake8)
       '(22 1 error "undefined name 'antigravity'" :id "F821"
            :checker python-flake8))))

  (flycheck-buttercup-def-checker-test python-pyright python nil
    (let ((flycheck-disabled-checkers '(python-mypy))
          (flycheck-checkers '(python-pyright)))
      (flycheck-buttercup-should-syntax-check
       "language/python/invalid_type.py" 'python-mode
       '(2 12 error "Expression of type \"str\" cannot be assigned to return type \"int\"\n  \"str\" is incompatible with \"int\""
           :checker python-pyright
           :end-line 2
           :end-column 13))))

  (flycheck-buttercup-def-checker-test python-mypy python nil
    (let ((flycheck-disabled-checkers '(python-flake8))
          (flycheck-checkers '(python-mypy))
          (flycheck-python-mypy-cache-dir null-device))
      (flycheck-buttercup-should-syntax-check
       "language/python/invalid_type.py" 'python-mode
       '(2 12 error "Incompatible return value type (got \"str\", expected \"int\")"
           :checker python-mypy))))

  (flycheck-buttercup-def-checker-test python-pylint python nil
    (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
          (flycheck-python-pylint-executable "python3"))
      (flycheck-buttercup-should-syntax-check
       "language/python/test.py" 'python-mode
       '(1 1 info "Missing module docstring" :id "missing-module-docstring" :checker python-pylint)
       '(5 1 error "No name 'antigravit' in module 'python'" :id "no-name-in-module"
           :checker python-pylint)
       '(5 1 warning "Unused import antigravit" :id "unused-import"
           :checker python-pylint)
       '(7 1 info "Missing class docstring" :id "missing-class-docstring" :checker python-pylint)
       '(7 1 warning "Class 'Spam' inherits from object, can be safely removed from bases in python3"
           :id "useless-object-inheritance" :checker python-pylint)
       '(9 5 info "Method name \"withEggs\" doesn't conform to snake_case naming style"
           :id "invalid-name" :checker python-pylint)
       '(9 5 info "Missing function or method docstring" :id "missing-function-docstring" :checker python-pylint)
       '(9 5 warning "Method could be a function" :id "no-self-use"
           :checker python-pylint)
       '(12 5 info "Missing function or method docstring" :id "missing-function-docstring" :checker python-pylint)
       '(12 5 warning "Either all return statements in a function should return an expression, or none of them should."
            :id "inconsistent-return-statements" :checker python-pylint)
       '(12 5 warning "Method could be a function"
            :id "no-self-use" :checker python-pylint)
       '(14 16 error "Module 'sys' has no 'python_version' member" :id "no-member"
            :checker python-pylint)
       '(22 1 error "Undefined variable 'antigravity'" :id "undefined-variable"
            :checker python-pylint))))

  (flycheck-buttercup-def-checker-test python-pylint python no-symbolic-id
    (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
          (flycheck-pylint-use-symbolic-id nil)
          (flycheck-python-pylint-executable "python3"))
      (flycheck-buttercup-should-syntax-check
       "language/python/test.py" 'python-mode
       '(1 1 info "Missing module docstring" :id "C0114" :checker python-pylint)
       '(5 1 error "No name 'antigravit' in module 'python'" :id "E0611"
           :checker python-pylint)
       '(5 1 warning "Unused import antigravit" :id "W0611"
           :checker python-pylint)
       '(7 1 info "Missing class docstring" :id "C0115" :checker python-pylint)
       '(7 1 warning "Class 'Spam' inherits from object, can be safely removed from bases in python3"
           :id "R0205" :checker python-pylint)
       '(9 5 info "Method name \"withEggs\" doesn't conform to snake_case naming style"
           :id "C0103" :checker python-pylint)
       '(9 5 info "Missing function or method docstring" :id "C0116" :checker python-pylint)
       '(9 5 warning "Method could be a function" :id "R0201"
           :checker python-pylint)
       '(12 5 info "Missing function or method docstring" :id "C0116" :checker python-pylint)
       '(12 5 warning "Either all return statements in a function should return an expression, or none of them should."
            :id "R1710" :checker python-pylint)
       '(12 5 warning "Method could be a function"
            :id "R0201" :checker python-pylint)
       '(14 16 error "Module 'sys' has no 'python_version' member" :id "E1101"
            :checker python-pylint)
       '(22 1 error "Undefined variable 'antigravity'" :id "E0602"
            :checker python-pylint))))

  (flycheck-buttercup-def-checker-test python-pylint python negative-columns
    (let ((flycheck-disabled-checkers '(python-flake8 python-mypy))
          (python-indent-guess-indent-offset nil)
          (flycheck-python-pylint-executable "python3"))
      (flycheck-buttercup-should-syntax-check
       "language/python/gh_1383.py" 'python-mode
       '(2 1 warning "Unused import sys"
           :id "unused-import" :checker python-pylint)
       '(4 1 warning "String statement has no effect"
           :id "pointless-string-statement" :checker python-pylint))))

  (flycheck-buttercup-def-checker-test python-pycompile python python27
    (assume (executable-find "python2"))
    (let ((flycheck-disabled-checkers '(python-flake8 python-pylint python-mypy))
          (flycheck-python-pycompile-executable "python2")
          (python-indent-guess-indent-offset nil))
      (flycheck-buttercup-should-syntax-check
       "language/python/syntax-error.py" 'python-mode
       `(3 nil error "invalid syntax" :checker python-pycompile))))

  (flycheck-buttercup-def-checker-test python-pycompile python has-no-warnings
    (let ((flycheck-disabled-checkers '(python-flake8 python-pylint python-mypy)))
      (flycheck-buttercup-should-syntax-check
       "language/python/test.py" 'python-mode))))

;;; test-python.el ends here
