;;; test-fortran.el --- Flycheck Specs: Fortran -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Fortran"
  (flycheck-buttercup-def-checker-test fortran-gfortran fortran error
    (flycheck-buttercup-should-syntax-check
     "language/fortran/error.f" '(fortran-mode f90-mode)
     '(1 2 error "Non-numeric character in statement label at (1)"
         :checker fortran-gfortran)
     '(2 2 error "Non-numeric character in statement label at (1)"
         :checker fortran-gfortran)
     '(3 2 error "Non-numeric character in statement label at (1)"
         :checker fortran-gfortran)))

  (flycheck-buttercup-def-checker-test fortran-gfortran fortran free-form-error
    (let ((flycheck-gfortran-layout 'free))
      (flycheck-buttercup-should-syntax-check
       "language/fortran/error.f" '(fortran-mode f90-mode)
       '(3 3 error "Expecting END PROGRAM statement at (1)"
           :checker fortran-gfortran))))

  (flycheck-buttercup-def-checker-test fortran-gfortran fortran warning
    (flycheck-buttercup-should-syntax-check
     "language/fortran/warning.f90" '(fortran-mode f90-mode)
     '(1 20 warning "Unused dummy argument 'p' at (1)"
         :checker fortran-gfortran)
     '(18 9 warning "Same actual argument associated with INTENT(IN) argument 'a' and INTENT(OUT) argument 'b' at (1)"
          :checker fortran-gfortran)))

  (describe "reading the tool's output"
    ;; Read from output recorded earlier, so this runs whether or
    ;; not the tool is installed here

    (flycheck-buttercup-def-parse-test fortran-gfortran "language/fortran/error.f"
      '(1 2 error "Non-numeric character in statement label at (1)")
      '(2 2 error "Non-numeric character in statement label at (1)")
      '(3 2 error "Non-numeric character in statement label at (1)"))))

;;; test-fortran.el ends here
