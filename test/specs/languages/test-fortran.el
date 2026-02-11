;;; test-fortran.el --- Flycheck Specs: Fortran -*- lexical-binding: t; -*-
;;; Code:
(require 'flycheck-buttercup)
(require 'test-helpers)

(describe "Language Fortran"
  (flycheck-buttercup-def-checker-test fortran-gfortran fortran error
    (flycheck-buttercup-should-syntax-check
     "language/fortran/error.f" '(fortran-mode f90-mode)
     '(1 1 error "Non-numeric character in statement label at (1)"
         :checker fortran-gfortran)
     '(2 1 error "Non-numeric character in statement label at (1)"
         :checker fortran-gfortran)
     '(3 1 error "Non-numeric character in statement label at (1)"
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
          :checker fortran-gfortran))))

;;; test-fortran.el ends here
