(define (run-command cmd)
  (let-values (((in out pid) (process cmd)))
    (close-output-port out)
    (read-all out)))
