#lang rosette

(require racket/engine)

(require "synthesize.rkt")

(current-bitwidth #f)

;(define true-output-file (open-output-file "/Users/jnewcomb/testcases/verify/simplifier_testcases_proven_true" #:exists 'append))
;(define not-true-output-file (open-output-file "/Users/jnewcomb/testcases/verify/simplifier_testcases_not_proven_true" #:exists 'append))
;(define timeout-file (open-output-file "/Users/jnewcomb/testcases/verify/simplifier_testcases_timeout2" #:exists 'append))

#;(call-with-input-file "/Users/jnewcomb/testcases/verify/simplifier_testcases_timeout"
  (λ (f)
    (for/fold ([counter 0])
              ([line (in-lines f)])
              (begin
                (display (format "~a: " counter))
                (displayln line)
                (with-handlers ([exn:fail? (λ (exn) (begin (displayln "z3 timeout")
                                                           (write-string (string-append line "\n") timeout-file)
                                                           'fail))])
                  (if (verify-testcase line)
                    (write-string (string-append line "\n") true-output-file)
                    (write-string (string-append line "\n") not-true-output-file)))
                (when (equal? (modulo counter 100) 0)
                  (begin
                    (flush-output true-output-file)
                    (flush-output not-true-output-file)
                    (flush-output timeout-file)))
                (add1 counter)))))

;(close-output-port true-output-file)
;(close-output-port not-true-output-file)
