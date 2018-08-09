#lang rosette

(require "synthesize.rkt")

(current-bitwidth #f)

(define true-output-file (open-output-file "/Users/jnewcomb/testcases/even_more/proven_true" #:exists 'append))
(define not-true-output-file (open-output-file "/Users/jnewcomb/testcases/even_more/not_proven_true" #:exists 'append))

(call-with-input-file "/Users/jnewcomb/testcases/even_more/xaa"
  (λ (f)
    (for/fold ([counter 0])
              ([line (in-lines f)])
              (begin
                (display (format "~a: " counter))
                (displayln line)
                (if (verify-testcase line)
                    (write-string (string-append line "\n") true-output-file)
                    (write-string (string-append line "\n") not-true-output-file))
                (when (equal? (modulo counter 100) 0)
                  (begin
                    (flush-output true-output-file)
                    (flush-output not-true-output-file)))
                (add1 counter)))))

(close-output-port true-output-file)
(close-output-port not-true-output-file)
