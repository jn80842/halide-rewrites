#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(require "parser.rkt")
(require "../lang.rkt")
(require "../ordering.rkt")

(provide find-ordering)

(define parser-to-ordering
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (λ (a b c) (void)))
   (precs (right EQ)
          (left < > GE LE)
          (left OR AND)
          (left - +)
          (left * / %)
          (left NEG)
          (left !))

   (grammar

    (start [() #f]
           [(error start) $2]
           [(exp) $1])

    (exp [(NUM) '()]
         [(VAR) 'var]
         [(MAX OP exp COMMA exp CP) (list max-idx $3 $5)]
         [(MIN OP exp COMMA exp CP) (list min-idx $3 $5)]
         [(! OP exp CP) (list not-idx $3)]
         [(SELECT OP exp COMMA exp COMMA exp CP) (list select-int-idx $3 $5 $7)]
         [(exp AND exp) (list and-idx $1 $3)]
         [(exp OR exp) (list or-idx $1 $3)]
         [(exp + exp) (list add-idx $1 $3)]
         [(exp - exp) (list sub-idx $1 $3)]
         [(exp * exp) (list mul-idx $1 $3)]
         [(exp / exp) (list div-idx $1 $3)]
         [(exp % exp) (list mod-idx $1 $3)]
         [(exp < exp) (list lt-idx $1 $3)]
         [(exp > exp) (list lt-idx $1 $3)] ;; x > y --> y < x
         [(exp EQ exp) (list eq-int-idx $1 $3)]
         [(exp GE exp) (list lt-idx not-idx $1 $3)] ;; x >= y --> ! x < y
         [(exp LE exp) (list lt-idx not-idx $1 $3)] ;; x <= y --> ! y < x
         [(- exp) (list negate-idx $2)]
         [(OP exp CP) $2]))))

(define (find-ordering s)
  (let ([order (get-new-ordering)])
    (for-each (λ (elt) (if (eq? 'var elt)
                           (increment-ordering-nc-count order)
                           (increment-ordering-operator order elt)))
              (flatten (evaluate-parser parser-to-ordering s)))
    order))