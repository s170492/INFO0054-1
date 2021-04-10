#lang racket

(provide lsystem.generate-string)
(provide tlsyst.angle)
(provide tlsyst.lsystem)

(provide sierpinski-triangle)
(provide sierpinski-carpet)
(provide dragon-curve)
(provide tree-growth)
(provide plant-growth)
(provide gosper-curve)

; a ``Turtle L-system'' contains the representation of a ``L-system'' generating
; a ``Turtle string'' and the rotation angle.
; It is represented as a dotted paid (lsystem . angle)

; if tlsyst is a ``Turtle L-system'', (tlsyst.angle tlsyst) is the rotation
; angle (in degree) associated to the ``Turtle string''
(define tlsyst.angle cdr)

; if tlsyst is a ``Turtle L-system'', (tlsyst.lsystem tlsyst) is the representation
; of the L-system
(define tlsyst.lsystem car)

; a ``L-system'' contains an axiom, the representation of a set of production rules
; and the representation of a set of terminal rules.
; It is represented as a triplet (axiom prules trules)

; a set of rules is represented by a list l of representations of rules
; such that (map (lambda (rule) (cons (car rule) (cadr rule))) l) is without repetition

; a rule is represented by either
;;        a pair (s S) where s is a symbol and S a list of symbols,
;;               if there is only one rule associated with s
;;        a triplet (s S p) where s is a symbol, S a list of symbols,
;;               and p a real number such that 0 < p < 1
;;               if there are several rules associated with s

; if lsystem is a ``L-system'', (lsystem.axiom lsystem) is the axiom of the L-system
(define lsystem.axiom car)

; if lsystem is a ``L-system'', (lsystem.prules lsystem) is a function the maps every
; symbol to a sequence of symbols, according to the production rules of the L-system
(define lsystem.prules
  (lambda (lsystem)
    (lambda (s) (apply-rules s (random) (cadr lsystem)))))

; if lsystem is a ``L-system'', (lsystem.prules lsystem) is a function the maps every
; nonterminal symbol to a sequence of terminal symbols,
; according to the production rules of the L-system
(define lsystem.trules
  (lambda (lsystem)
    (lambda (s) (apply-rules s (random) (caddr lsystem)))))

; if s is a symbol, p a real number chosen uniformly at random between 0 and 1
; and rules a set of rules, then (apply-rules s p rules) is the result of applying
; the rules to the symbol s. If several rules are associated to the symbol s, their
; probabilities are summed until it becomes greater than p, and the rule that
; allowed the sum to go above p is chosen
(define apply-rules
  (lambda (s p rules)
    (cond ((null? rules) (list s))
          ((equal? s (caar rules))
           (let ((rule (car rules)))
             (cond ((= (length rule) 2) (cadr rule))
                   ((< p (caddr rule)) (cadr rule))
                   (else (apply-rules s (- p (caddr rule)) (cdr rules))))))
          (else (apply-rules s p (cdr rules))))))


; if lsystem is the representation of a  ``L-system'' and order is a natural,
; (lsystem.generate-string lsystem order) returns an order-th ``L-system string''
; A ``L-system string'' is represented as a list of actual string.
; Note that an order of zero means applying the termination rules to the axiom
(define lsystem.generate-string
    (lambda (lsystem order)
        (lsystem.terminate-string lsystem
                                  (lsystem.develop-string lsystem order))))

; if lsystem is the representation of a ``L-system'' and order is a natural,
; (lsystem.develop-string lsystem order) returns the result of applying the
; production rules of lsystem order times to the axiom of lsystem
(define lsystem.develop-string
  (lambda (lsystem order)
    (if (zero? order) (lsystem.axiom lsystem)
        (apply append (map (lsystem.prules lsystem)
                           (lsystem.develop-string lsystem (- order 1)))))))

; if lsystem is the representation of a ``L-system'' and ls is a list of symbols,
; (lsystem.terminate-string lsystem ls) returns the result of applying the
; termination rules of lsystem to the string ls
(define lsystem.terminate-string
  (lambda (lsystem ls)
    (apply append (map (lsystem.trules lsystem) ls))))


; The ``Turtle L-system'' corresponding the Sierpinsky triangle
(define sierpinski-triangle
  (cons (list '("A" "-" "B" "-" "B")
              (list '("A" ("A" "-" "B" "+" "A" "+" "B" "-" "A"))
                    '("B" ("B" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        120))

; The ``Turtle L-system'' corresponding the Sierpinsky carpet
(define sierpinski-carpet (cons "TODO" 90))

; The ``Turtle L-system'' corresponding the dragon curve
(define dragon-curve (cons "TODO" 45))

; The ``Turtle L-system'' corresponding the tree growth
(define tree-growth (cons "TODO" 25.7))

; The ``Turtle L-system'' corresponding the plant growth
(define plant-growth (cons "TODO" 8))

; The ``Turtle L-system'' corresponding the Gosper curve
(define gosper-curve (cons "TODO" 60))

; The ``Turtle L-system'' corresponding the Koch snowflake
(define koch-snowflake (cons "TODO" 60))