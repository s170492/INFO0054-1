#lang racket

(provide lsystem.generate-string)
(provide tlsyst.angle)
(provide tlsyst.lsystem)

(provide sierpinski-triangle)
(provide sierpinski-arrowhead)
(provide sierpinski-carpet)
(provide dragon-curve)
(provide tree-growth)
(provide plant-growth)
(provide branch-growth)
(provide binary-tree)
(provide gosper-curve)
(provide koch-snowflake)
(provide koch-antisnowflake)
(provide koch-curve)

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
; It is represented as a triplet (axiom p-rules t-rules)

; a set of rules is represented by a list l of representations of rules
; such that (map (lambda (rule) (cons (car rule) (cadr rule))) l) is without repetition

; a rule is represented by either
;;        a pair (s S) where s is a symbol and S a list of symbols,
;;               if there is only one rule associated with s
;;        a triplet (s S p) where s is a symbol, S a list of symbols,
;;               and p a real number such that 0 < p < 1
;;               if there are several rules associated with s

; a symbol is represented by either
;;        a string if it as no parameter
;;        a list whose car is a string and whose cdr is a
;;               (non necessarily flat) list of scheme symbols
;;               the special symbols '+, '*, '- and '/ cannot be used to name parameters
;;               they should not be confused with the turtle symbols "+" and "-"!

; if lsystem is a ``L-system'', (lsystem.axiom lsystem) is the axiom of the L-system
(define lsystem.axiom car)

; if lsystem is a ``L-system'', (lsystem.p-rules lsystem) is a function the maps every
; symbol to a sequence of symbols, according to the production rules of the L-system
(define lsystem.p-rules
  (lambda (lsystem)
    (lambda (s) (apply-rules s (random) (cadr lsystem)))))

; if lsystem is a ``L-system'', (lsystem.p-rules lsystem) is a function the maps every
; nonterminal symbol to a sequence of terminal symbols,
; according to the production rules of the L-system
(define lsystem.t-rules
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
        (apply append (map (lsystem.p-rules lsystem)
                           (lsystem.develop-string lsystem (- order 1)))))))

; if lsystem is the representation of a ``L-system'' and ls is a list of symbols,
; (lsystem.terminate-string lsystem ls) returns the result of applying the
; termination rules of lsystem to the string ls
(define lsystem.terminate-string
  (lambda (lsystem ls)
    (apply append (map (lsystem.t-rules lsystem) ls))))


; The ``Turtle L-system'' corresponding to the Sierpinsky triangle
(define sierpinski-triangle
  (cons (list '("A" "-" "B" "-" "B")
              (list '("A" ("A" "-" "B" "+" "A" "+" "B" "-" "A"))
                    '("B" ("B" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        120))

; The ``Turtle L-system'' corresponding to the Sierpinsky arrowhead
(define sierpinski-arrowhead
  (cons (list '("A")
              (list '("A" ("B" "-" "A" "-" "B"))
                    '("B" ("A" "+" "B" "+" "A")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        60))

; The ``Turtle L-system'' corresponding to the Sierpinsky carpet
(define sierpinski-carpet
  (cons (list '("T" "-" "T" "-" "T" "-" "T")
              (list '("T" ("T" "<" "-" "T" "-" "T" ">" "T" "<" "-" "T" "-" "T" "-" "T" ">" "T")))
              (list))
        90))

; The ``Turtle L-system'' corresponding to the dragon curve
(define dragon-curve
  (cons (list '("D")
              (list '("D" ("-" "D" "+" "+" "E"))
                    '("E" ("D" "-" "-" "E" "+")))
              (list '("D" ("-" "-" "T" "+" "+" "T"))
                    '("E" ("T" "-" "-" "T" "+" "+"))))
        45))

; The ``Turtle L-system'' corresponding to the tree growth
(define tree-growth
  (cons (list '("T")
              (list '("T" ("T" "<" "+" "T" ">" "T" "<" "-" "T" ">" "T") 0.33)
                    '("T" ("T" "<" "+" "T" ">" "T") 0.33)
                    '("T" ("T" "<" "-" "T" ">" "T") 0.34))
              (list))
        25.7))

; The ``Turtle L-system'' corresponding to the plant growth
(define plant-growth
  (cons (list '(("B" x))
              (list '(("B" x) (("T" x) "<" ("+" 5) ("B" (* 0.5 x)) ">" "<" ("-" 7) ("B" (* 0.5 x)) ">"
                               "-" ("T" x) "<" ("+" 4) ("B" (* 0.5 x)) ">" "<" ("-" 7) ("B" (* 0.5 x)) ">"
                               "-" ("T" x) "<" ("+" 3) ("B" (* 0.5 x)) ">" "<" ("-" 5) ("B" (* 0.5 x)) ">"
                               "-" ("T" x) ("B" (* 0.5 x)))))
              (list '(("B" x) (("T" x)))))
        8))

; The ``Turtle L-system'' corresponding to the branch growth
(define branch-growth
  (cons (list '("X")
              (list '("X" ("T" "-" "<" "<" "X" ">" "+" "X" ">" "+" "T" "<" "+" "T" "X" ">" "-" "X"))
                    '("T" ("T" "T")))
              (list '("X" ())))
        25))

; The ``Turtle L-system'' corresponding to a binary tree
(define binary-tree
  (cons (list '("A")
              (list '("A" ("B" "<" "+" "A" ">" "-" "A"))
                    '("B" ("B" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        45))

; The ``Turtle L-system'' corresponding to the Gosper curve
(define gosper-curve
  (cons (list '("A")
              (list '("A" ( "A" "-" "B" "-" "-" "B" "+" "A" "+" "+" "A" "A" "+" "B" "-"))
                    '("B" ("+" "A" "-" "B" "B" "-" "-" "B" "-" "A" "+" "+" "A" "+" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        60))

; The ``Turtle L-system'' corresponding to the Koch snowflake
(define koch-snowflake
  (cons (list '("T" "-" "-" "T" "-" "-" "T")
              (list '("T" ("T" "+" "T" "-" "-" "T" "+" "T")))
              (list))
        60))

; The ``Turtle L-system'' corresponding to the Koch antisnowflake
(define koch-antisnowflake
  (cons (list '("T" "-" "-" "T" "-" "-" "T")
              (list '("T" ("T" "-" "T" "+" "+" "T" "-" "T")))
              (list))
        60))

; The ``Turtle L-system'' corresponding to the Koch curve
(define koch-curve
  (cons (list '("T")
              (list '( "T" ("T" "-" "T" "+" "T" "+" "T" "-" "T")))
              (list))
        90))