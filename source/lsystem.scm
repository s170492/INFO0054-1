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
(provide koch-snowflake)
(provide sierpinski-arrowhead)
(provide branch-growth)
(provide binary-tree)
(provide koch-antisnowflake)
(provide koch-curve)
(provide hilbert-curve)
(provide cesaro-fractal85)
(provide cesaro-fractal90)
(provide twindragon-curve)
(provide terdragon-curve)
(provide pentadendrite)
(provide gosper-island)
(provide minkowski-island)
(provide moore-curve)
(provide fibonacci-fractal)


;; DEFINITION

; a ``Turtle string'' is a finite sequence of elements that are part of the
; following symbols :
;    T, T[x], F, F[x], <, >, +, +[x], -, -[x] where x is a number.
; It is represented as a list whose elements are part of the following strings :
;    "T", "T[x]", "F", "F[x]", "<", ">", "+", "+[x]", "-", "-[x]"
; where x is a number.

; a ``Turtle L-system'' contains the representation of a ``L-system'' generating
; a ``Turtle string'' and the rotation angle.
; It is represented as a dotted paid (lsystem . angle)

; a ``L-system'' contains an ``axiom'', the representation of a set of
; production ``rules'' and the representation of a set of terminal ``rules''.
; It is represented as a triplet (axiom p-rules t-rules)

; an ``axiom'' is a list of ``symbols'' whose ``parameters'', if any, are numbers

; a set of ``rules'' is represented by a list l of representations of ``rules''

; a ``rule'' is represented by either
;;        a pair (s ls)  if there is no probability associated with it,
;;               where s is the ``symbol'' the ``rule'' applies to
;;                      whose ``parameters'', if any, are ``variables''
;;               and ls is the list of ``symbols'' the ``rule'' translates to
;;        a triplet (s ls p)  if there is a probability associated with it,
;;               s and ls are as above and p is a number such that 0 < p < 1

; a ``symbol'' is either
;;        a string, if it as no parameter
;;        a list whose car is a string and whose cdr is
;;               a list of ``arithmetical expression'', its ``parameters''

; an ``arithmetical expression'' is either a number, a ``variable'' or a list
; whose car is one of the special scheme symbols '+, '-, '*, '/
; and whose cdr is a list of ``arithmetical expressions''

; a ``variable'' is a scheme symbol different from '+, '-, '*, '/ and '()


;; TURTLE L-SYSTEM GETTERS

; if tlsyst is a ``Turtle L-system'', (tlsyst.angle tlsyst) is the rotation
; angle (in degree) associated to the ``Turtle L-system''
(define tlsyst.angle cdr)

; if tlsyst is a ``Turtle L-system'', (tlsyst.lsystem tlsyst) is the
; representation of the L-system associated with the ``Turtle L-system''
(define tlsyst.lsystem car)


;; L-SYSTEM GETTERS

; if lsystem is a ``L-system'', (lsystem.axiom lsystem) is the ``axiom'' of the L-system
(define lsystem.axiom car)

; if lsystem is a ``L-system'', (lsystem.p-rules lsystem) is a function the maps
; every symbol to a list of symbols,
; according to the production rules of the L-system
(define lsystem.p-rules
  (lambda (lsystem)
    (lambda (s) (apply-rules s (cadr lsystem)))))

; if lsystem is a ``L-system'', (lsystem.p-rules lsystem) is a function the maps
; every nonterminal symbol to a list of terminal symbols,
; according to the production rules of the L-system
(define lsystem.t-rules
  (lambda (lsystem)
    (lambda (s) (apply-rules s (caddr lsystem)))))


;; RULE GETTERS AND PREDICATES

; if rule is a ``rule'', (proba? rule) returns true if there is a
; probability associated with the rule
(define proba?
  (lambda (rule) (= (length rule) 3)))

; if rule is a ``rule'', (rule.applies-to rule) is the ``symbol'' it applies to
(define rule.applies-to car)

; if rule is a ``rule'', (rule.translates-to rule) is the list of ``symbols''
; the rules translates to
(define rule.translates-to cadr)

; if rule is a ``rule'' with a probability associated with it,
; (rule.proba rule) is the probability
(define rule.proba caddr)


;; SYMBOL GETTERS AND PREDICATES

; if s is a ``symbol'', (param? s) returns true if s has parameters
(define param? list?)

; if s is a ``symbol'', (symb.head s) is the string it contains
(define symb.head
  (lambda (s) (if (param? s) (car s) s)))

; if s is a symbol ``symbol'' with parameters, (symb.params s) its list of parameters
(define symb.params cdr)

; if a and b are ``symbols'', (symb= a b) returns true if a and b represent the
; same symbol, that is if the string they contains are the same
(define symb=?
  (lambda (a b)
      (string=? (symb.head a) (symb.head b))))


;; APPLYING A SET OF RULES TO A SYMBOL

; if s is a ``symbol'' whose parameters, if any, are numbers, and rules a set of ``rules''
; then (apply-rules s rules) is the result of applying the ``rules'' to
; the ``symbol'' s. If several ``rules'' are associated to the ``symbol'' s,
; a random number between 0 and 1 is chosen and the function apply-stoch-rules
; is called to chose which ``rule'' to apply to the ``symbol''. 
(define apply-rules
  (lambda (s rules)
    (if (null? rules) (list s)
        (let ((rule (car rules)))
          (cond ((and (symb=? s (rule.applies-to rule)) (proba? rule))
                 (apply-stoch-rules s rules (random)))  ; probability associated
                ((symb=? s (rule.applies-to rule))
                 (apply-rule s rule))  ; no probability associated
                (else (apply-rules s (cdr rules))))))))

; if s is a ``symbol'' whose parameters, if any, are numbers, p a number strictly
; between 0 and 1 and rules a set of ``rules'',
; then (apply-stoch-rules s rules p) is the result of applying the ``rules'' to the
; ``symbol'' s. To choose the ``rule'' to apply, the probabilities associated
; with the ``rules'' (reporting to s) are summed until it goes above p,
; then the last ``rule'' to be added to the sum is applied to the ``symbol''
(define apply-stoch-rules
  (lambda (s rules p)
    (if (null? rules) (list s)
        (let ((rule (car rules)))
          (cond ((and (symb=? s (rule.applies-to rule)) (< p (rule.proba rule)))
                 (apply-rule s rule))  ; sum is above p
                ((symb=? s (rule.applies-to rule))
                 (apply-stoch-rules s (cdr rules) (- p (rule.proba rule))))  ; sum is below p
                (else (apply-stoch-rules s (cdr rules) p)))))))


;; APPLYING A SINGLE RULE TO A SYMBOL

; if s is a ``symbol'' whose parameters, if any, are numbers, and if rule is a ``rule'',
; then (apply-rule s rule) is the result of applying the ``rule'' to the ``symbol''
(define apply-rule
  (lambda (s rule)
    (if (not (param? s)) (rule.translates-to rule)  ; no parameter to deal with
        (let* ((values (symb.params s))
               (variables (symb.params (rule.applies-to rule)))
               (ls (rule.translates-to rule)))
          (map (lambda (s) (eval-symbol s variables values)) ls)))))

; if s is a ``symbol'', variables is a list of ``variables'' and values is a
; list of numbers such that every ``variable'' corresponds to a value, then
; (eval-symbol s variables values) is the ``symbol'' obtained by replacing every
; ``variable'' in s by its value and simplifying
(define eval-symbol
  (lambda (s variables values)
    (if (not (param? s)) s
        (cons (symb.head s) (map (lambda (e) (eval-ari-exp e variables values))
                                 (symb.params s))))))

; if exp is an ``arithmetical expression'', variables is a list of ``variables''
; and values is a  list of numbers such that every ``variable'' corresponds to a value,
; then (eval-ari-exp exp variables values) corresponds to the number obtained
; by replacing every ``variable'' in exp by its value and simplifying
(define eval-ari-exp
  (lambda (exp variables values)
    (cond ((number? exp) exp)
          ((and (symbol? exp) (eq? exp (car variables))) (car values))
          ((symbol? exp) (eval-ari-exp exp (cdr variables) (cdr values)))
          (else (let ((op (car exp))
                      (args (map (lambda (e) (eval-ari-exp e variables values))
                                 (cdr exp))))
                  (cond ((eq? op '+) (apply + args))
                        ((eq? op '-) (apply - args))
                        ((eq? op '*) (apply * args))
                        ((eq? op '/) (apply / args))))))))


;; GENERATING A STRING FROM A L-SYSTEM

; if lsystem is the representation of a ``L-system'' generating a ``Turtle string''
; and order is a natural number,
; then (lsystem.generate-string lsystem order) returns an order-th ``Turtle string''
; Note that an order of zero means applying the termination rules to the axiom
(define lsystem.generate-string
    (lambda (lsystem order)
      (lstr->tstr
       (lsystem.terminate-string lsystem
                                 (lsystem.develop-string lsystem order)))))

; if lsystem is the representation of a ``L-system'' and order is a natural,
; (lsystem.develop-string lsystem order) returns the result of applying the
; production rules of lsystem order times to the axiom of lsystem
(define lsystem.develop-string
  (lambda (lsystem order)
    (if (zero? order) (lsystem.axiom lsystem)
        (apply append (map (lsystem.p-rules lsystem)
                           (lsystem.develop-string lsystem (- order 1)))))))

; A ``L-system string'' is represented as a list of ``symbols''

; if lsystem is the representation of a ``L-system'' and ls is a ``L-system string''
; (lsystem.terminate-string lsystem ls) returns the result of applying the
; termination rules of the ``L-system'' to the ``L-system string''
(define lsystem.terminate-string
  (lambda (lsystem ls)
    (apply append (map (lsystem.t-rules lsystem) ls))))

; if lsymb is a ``symbol'', (lsymb->tsymb lsymb) is the corresponding ``Turtle symbol''
; that is, any symbol of the form ("S" x) is converted to "S[x]"
(define lsymb->tsymb
  (lambda (lsymb)
    (if (not (param? lsymb)) lsymb
        (string-append (symb.head lsymb)
                       "["
                       (string-join (map ~r (symb.params lsymb)) ", ")
                       "]"))))
                                                    
; if lstr is a ``L-system string'', (lstr->tstr lstr) is a ``Turtle string''
(define lstr->tstr
  (lambda (lstr)
    (map lsymb->tsymb lstr)))


;; EXAMPLES OF L-SYSTEMS

; The ``Turtle L-system'' corresponding to the Sierpinsky triangle
(define sierpinski-triangle
  (cons (list '("A" "-" "B" "-" "B")
              (list '("A" ("A" "-" "B" "+" "A" "+" "B" "-" "A"))
                    '("B" ("B" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        120))

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
  (cons (list '(("B" 1))
              (list '(("B" x) (("T" x) "<" ("+" 5) ("B" (* 0.5 x)) ">" "<" ("-" 7) ("B" (* 0.5 x)) ">"
                                ("-" 1) ("T" x) "<" ("+" 4) ("B" (* 0.5 x)) ">" "<" ("-" 7) ("B" (* 0.5 x)) ">"
                                ("-" 1) ("T" x) "<" ("+" 3) ("B" (* 0.5 x)) ">" "<" ("-" 5) ("B" (* 0.5 x)) ">"
                                ("-" 1) ("T" x) ("B" (* 0.5 x)))))
              (list '(("B" x) (("T" x)))))
        8))

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

; The ``Turtle L-system'' corresponding to the Sierpinsky arrowhead
(define sierpinski-arrowhead
  (cons (list '("A")
              (list '("A" ("B" "-" "A" "-" "B"))
                    '("B" ("A" "+" "B" "+" "A")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        60))

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
              (list '("A" ("B" "<" "+" "A" ">" "-" "A") 0.75)
                    '("A" ("A") 0.25)
                    '("B" ("B" "B")))
              (list '("A" ("T"))
                    '("B" ("T"))))
        45))

; The ``Turtle L-system'' corresponding to the Koch antisnowflake
(define koch-antisnowflake
  (cons (list '("T" "-" "-" "T" "-" "-" "T")
              (list '("T" ("T" "-" "T" "+" "+" "T" "-" "T")))
              (list))
        60))

; The ``Turtle L-system'' corresponding to the Koch curve
(define koch-curve
  (cons (list '("T")
              (list '("T" ("T" "-" "T" "+" "T" "+" "T" "-" "T")))
              (list))
        90))

; The ``Turtle L-system'' corresponding to the Hilbert curve
(define hilbert-curve
  (cons (list '("A")
              (list '("A" ("+" "B" "T" "-" "A" "T" "A" "-" "T" "B" "+"))
                    '("B" ("-" "A" "T" "+" "B" "T" "B" "+" "T" "A" "-")))
              (list '("A" ())
                    '("B" ())))
        90))

; The ``Turtle L-system'' corresponding to the Cesaro fractal (85)
(define cesaro-fractal85
  (cons (list '("T")
              (list '("T" ("T" "-" "T" ("+" 2) "T" "-" "T")))
              (list))
        85))

; The ``Turtle L-system'' corresponding to the Cesaro fractal (90)
(define cesaro-fractal90
  (cons (list '("T")
              (list '("T" ("T" "-" "T" ("+" 2) "T" "-" "T")))
              (list))
        90))

; The ``Turtle L-system'' corresponding to the twindragon curve
(define twindragon-curve
  (cons (list '("T" "X" "+" "T" "X" "+")
              (list '("X" ("X" "+" "Y" "T"))
                    '("Y" ("T" "X" "-" "Y")))
              (list '("X" ())
                    '("Y" ())))
        90))

; The ``Turtle L-system'' corresponding to the terdragon curve
(define terdragon-curve
  (cons (list '("T")
              (list '("T" ("T" "+" "T" "-" "T")))
              (list))
        120))

; The ``Turtle L-system'' corresponding to the pentadendrite
(define pentadendrite
  (cons (list '("T")
              (list '("T" ("T" "+" "T" "-" "T" "-" "-" "T" "+" "T" "+" "T")))
              (list))
        72))

; The ``Turtle L-system'' corresponding to the Gosper island
(define gosper-island
  (cons (list '("T" "-" "T" "-" "T" "-" "T" "-" "T" "-" "T")
              (list '("T" ("T" "-" "T" "+" "T")))
              (list))
        60))

; The ``Turtle L-system'' corresponding to the Minkowski island
(define minkowski-island
  (cons (list '("T" "+" "T" "+" "T" "+" "T")
              (list '("T" ("T" "-" "T" "+" "T" "+" "T" "T" "-" "T" "-" "T" "+" "T")))
              (list))
        90))

; The ``Turtle L-system'' corresponding to the Moore curve
(define moore-curve
  (cons (list '("L" "T" "L" "+" "T" "+" "L" "T" "L")
              (list '("L" ("-" "R" "T" "+" "L" "T" "L" "+" "T" "R" "-"))
                    '("R" ("+" "L" "T" "-" "R" "T" "R" "-" "T" "L" "+")))
              (list '("R" ())
                    '("L" ())))
        90))

; The ``Turtle L-system'' corresponding to the Fibonacci word fractal
(define fibonacci-fractal
  (cons (list '("L")
              (list '("L" ("-" "R" "+" "L" "K" "L" "+" "R" "-"))
                    '("R" ("+" "L" "-" "R" "Q" "R" "-" "L" "+"))
                    '("K" ("L"))
                    '("Q" ("R")))
              (list '("L" ("T"))
                    '("R" ("T"))
                    '("K" ("T"))
                    '("Q" ("T"))))
        90))