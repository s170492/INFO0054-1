#lang racket
;; 15 mars 2021
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
; It is represented as [insert spec. here]

;; ---> a dotted pair (lsystem . angle)

; if tlsyst is a ``Turtle L-system'', (tlsyst.angle tlsyst) is the rotation
; angle (in degree) associated to the ``Turtle string''
(define tlsyst.angle
  (lambda (tlsyst)
    (cdr tlsyst)))

; if tlsyst is a ``Turtle L-system'', (tlsyst.lsystem tlsyst) is the representation
; of the L-system
(define tlsyst.lsystem
  (lambda (tlsyst)
    (car tlsyst)))


; if lsystem is the representation of a  ``L-system'' and order is a natural,
; (lsystem.generate-string lsystem order) returns an order-th ``L-system string''
; A ``L-system string'' is represented as a list of actual string.
; Note that an order of zero means applying the termination rules to the axiom
(define lsystem.generate-string
    (lambda (lsystem order)
        (lsystem.terminate-string lsystem (lsystem.develop-string lsystem order))))

;; if lsystem is the representation of a  ``L-system'' and lstr is a list of actual string \in N \cup \Sigma
;; (lsystem.terminate-string lsystem lstr) renvoie la liste de string lstr où chaque string non terminal est remplacé par une règle de terminaison associée

(define lsystem.terminate-string
  (lambda (lsystem lstr)
    (if (null? lstr) '()
        (append ((lsystem.t-rules lsystem) (car lstr)) (lsystem.terminate-string lsystem (cdr lstr))))))

; renvoie la order-th chaine non terminale du lsystem
(define lsystem.develop-string
  (lambda (lsystem order)
    (if (zero? order) (lsystem.axiom lsystem)
        (let ([lstr (lsystem.develop-string lsystem (- order 1))])
          (apply append (map (lsystem.p-rules lsystem) lstr))))))


;; Un lsystems est représenté par une liste constituée de 3 listes telles que
;;     -le car est l'axiome
;;     -le cadr -> p-rules
;;     -le caddr -> t-rules

(define lsystem.axiom (lambda (lsystem) (car lsystem)))
(define lsystem.p-rules (lambda (lsystem) (cadr lsystem)))
(define lsystem.t-rules (lambda (lsystem) (caddr lsystem)))

; The ``Turtle L-system'' corresponding the Sierpinsky triangle
(define sierpinski-triangle
  (list (list "A" "-" "B" "-" "B")
        (lambda (s)
          (cond [(eq? s "A") (list "A" "-" "B" "+" "A" "+" "B" "-" "A")]
                [(eq? s "B") (list "B" "B")]
                [else (list s)]))
         (lambda (t)
           (cond [(eq? t "A") (list "T")]
                 [(eq? t "B") (list "T")]
                 [else (list t)]))))

; The ``Turtle L-system'' corresponding the Sierpinsky carpet
(define sierpinski-carpet
  (list (list "T" "-" "T" "-" "T" "-" "T")
        (lambda (s)
          (cond [(eq? s "T") (list "T" "<" "-" "T" "-" "T" ">" "T" "<" "-" "T" "-" "T" "-" "T" ">" "T")]
                [else (list s)]))
        (lambda (t) (list t))))
                             

; The ``Turtle L-system'' corresponding the dragon curve
(define dragon-curve
  (list (list "D")
        (lambda (s)
          (cond [(eq? s "D") (list "-" "D" "+" "+" "E")]
                [(eq? s "E") (list "D" "-" "-" "E" "+")]
                [else (list s)]))
        (lambda (t)
          (cond [(eq? t "D") (list "-" "-" "T" "+" "+" "T")]
                [(eq? t "E") (list "T" "-" "-" "T" "+" "+")]
                [else (list t)]))))

; The ``Turtle L-system'' corresponding the tree growth

(define tree-growth
  (list (list "T")
        (lambda (s)
          (if (eq? s "T") (rand-proc-choice
                           (list
                            (cons 0.33 (list "T" "<" "+" "T" ">" "T" "<" "-" "T" ">" "T"))
                            (cons 0.33 (list "T" "<" "+" "T" ">" "T"))
                            (cons 0.34 (list "T" "<" "-" "T" ">" "T"))))
              (list s)))
        (lambda (t) (list t))))
    

; The ``Turtle L-system'' corresponding the plant growth

(define plant-growth
  (let ([B (lambda (x) (cons "B" x))]
        [T (lambda (x) (cons "T" x))]
        [plus (lambda (x) (cons "plus" x))]
        [minus (lambda (x) (cons "minus" x))])
    (list (list (B 1))
          (lambda (s)
            (if (and (pair? s) (eq? (car s) "B"))
                (let ([x (cdr s)])
                  (list (T x) "<" (plus 5) (B (/ x 2)) ">"
                        (minus 1) (T x) "<" (plus 4) (B (/ x 2)) ">" "<" (minus 7) (B (/ x 2)) ">"
                        (minus 1) (T x) "<" (plus 3) (B (/ x 2)) ">" "<" (minus 5) (B (/ x 2)) ">"
                        (minus 1) (T x) (B (/ x 2))))
                (list s)))
          (lambda (t)
            (if (and (pair? t) (eq? (car t) "B")) (list (T (cdr t)))
                (list t))))))
          
; The ``Turtle L-system'' corresponding the gosper curve
(define gosper-curve
    (list (list "A")
        (lambda (s)
          (cond [(eq? s "A") (list "A" "-" "B" "-" "-" "B" "+" "A" "+" "+" "A" "A" "+" "B" "-")]
                [(eq? s "B") (list "+" "A" "-" "B" "B" "-" "-" "B" "-" "A" "+" "+" "A" "+" "B")]
                [else (list s)]))
        (lambda (t)
          (cond [(eq? t "A") (list "T")]
                [(eq? t "E") (list "T")]
                [else (list t)]))))


; Exemple de l'énoncé du projet :

(define lsystem-example
  (let ([A (lambda (x) (cons "A" x))]
        [F (lambda (x) (cons "F" x))])
    (list (list (A 16))
          (lambda (s)
            (cond [(and (pair? s) (eq? (car s) "A")) (list (A (/ (cdr s) 2)) "B")]
                  [(eq? s "B") (list "G" "B")]
                  [else (list s)]))
          (lambda (t)
            (cond [(and (pair? t) (eq? (car t) "A")) (list (F (cdr t)))]
                  [(eq? t "B") (list "G")]
                  [else (list t)])))))
;; Fonctions auxilliaires

;; si ls est une liste dont de paires pointées dont les cdr sont des procédures f et dont le car est un nombre p_f tels que somme p_f = 1
;; renvoie la procédure f avec probabilité p_f
(define rand-proc-choice
  (lambda (ls)
      (proc-choice (cons ls (random)))))

;; si ls.r est une paire pointée dont
;;     - le car est une liste de paires pointées ((f1 . p_f1) ... (fn . p_fn)) dont les cdr sont des procédures fi et dont le car est un nombre p_fi tels que somme sur i des p_fi vaut 1
;;     - le cdr est un nombre r tel que 0 < r < 1 
;; alors (proc-choice ls.r) renvoie la procédure f1 si r < p_f1
;;                                               fi si p_f(i-1) =< r < p_fi 
(define proc-choice
      (lambda (ls.r)
        (let ([r (cdr ls.r)] [p (caaar ls.r)])
        (if (< r p) (cdaar ls.r)
            (proc-choice (cons (cdar ls.r) (- r p)))))))
#|
(define flat
  (lambda (ls)
    (cond [(null? ls) '()]
          [(list? (car ls)) (flat (append (car ls)(cdr ls)))]
          [else (cons (car ls) (flat (cdr ls)))])))
|#
      
     
