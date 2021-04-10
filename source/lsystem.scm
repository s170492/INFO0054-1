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
; It is represented as [insert spec. here]

; if tlsyst is a ``Turtle L-system'', (tlsyst.angle tlsyst) is the rotation
; angle (in degree) associated to the ``Turtle string''
(define tlsyst.angle "TODO")

; if tlsyst is a ``Turtle L-system'', (tlsyst.lsystem tlsyst) is the representation
; of the L-system
(define tlsyst.lsystem "TODO")


; if lsystem is the representation of a  ``L-system'' and order is a natural,
; (lsystem.generate-string lsystem order) returns an order-th ``L-system string''
; A ``L-system string'' is represented as a list of actual string.
; Note that an order of zero means applying the termination rules to the axiom
(define lsystem.generate-string
    (lambda (lsystem order)
        "TODO"))


; The ``Turtle L-system'' corresponding the Sierpinsky triangle
(define sierpinski-triangle "TODO")

; The ``Turtle L-system'' corresponding the Sierpinsky carpet
(define sierpinski-carpet "TODO")

; The ``Turtle L-system'' corresponding the dragon curve
(define dragon-curve "TODO")

; The ``Turtle L-system'' corresponding the tree growth
(define tree-growth "TODO")

; The ``Turtle L-system'' corresponding the plant growth
(define plant-growth "TODO")
