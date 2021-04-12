#lang racket

(require "lsystem.scm")
(require "svg.scm")
(require "turtle.scm")

(provide generate-and-draw*)

(random-seed 0)

; if tlsyst is a ``turtle L-system'' (see "lsystem.scm"), order, width and height
; are naturals, (generate-and-draw tlsyst order width height) returns the SVG
; representation as a string of the order-th drawing of the given L-system in a
; (width x height) viewport
(define generate-and-draw
  (lambda (tlsyst order width height)
    (drawing->svg
     width height
     (draw-from-instructions (cons-init-turtle-store (tlsyst.angle tlsyst) 0)
                             (lsystem.generate-string (tlsyst.lsystem tlsyst) order)))))


; if turtle-f-store is a ``turtle f-store'' (see "turtle.scm") and turtle-instructions
; is a ``turtle string'' (see "turtle.scm"),
; (draw-from-instructions turtle-f-store turtle-instructions) is the drawing
; corresponding to envolving the drawing encapsulated in the turtle-f-store with
; the instrictions of turtle-instructions
(define draw-from-instructions
  (lambda (turtle-f-store turtle-instructions)
    (if (null? turtle-instructions)
        (turtle-f-store "")
        (draw-from-instructions (turtle-f-store (car turtle-instructions))
                                (cdr turtle-instructions)))))

; if tlsyst is a ``turtle L-system'', order, width and height are naturals and file is filepath
; (generate-and-draw* tlsyst order width height file) overwrites the given file
; with the SVG representation of the order-th drawing of the given L-system in a
; (width x height) viewport
(define generate-and-draw*
  (lambda (tlsyst order width height file)
    (with-output-to-file
        file
      (lambda () (display (generate-and-draw tlsyst order width height)))
      #:mode 'text
      #:exists 'replace)))
