#lang racket


(require "drawing.scm")
(provide cons-init-turtle-store)


; A ``turtle f-store'' is a function encapsulating in its closure:
;;        a ``drawing'' (see "drawing.scm")
;;        a ``rotation angle'' which is the angle (expressed in
;;            radians) of a unit change of direction
; It takes as input t-symb,  a ``turtle symbol'', and returns:
;;       the encapsulated ``drawing'', if t-symb is the empty string ""
;;       a ``turtle f-store'' encapsulating the new ``drawing'' corresponding to
;;            changing the current ``drawing'' according to the known
;;            ``turtle-symbol'' t-symb
;; or raises (error "Unknown turtle symbol") if t-symb is unknown

; The ``turtle symbol''s are the following string:
;;  T,    which means trace forward one unit (on the current polyline)
;;  T[x], which means trace forward x units (on the current polyline)
;;  F,    which means move forward one unit and start a new polyline there
;;  F[x], which means move forward x units and start a new polyline there
;;  <,    which means save the current position and direction
;;  >,    which means restore the last position and direction saved if there were any, otherwise, does nothing
;;  +,    which means make a unit change of  direction
;;  +[x], which means make a change of direction of x units
;;  -,    which means make a change of direction of -1 unit
;;  -[x], which means make a change of direction of -x units

; A ``turtle string'' (or ``L-system string'') is a list of ``turtle symbol''s.
; i.e. a list of string


; if angle and direction are trigonometric angles expressed in degrees,
; (cons-init-turtle-store angle direction) returns a ``turtle f-store''
; encapsulating the empty ``drawing'', with the given initial direction (in radians)
;               a ``rotation angle'' of angle radians
(define cons-init-turtle-store
  (lambda (angle direction)
    (cons-turtle-store (cons-empty-drawing direction) angle)))
                       
(define cons-turtle-store
  (lambda (drawing angle)
    (lambda (t-symb)
      (cond ((equal? t-symb "") drawing)  ; t-symb is the empty string
            
            ((equal? t-symb "T")  ; t-symb is T   works fine
             (let* ((position (drawing.position drawing))
                    (direction (drawing.direction drawing))
                    (new-position (cons-point (+ (point.x position) (cos direction))
                                              (+ (point.y position) (sin direction)))))
               (cons-turtle-store
                (cons-drawing direction
                              (drawing.saved-positions drawing)
                              (drawing.update-polyline drawing new-position)
                              (drawing.update-bounding-box drawing new-position))
                angle)))
            
            ((regexp-match? #px"T[[][\\d]+[]]" t-symb)  ; t-symb is T[x]   works fine
             (let* ((x (string->number (regexp-replace* #rx"T|[[]|[]]" t-symb "")))
                    (position (drawing.position drawing))
                    (direction (drawing.direction drawing))
                    (new-position (cons-point (+ (point.x position) (* x (cos direction)))
                                              (+ (point.y position) (* x (sin direction))))))
               (cons-turtle-store
                (cons-drawing direction
                              (drawing.saved-positions drawing)
                              (drawing.update-polyline drawing new-position)
                              (drawing.update-bounding-box drawing new-position))
                angle)))
             
            
            ((equal? t-symb "F")  ; t-symb is F
             (let* ((position (drawing.position drawing))
                    (direction (drawing.direction drawing))
                    (new-position (cons-point (+ (point.x position) (cos direction))
                                              (+ (point.y position) (sin direction)))))
               (cons-turtle-store
                (cons-drawing direction
                              (drawing.saved-positions drawing)
                              (drawing.peek-new-polyline drawing new-position)
                              (drawing.update-bounding-box drawing new-position))
                angle)))
            
            ((regexp-match? #px"F[[][\\d]+[]]" t-symb)  ; t-symb is F[x]
             (let* ((x (string->number (regexp-replace* #rx"F|[[]|[]]" t-symb "")))
                    (position (drawing.position drawing))
                    (direction (drawing.direction drawing))
                    (new-position (cons-point (+ (point.x position) (* x (cos direction)))
                                              (+ (point.y position) (* x (sin direction))))))
               (cons-turtle-store
                (cons-drawing direction
                              (drawing.saved-positions drawing)
                              (drawing.peek-new-polyline drawing new-position)
                              (drawing.update-bounding-box drawing new-position))
                angle)))
            
            
            ((equal? t-symb "<")  ; t-symb is <
             (let ((direction (drawing.direction drawing)))
               (cons-turtle-store
                (cons-drawing direction
                              (drawing.push-d&p drawing direction (drawing.position drawing))
                              (drawing.polylines drawing)
                              (drawing.bounding-box drawing))
                angle)))

            ((equal? t-symb ">")  ; t-symb is >
             (let* ((stack (drawing.pop-saved-positions drawing)) (d&p (car stack)) (tail (cdr stack)))
                   (if (null? stack) (cons-turtle-store drawing angle)
                       (cons-turtle-store
                        (cons-drawing (d&p.direction d&p)
                                      tail
                                      (drawing.peek-new-polyline drawing (d&p.point d&p))
                                      (drawing.bounding-box drawing))
                        angle))))
            
            ((equal? t-symb "+")  ; t-symb is +
             (cons-turtle-store
              (cons-drawing (+ (drawing.direction drawing) angle)
                            (drawing.saved-positions drawing)
                            (drawing.polylines drawing)
                            (drawing.bounding-box drawing))
              angle))

            ((equal? t-symb "+[x]") "TO DO")  ; t-symb is +[x]

            ((equal? t-symb "-")  ; t-symb is -
             (cons-turtle-store
              (cons-drawing (- (drawing.direction drawing) angle)
                            (drawing.saved-positions drawing)
                            (drawing.polylines drawing)
                            (drawing.bounding-box drawing))
              angle))


            ((equal? t-symb "-[x]") "TO DO")  ; t-symb is -[x]

            (else (error "Unknown turtle symbol"))))))