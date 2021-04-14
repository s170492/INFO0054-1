#lang racket

(require racket/format) ; see ~r function
(require "drawing.scm")

(provide drawing->svg)

; if width and height are strictly positive integers and drawing is a ``drawing'' (see "drawing.scm")
; then (drawing->svg width height drawing) returns the SVG representation  as a string
; of the given drawing in a (width x height) viewport
(define drawing->svg
  (lambda (width height drawing)
    (let* ((minx (drawing.minx drawing))
          (miny (drawing.miny drawing))
          (rangex (- (drawing.maxx drawing) minx))
          (rangey (- (drawing.maxy drawing) miny)))
      (string-join (list "<svg xmlns=\"http://www.w3.org/2000/svg\""
                         "     xmlns:xlink=\"http://www.w3.org/1999/xlink\""
                         (format "     width=\"~a\" height=\"~a\" viewBox=\"~a ~a ~a ~a\">"
                                 width height (~r minx #:precision 5) (~r miny #:precision 5)
                                 (~r rangex #:precision 5) (~r rangey #:precision 5))
                         (format "<rect x=\"~a\" y=\"~a\" width=\"100%\" height=\"100%\" fill=\"white\"/>"
                                 (~r minx #:precision 5) (~r miny #:precision 5))
                         (string-join (map (lambda (polyline)
                                             (polyline->svg polyline
                                                            (/ (sqrt (max rangex rangey)) 100)))
                                           (drawing.polylines drawing))
                                      "\n")
                         "</svg>")
                   "\n"))))

; if polyline is a polyline and width is a positive number then (polyline->svg)
; returns the SVG representation as a string of the given polyline with the given width
(define polyline->svg
  (lambda (polyline width)
    (string-join
     (map (lambda (p) (string-append (~r (car p) #:precision 5) ", " (~r (cdr p) #:precision 5)))
          polyline)
     "\n"
     #:before-first "<polyline points=\"\n"
     #:after-last (format "\n\" fill=\"none\" stroke=\"black\" stroke-width=\"~a\"/>" width))))