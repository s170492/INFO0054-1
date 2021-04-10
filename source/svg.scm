#lang racket

(require racket/format) ; see ~r function
(require "drawing.scm")

(provide drawing->svg)

; if width and height are strictly positive integers and drawing is a ``drawing'' (see "drawing.scm")
; then (drawing->svg width height drawing) returns the SVG representation  as a string
; of the given drawing in a (width x height) viewport
(define drawing->svg
  (lambda (width height drawing)
    (string-join (list (format "<svg width=\"~a\" height=\"~a\" viewbox=\"~a ~a ~a ~a\" />" width height
                               (drawing.minx drawing) (drawing.miny drawing)
                               (- (drawing.maxx drawing) (drawing.minx drawing))
                               (- (drawing.maxy drawing) (drawing.miny drawing)))
                       (string-join (map polyline->svg (drawing.polylines drawing))
                                    "\n\t" #:before-first "\t")
                       "</svg>") "\n")))

; if polyline is a polyline then (polyline->svg) returns the SVG representation
; as a string of the given polyline
(define polyline->svg
  (lambda (polyline)
    (string-append "<polyline points=\""
                   (string-join
                    (map (lambda (p) (string-append (~a (car p)) "," (~a (cdr p)))) polyline)
                    " ")
                   "\"/>")))