#lang racket

(require racket/format) ; see ~r function
(require "drawing.scm")

(provide drawing->svg)

; if width and height are strictly positive integers and drawing is a ``drawing'' (see "drawing.scm")
; then (drawing->svg width height drawing) returns the SVG representation  as a string
; of the given drawing in a (width x height) viewport
(define drawing->svg
  (lambda (width height drawing)
    "TODO"))
