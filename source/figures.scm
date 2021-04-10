#lang racket

(require "lsystem.scm")
(require "svg.scm")
(require "turtle.scm")
(require "app.scm")

(generate-and-draw* sierpinski-triangle 9 500 500 "../figures/sierpinski-triangle.svg")
(generate-and-draw* sierpinski-carpet 5 500 500 "../figures/sierpinski-carpet.svg")
(generate-and-draw* dragon-curve 16 500 500 "../figures/dragon-curve.svg")
(generate-and-draw* tree-growth 9 500 500 "../figures/tree.svg")

(generate-and-draw* gosper-curve 6 500 500 "../figures/gosper-curve.svg")
(generate-and-draw* koch-snowflake 8 500 500 "../figures/koch-snowflake.svg")