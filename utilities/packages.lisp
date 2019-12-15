(cl:in-package #:cl-user)

(defpackage :adventofcode
  (:nicknames :aoc)
  (:use :cl)
  (:export
   #:ensure-adventofcode-directories

   #:grid
   #:gridp
   #:make-grid
   #:grid-ref
   #:copy-grid
   #:grid-bounding-box
   #:draw-grid
   #:map-sparse-grid
   #:map-dense-grid

   #:mover
   #:moverp
   #:direction
   #:x
   #:y
   #:move
   #:rotate-clockwise
   #:rotate-counterclockwise))

