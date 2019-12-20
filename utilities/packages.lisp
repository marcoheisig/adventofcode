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
   #:read-grid
   #:copy-grid
   #:filter-grid
   #:grid-bounding-box
   #:grid-center
   #:print-grid
   #:map-sparse-grid
   #:map-dense-grid

   #:position-mixin
   #:x
   #:y

   #:mover
   #:moverp
   #:direction
   #:move
   #:rotate-clockwise
   #:rotate-counterclockwise))

