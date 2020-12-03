(defpackage :adventofcode-2020-day-3
  (:use :cl))

(in-package :adventofcode-2020-day-3)

(defstruct grid
  (array nil :type (simple-array character (* *))))

(defun grid-ref (grid x y)
  (let ((array (grid-array grid)))
    (aref array
          (mod y (array-dimension array 0))
          (mod x (array-dimension array 1)))))

(defun read-grid (stream)
  (let ((lines (loop for line = (read-line stream nil nil)
                     while line
                     collect line)))
    (make-grid :array (make-array (list (length lines) (length (first lines)))
                                  :element-type 'character
                                  :initial-contents lines))))

(defun count-trees (grid dx dy)
  (loop for y from 0 by dy below (array-dimension (grid-array grid) 0)
        for x from 0 by dx
        count (char= (grid-ref grid x y) #\#)))

(defun solve-day-1-part-1 ()
  (let ((grid (with-open-file (stream "input" :direction :input)
                (read-grid stream))))
    (count-trees grid 3 1)))

(defun solve-day-1-part-2 ()
  (let ((grid (with-open-file (stream "input" :direction :input)
                (read-grid stream))))
    (* (count-trees grid 1 1)
       (count-trees grid 3 1)
       (count-trees grid 5 1)
       (count-trees grid 7 1)
       (count-trees grid 1 2))))
