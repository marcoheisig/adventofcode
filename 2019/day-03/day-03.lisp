(defpackage :adventofcode-2019-day-3
  (:use :cl))

(in-package :adventofcode-2019-day-3)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect
          (mapcar #'parse-token (split-sequence:split-sequence #\, line)))))

(defun parse-token (token)
  (let ((distance (parse-integer token :start 1)))
    (ecase (elt token 0)
      (#\R (cons '( 1 .  0) distance))
      (#\L (cons '(-1 .  0) distance))
      (#\U (cons '( 0 .  1) distance))
      (#\D (cons '( 0 . -1) distance)))))

(defstruct grid (table (make-hash-table :test #'equal)))

(defun add-wire (grid position wire-id steps)
  (pushnew
   (cons wire-id steps)
   (gethash position (grid-table grid) nil)
   :key #'car))

(defun add-path (grid position path wire-id)
  (let ((steps 0))
    (destructuring-bind (x . y) position
      (loop for ((dx . dy) . distance) in path do
        (loop repeat distance do
          (setf x (+ x dx) y (+ y dy))
          (add-wire grid (cons x y) wire-id (incf steps)))))))

(defun read-grid (filename)
  (let ((paths (read-input filename))
        (grid (make-grid))
        (origin (cons 0 0)))
    (loop for path in paths
          for wire-id from 0
          do (add-path grid origin path wire-id))
    grid))

(defun map-grid-intersections (function grid)
  (maphash
   (lambda (point data)
     (when (> (length data) 1)
       (funcall function point data)))
   (grid-table grid)))

(defun solve-day-1-part-1 ()
  (let ((grid (read-grid "input")))
    (let ((min-distance nil))
      (map-grid-intersections
       (lambda (point data)
         (declare (ignore data))
         (let ((distance (+ (abs (car point)) (abs (cdr point)))))
           (when (plusp distance)
             (when (or (not min-distance) (< distance min-distance))
               (setf min-distance distance)))))
       grid)
      min-distance)))

(defun solve-day-1-part-2 ()
  (let ((grid (read-grid "input")))
    (let ((min-steps nil))
      (map-grid-intersections
       (lambda (point data)
         (declare (ignore point))
         (let ((steps (reduce #'+ data :key #'cdr)))
           (when (plusp steps)
             (when (or (not min-steps)
                       (< steps min-steps))
               (setf min-steps steps)))))
       grid)
      min-steps)))
