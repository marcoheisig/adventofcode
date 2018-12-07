(defpackage :adventofcode-2018-day-6
  (:use :cl))

(in-package :adventofcode-2018-day-6)

(defun read-points (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect
          (let ((pos (position #\, line)))
            (cons
             (parse-integer line :end pos)
             (parse-integer line :start (+ pos 2)))))))

(defun bounding-box (points)
  (loop for (x . y) of-type fixnum in points
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally
           (return
             (values min-x max-x min-y max-y))))

(defun map-points (function points)
  (multiple-value-bind (min-x max-x min-y max-y)
      (bounding-box points)
    (loop for x from min-x to max-x do
      (loop for y from min-y to max-y do
        (funcall function (cons x y))))))

(defun map-boundary-points (function points)
  (multiple-value-bind (min-x max-x min-y max-y)
      (bounding-box points)
    (loop for x from min-x to max-x do
      (funcall function (cons x min-y))
      (funcall function (cons x max-y)))
    (loop for y from (1+ min-y) to (1- max-y) do
      (funcall function (cons min-x y))
      (funcall function (cons max-x y)))))

(defun manhattan-distance (point-1 point-2)
  (destructuring-bind (x1 . y1) point-1
    (destructuring-bind (x2 . y2) point-2
      (+ (abs (- x1 x2))
         (abs (- y1 y2))))))

;;; Return the unique point in OTHER-POINTS that is closest to POINT.  If
;;; multiple points are closest, return NIL.
(defun closest-point (point other-points &key (metric #'manhattan-distance))
  (let ((min-distance nil)
        (closest-points nil))
    (loop for other-point in other-points
          for distance = (funcall metric point other-point) do
            (cond ((or (null min-distance)
                       (< distance min-distance))
                   (setf min-distance distance)
                   (setf closest-points (list other-point)))
                  ((and (= distance min-distance))
                   (pushnew other-point closest-points))))
    (if (= 1 (length closest-points))
        (values (first closest-points) min-distance)
        (values nil 0))))

(defun solve-problem-1 ()
  (let ((points (read-points "input"))
        (outer-points '())
        (table (make-hash-table :test #'equal)))
    (map-points
     (lambda (point)
       (let ((closest-point (closest-point point points)))
         (unless (null closest-point)
           (incf (gethash closest-point table 0)))))
     points)
    (map-boundary-points
     (lambda (point)
       (pushnew (closes-point point points) outer-points
                :test #'equal))
     points)
    (first
     (sort
      (mapcar
       (lambda (point)
         (gethash point table))
       (set-difference points outer-points))
      #'>))))

(defun total-distance (point other-points &key (metric #'manhattan-distance))
  (loop for other-point in other-points
        sum (funcall metric point other-point)))

(defun solve-problem-2 ()
  (let ((points (read-points "input"))
        (threshold 10000)
        (region-size 0))
    (map-points
     (lambda (point)
       (when (< (total-distance point points) threshold)
         (incf region-size)))
     points)
    region-size))
