(defpackage :adventofcode-2018-day-10
  (:use :cl))

(in-package :adventofcode-2018-day-10)

(defgeneric pos (point time))

(defclass point ()
  ((%pos-x :initarg :pos-x :reader pos-x)
   (%pos-y :initarg :pos-y :reader pos-y)
   (%vel-x :initarg :vel-x :reader vel-x)
   (%vel-y :initarg :vel-y :reader vel-y)))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (format stream "<~6D,~6D> <~2D,~2D>"
            (pos-x point)
            (pos-y point)
            (vel-x point)
            (vel-y point))))

(defun read-points-from-file (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-point line))))

(defun parse-point (string)
  (multiple-value-bind (match substrings)
        (cl-ppcre:scan-to-strings
         ".*<( *[+-]?[0-9]+), *([+-]?[0-9]+) *>.*<( *[+-]?[0-9]+), *([+-]?[0-9]+) *>.*"
         string)
    (assert match)
    (make-instance 'point
      :pos-x (parse-integer (aref substrings 0))
      :pos-y (parse-integer (aref substrings 1))
      :vel-x (parse-integer (aref substrings 2))
      :vel-y (parse-integer (aref substrings 3)))))

(defmethod pos ((point point) (time integer))
  (values (+ (pos-x point) (* (vel-x point) time))
          (+ (pos-y point) (* (vel-y point) time))))

(define-modify-macro maxf (form &rest other-values) max)
(define-modify-macro minf (form &rest other-values) min)

(defun bounding-box (points time)
  (let ((min-x most-positive-fixnum)
        (max-x most-negative-fixnum)
        (min-y most-positive-fixnum)
        (max-y most-negative-fixnum))
    (loop for point in points do
      (multiple-value-bind (x y) (pos point time)
        (minf min-x x)
        (maxf max-x x)
        (minf min-y y)
        (maxf max-y y)))
    (values min-x max-x min-y max-y)))

(defun bounding-box-size (points time)
  (multiple-value-bind (min-x max-x min-y max-y)
      (bounding-box points time)
    (* (abs (- max-x min-x))
       (abs (- max-y min-y)))))

(defun print-points (points time &optional (stream *standard-output*))
  (multiple-value-bind (min-x max-x min-y max-y)
      (bounding-box points time)
    (let ((w (1+ (abs (- max-x min-x))))
          (h (1+ (abs (- max-y min-y)))))
      (let ((array (make-array (list w h) :initial-element 0)))
        (mapc
         (lambda (point)
           (multiple-value-bind (x y) (pos point time)
             (incf (aref array (- x min-x) (- y min-y)))))
         points)
        (loop for i below h do
          (loop for j below w do
            (if (zerop (aref array j i))
                (write-char #\. stream)
                (write-char #\# stream)))
          (terpri stream))))))

(defun solve-exercise-1 ()
  (let* ((points (read-points-from-file "input"))
         (min-size (bounding-box-size points 0)))
    (loop for time from 1
          for new-size = (bounding-box-size points time) do
            (if (< new-size min-size)
                (setf min-size new-size)
                (progn
                  (format t "Min bounding box at time step ~D.~%" (1- time))
                  (print-points points (1- time))
                  (return))))))
