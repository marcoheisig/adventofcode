(defpackage :adventofcode-2018-day-17
  (:use :cl))

(in-package :adventofcode-2018-day-17)

(define-modify-macro minf (number &rest more-numbers) min)
(define-modify-macro maxf (number &rest more-numbers) max)

(defclass ground ()
  ((%tiles :initform (make-hash-table :test #'equal) :reader tiles)
   (%xmin :initform most-positive-fixnum :accessor xmin)
   (%xmax :initform most-negative-fixnum :accessor xmax)
   (%ymin :initform most-positive-fixnum :accessor ymin)
   (%ymax :initform most-negative-fixnum :accessor ymax)))

(defvar *ground*)

(defun tile (x y)
  (gethash (list x y) (tiles *ground*) #\.))

(defun (setf tile) (value x y)
  (minf (xmin *ground*) (1- x))
  (maxf (xmax *ground*) (1+ x))
  (minf (ymin *ground*) y)
  (maxf (ymax *ground*) y)
  (setf (gethash (list x y) (tiles *ground*)) value))

(defun register-clay (string)
  (trivia:ematch string
    ((trivia.ppcre:ppcre "x=([0-9]+), *y=([0-9]+)\.\.([0-9]+)" x ymin ymax)
     (loop for y from (parse-integer ymin) to (parse-integer ymax) do
       (setf (tile (parse-integer x) y) #\#)))
    ((trivia.ppcre:ppcre "y=([0-9]+), *x=([0-9]+)\.\.([0-9]+)" y xmin xmax)
     (loop for x from (parse-integer xmin) to (parse-integer xmax) do
       (setf (tile x (parse-integer y)) #\#)))))

(defun read-clay-specs (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line do (register-clay line))))

(defun scan (initial-x y step)
  (loop for x from initial-x by step do
    (let ((current (tile x y))
          (below (tile x (1- y))))
      (when (char= current #\#)
        (return-from scan (values x y #\#)))
      (unless (member below '(#\# #\~))
        (return-from scan (values x y current))))))

(defun spawn-water (spawn-x spawn-y)
  (let ((vertical-spawns '())
        (bottom (ymin *ground*))
        (y spawn-y))
    ;; Trickle down.
    (loop until (or (member (tile spawn-x y) '(#\# #\~)) (= y bottom)) do
      (setf (tile spawn-x y) #\|)
      (decf y))
    ;; Now move back upwards and scan to the left and to the right.
    (loop until (= y spawn-y) do
      (multiple-value-bind (left-x left-y left-tile) (scan spawn-x y -1)
        (multiple-value-bind (left-x left-y right-tile) (scan spawn-x y 1)
          (when (char= left-tile #\.)
            (push (list left-x left-y) vertical-spawns))
          (when (char= right-tile #\.)
            (push (list right-x right-y) vertical-spawns))
          (unless (and (char= left-tile #\#)
                       (char= right-tile #\#))
            (loop-finish))
          ;; Fill the current layer.
          (loop for x from (1+ left-x) to (1- right-x) do
            (setf (tile x y) #\~)))))))

(defun solve-exercise-1 ()
  (let ((*ground* (make-instance 'ground)))
    (register-clay "test-input")
    (spawn-water 500 0)
    (loop for tile being the hash-values of *ground*
          count (or (char= tile #\|)
                    (char= tile #\~)))))
