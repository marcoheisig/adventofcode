(defpackage :adventofcode-2018-day-13
  (:use :cl))

(in-package :adventofcode-2018-day-13)

(defvar *tracks*)

(defvar *carts*)

(defvar *tmp*) ; Where I store the moved carts during the tick.

(defmacro track (y x)
  `(aref *tracks* ,y ,x))

(defmacro cart (y x)
  `(aref *carts* ,y ,x))

(defmacro tmp (y x)
  `(aref *tmp* ,y ,x))

(defclass cart ()
  ((%move-alists
    :accessor move-alists
    :initform '#1=(((#\> . #\^) (#\^ . #\<) (#\< . #\v) (#\v . #\>))
                   ((#\> . #\>) (#\^ . #\^) (#\< . #\<) (#\v . #\v))
                   ((#\> . #\v) (#\^ . #\>) (#\< . #\^) (#\v . #\<))
                   .
                   #1#))
   (%state :initarg :state :accessor state
           :type (member #\< #\^ #\> #\v))))

(defun make-cart (initial-state)
  (make-instance 'cart :state initial-state))

(defun print-tracks (&optional (stream *standard-output*))
  (loop for y below (array-dimension *tracks* 0) do
    (loop for x below (array-dimension *tracks* 1) do
      (if (null (cart y x))
          (write-char (track y x) stream)
          (write-char (state (cart y x)) stream)))
    (terpri stream)))

(defun map-grid-coordinates (function grid)
  (loop for y below (array-dimension grid 0) do
    (loop for x below (array-dimension grid 1) do
      (funcall function y x))))

(defun map-carts (function)
  (map-grid-coordinates
   (lambda (y x)
     (let ((cart (cart y x)))
       (unless (null cart)
         (funcall function cart y x))))
   *carts*))

(defun all-carts ()
  (let ((carts '()))
    (map-carts
     (lambda (cart y x)
       (declare (ignore y x))
       (push cart carts)))
    (nreverse carts)))

(defun move-cart (cart y x)
  (let ((state (state cart)))
       (multiple-value-bind (new-y new-x)
           (ecase state
             (#\> (values y (1+ x)))
             (#\< (values y (1- x)))
             (#\v (values (1+ y) x))
             (#\^ (values (1- y) x)))
         (cond
           ((and (null (cart new-y new-x))
                 (null (tmp new-y new-x)))
            (setf (state cart)
                  (ecase (track new-y new-x)
                    (#\| (ecase state (#\^ #\^) (#\v #\v)))
                    (#\- (ecase state (#\> #\>) (#\< #\<)))
                    (#\\ (ecase state (#\^ #\<) (#\< #\^) (#\v #\>) (#\> #\v)))
                    (#\/ (ecase state (#\^ #\>) (#\< #\v) (#\v #\<) (#\> #\^)))
                    (#\+ (cdr (assoc state (pop (move-alists cart))
                                     :test #'char=)))))
            (rotatef (cart y x) (tmp new-y new-x)))
           (t
            (format t "Collision at position ~D,~D.~%" new-x new-y)
            (setf (cart y x) nil)
            (setf (cart new-y new-x) nil)
            (setf (tmp new-y new-x) nil))))))

(defun tick ()
  (map-carts #'move-cart)
  (rotatef *carts* *tmp*))

(defun read-tracks-and-carts (file)
  (with-open-file (stream file :direction :input)
    (let* ((list-of-lists
             (loop for line = (read-line stream nil nil)
                   while line
                   collect (coerce line 'list)))
           (dimensions (list (length list-of-lists)
                             (length (first list-of-lists))))
           (tracks (make-array dimensions :initial-contents list-of-lists))
           (carts (make-array dimensions :initial-element nil)))
      (loop for index below (array-total-size tracks) do
        (let ((track (row-major-aref tracks index)))
          (case track
            ((#\< #\>)
             (setf (row-major-aref tracks index) #\-)
             (setf (row-major-aref carts index)
                             (make-cart track)))
            ((#\^ #\v)
             (setf (row-major-aref tracks index) #\|)
             (setf (row-major-aref carts index)
                   (make-cart track))))))
      (values tracks carts))))

(defun solve-exercise-1 ()
  (multiple-value-bind (*tracks* *carts*)
      (read-tracks-and-carts "input")
    (let ((*tmp* (make-array (array-dimensions *carts*) :initial-element nil))
          (n-carts (length (all-carts))))
      (loop while (= (length (all-carts)) n-carts) do (tick)))))

(defun solve-exercise-2 ()
  (multiple-value-bind (*tracks* *carts*)
      (read-tracks-and-carts "input")
    (let ((*tmp* (make-array (array-dimensions *carts*) :initial-element nil)))
      (loop until (= (length (all-carts)) 1) do (tick))
      (map-carts
       (lambda (cart y x)
         (declare (ignore cart))
         (format t "The last cart is at position: ~D,~D" x y))))))
