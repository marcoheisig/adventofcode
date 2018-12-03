(defpackage :adventofcode-2018-day-3
  (:use :cl))

(in-package :adventofcode-2018-day-3)

(defclass claim ()
  ((%id :initarg :id :reader id)
   (%left-offset :initarg :left-offset :reader left-offset)
   (%top-offset :initarg :top-offset :reader top-offset)
   (%width :initarg :width :reader width)
   (%height :initarg :height :reader height)))

(defmethod print-object ((claim claim) stream)
  (print-unreadable-object (claim stream :type t)
    (format stream "#~D @ ~D,~D: ~Dx~D"
            (id claim)
            (left-offset claim)
            (top-offset claim)
            (width claim)
            (height claim))))

(defun read-claims (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-claim line))))

(defun parse-claim (string)
  (multiple-value-bind (match substrings)
      (cl-ppcre:scan-to-strings
       "#([0-9]+) *@ *([0-9]+) *, *([0-9]+) *: *([0-9]+) *x *([0-9]+) *"
       string)
    (assert match)
    (make-instance 'claim
      :id          (parse-integer (aref substrings 0))
      :left-offset (parse-integer (aref substrings 1))
      :top-offset  (parse-integer (aref substrings 2))
      :width       (parse-integer (aref substrings 3))
      :height      (parse-integer (aref substrings 4)))))

(defun make-fabric (width height)
  (make-array (list width height) :initial-element '()))

(defvar *fabric*)

(defun add-claim (claim)
  (with-accessors ((left-offset left-offset)
                   (top-offset top-offset)
                   (width width)
                   (height height)) claim
    (loop for h from top-offset below (+ top-offset height) do
      (loop for w from left-offset below (+ left-offset width) do
        (pushnew claim (aref *fabric* h w))))))

(defun solve-problem-1 ()
  (let ((claims (read-claims "input"))
        (*fabric* (make-fabric 1000 1000)))
    (mapc #'add-claim claims)
    (loop for index below (array-total-size *fabric*)
          count (<= 2 (length (row-major-aref *fabric* index))))))

(defun non-overlapping-claim-p (claim)
  (with-accessors ((left-offset left-offset)
                   (top-offset top-offset)
                   (width width)
                   (height height)) claim
    (block outer
      (loop for h from top-offset below (+ top-offset height) do
        (loop for w from left-offset below (+ left-offset width)
              unless (= 1 (length (aref *fabric* h w))) do
                (return-from outer nil)))
      t)))

(defun solve-problem-2 ()
  (let ((claims (read-claims "input"))
        (*fabric* (make-fabric 1000 1000)))
    (mapc #'add-claim claims)
    (find-if #'non-overlapping-claim-p claims)))
