(defpackage :adventofcode-2018-day-8
  (:use :cl))

(in-package :adventofcode-2018-day-8)

(defclass node ()
  ((%children :initarg :children :reader children)
   (%metadata :initarg :metadata :reader metadata)))

(defun read-node (stream)
  (let ((n-children (read stream))
        (n-metadata (read stream)))
    (make-instance 'node
      :children (loop repeat n-children collect (read-node stream))
      :metadata (loop repeat n-metadata collect (read stream)))))

(defun read-tree-from-file (file)
  (with-open-file (stream file :direction :input)
    (read-node stream)))

(defun metadata-sum (node)
  (+ (reduce #'+ (metadata node))
     (reduce #'+ (children node) :key #'metadata-sum)))

(defun solve-exercise-1 ()
  (metadata-sum (read-tree-from-file "input")))

(defun node-value (node)
  (if (null (children node))
      (reduce #'+ (metadata node))
      (loop for index in (metadata node)
            sum
            (let ((child (nth (1- index) (children node))))
              (if (not child)
                  0
                  (node-value child))))))

(defun solve-exercise-2 ()
  (node-value (read-tree-from-file "input")))
