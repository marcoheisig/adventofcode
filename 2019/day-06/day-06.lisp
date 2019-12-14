(defpackage :adventofcode-2019-day-6
  (:use :cl))

(in-package :adventofcode-2019-day-6)

(defstruct entity
  (name nil :type string)
  parent
  n-orbits
  distance-to-you
  distance-to-santa)

(defparameter *entities* (make-hash-table :test #'equal))

(defun entity (name)
  (or (gethash name *entities*)
      (setf (gethash name *entities*)
            (make-entity :name name))))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line do
      (let* ((mid (position #\) line))
             (child (entity (subseq line (1+ mid))))
             (parent (entity (subseq line 0 mid))))
        (setf (entity-parent child) parent)))))

(defun n-orbits (entity)
  (let ((cache (entity-n-orbits entity)))
    (cond ((integerp cache) cache)
          ((null (entity-parent entity)) 0)
          (t (setf cache (1+ (n-orbits (entity-parent entity))))))))

(defun solve-day-6-part-1 ()
  (let ((*entities* (make-hash-table :test #'equal)))
    (read-input "input")
    (loop for entity being the hash-values of *entities*
          sum (n-orbits entity))))

(defun set-distances (start writer)
  (loop for entity = start then (entity-parent entity)
        for distance from 0
        while entity do
          (funcall writer distance entity)))

(defun solve-day-6-part-2 ()
  (let ((*entities* (make-hash-table :test #'equal)))
    (read-input "input")
    (set-distances (entity "YOU") #'(setf entity-distance-to-you))
    (set-distances (entity "SAN") #'(setf entity-distance-to-santa))
    (loop for entity being the hash-values of *entities*
          when (and (entity-distance-to-you entity)
                    (entity-distance-to-santa entity))
          minimize (+ (entity-distance-to-you entity)
                      (entity-distance-to-santa entity)
                      -2))))
