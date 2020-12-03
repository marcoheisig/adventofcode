(defpackage :adventofcode-2020-day-2
  (:use :cl))

(in-package :adventofcode-2020-day-2)

(defun map-input (function)
  (with-open-file (stream "input" :direction :input)
    (loop for line = (read-line stream nil nil) while line do
      (multiple-value-bind (start end starts ends)
          (cl-ppcre:scan "([0-9]+)-([0-9]+) ([a-z]): (.+)" line)
        (assert (and start end))
        (funcall function
                 (parse-integer line :start (svref starts 0) :end (svref ends 0))
                 (parse-integer line :start (svref starts 1) :end (svref ends 1))
                 (schar line (svref starts 2))
                 (subseq line (svref starts 3) (svref ends 3)))))))

(defun solve-day-2-part-1 ()
  (let ((valid 0))
    (map-input
     (lambda (lo hi char string)
       (when (<= lo (count char string) hi)
         (incf valid))))
    valid))

(defun solve-day-2-part-2 ()
  (let ((valid 0))
    (map-input
     (lambda (lo hi char string)
       (when (alexandria:xor
              (char= char (schar string (1- lo)))
              (char= char (schar string (1- hi))))
         (incf valid))))
    valid))
