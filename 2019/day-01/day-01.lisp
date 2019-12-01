(defpackage :adventofcode-2019-day-1
  (:use :cl))

(in-package :adventofcode-2019-day-1)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (and line (find-if #'digit-char-p line))
          collect
          (parse-integer line))))

(defun required-fuel (mass)
  (- (floor mass 3) 2))

(defun solve-day-1-part-1 ()
  (reduce #'+ (read-input "input") :key #'required-fuel))

(defun actually-required-fuel (mass)
  (let ((fuel (required-fuel mass)))
    (if (<= fuel 0)
        0
        (+ fuel (actually-required-fuel fuel)))))

(defun solve-day-1-part-2 ()
  (reduce #'+ (read-input "input") :key #'actually-required-fuel))
