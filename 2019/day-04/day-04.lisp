(defpackage :adventofcode-2019-day-4
  (:use :cl))

(in-package :adventofcode-2019-day-4)

(defun digits (number)
  (labels ((frob (number digits)
             (if (zerop number)
                 digits
                 (multiple-value-bind (rest digit) (floor number 10)
                   (frob rest (cons digit digits))))))
    (frob number '())))

(defun groups (digits)
  (let ((groups '())
        (acc '()))
    (loop for digit in digits do
      (cond ((null acc)
             (push digit acc))
            ((= (first acc) digit)
             (push digit acc))
            (t
             (push acc groups)
             (setf acc (list digit)))))
    (cons acc groups)))

(defun solve-day-4-task-1 (low high)
  (loop for candidate from low to high
        for digits = (digits candidate)
        count (and (apply #'<= digits)
                   (not (apply #'< digits)))))

(defun solve-day-4-task-2 (low high)
  (loop for candidate from low to high
        for digits = (digits candidate)
        count (and (apply #'<= digits)
                   (find 2 (groups digits) :key #'length))))
