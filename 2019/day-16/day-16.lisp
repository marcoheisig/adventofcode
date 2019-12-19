(defpackage :adventofcode-2019-day-16
  (:use :cl))

(in-package :adventofcode-2019-day-16)

(defun make-circular (list)
  (let ((copy (copy-list list)))
    (setf (cdr (last copy)) copy)))

(defun compute-digit (position pattern numbers)
  (let ((circular-pattern (make-circular pattern))
        (sum 0))
    (flet ((next-number ()
             (if (null numbers)
                 (return-from compute-digit (mod (abs sum) 10))
                 (the (integer 0 9) (pop numbers)))))
      (loop for repetitions = position then (1+ position) do
        (let ((factor (pop circular-pattern)))
          (ecase factor
            (0 (loop repeat repetitions do (next-number)))
            (1 (loop repeat repetitions do (incf sum (next-number))))
            (-1 (loop repeat repetitions do (decf sum (next-number))))))))))

(defun fft (numbers)
  (loop for position below (length numbers)
        collect (compute-digit position '(0 1 0 -1) numbers)))

(defun solve-day-16-part-1 ()
  (let ((numbers (with-open-file (stream "input" :direction :input)
                   (loop for char = (read-char stream #\space nil)
                         while (digit-char-p char)
                         collect (parse-integer (string char))))))
    (loop repeat 100 do (setf numbers (fft numbers)))
    (subseq numbers 0 8)))

;; Destructively update all the points after a certain lower-bound with the
;; values of the next iteration.
(defun weird-inplace-fft (numbers lower-bound)
  (declare (type (simple-array (unsigned-byte 8) (*)) numbers))
  (let ((n (array-total-size numbers))
        (sum 0))
    (assert (< n (* 2 lower-bound)))
    (loop for index from (1- n) downto lower-bound do
      (setf sum (mod (+ sum (aref numbers index)) 10))
      (setf (aref numbers index) sum))))

(defun solve-day-16-part-2 ()
  (let* ((input (with-open-file (stream "input" :direction :input)
                  (loop for char = (read-char stream #\space nil)
                        while (digit-char-p char)
                        collect (parse-integer (string char)))))
         (numbers (coerce (apply #'append
                                 (make-list 10000 :initial-element input))
                          '(simple-array (unsigned-byte 8) (*))))
         (offset (loop for pow = 1 then (* 10 pow)
                       for digit in (reverse (subseq input 0 7))
                       sum (* pow digit))))
    (loop for lower-bound from (- offset 99) to offset do
      (weird-inplace-fft numbers lower-bound))
    (subseq numbers offset (+ offset 8))))
