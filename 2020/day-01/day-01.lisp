(defpackage :adventofcode-2020-day-1
  (:use :cl))

(in-package :adventofcode-2020-day-1)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while (and line (find-if #'digit-char-p line))
          collect (parse-integer line))))

(defun solve-day-1-part-1 ()
  (alexandria:map-combinations
   (trivia:lambda-ematch
     ((list a b)
      (when (= (+ a b) 2020)
        (return-from solve-day-1-part-1 (* a b)))))
   (read-input "input")
   :length 2))

(defun solve-day-1-part-2 ()
  (alexandria:map-combinations
   (trivia:lambda-ematch
     ((list a b c)
      (when (= (+ a b c) 2020)
        (return-from solve-day-1-part-2 (* a b c)))))
   (read-input "input")
   :length 3))
