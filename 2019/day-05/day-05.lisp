(defpackage :adventofcode-2019-day-5
  (:use :cl :intcode-computer))

(in-package :adventofcode-2019-day-5)

(defun solve-day-5-part-1 ()
  (run-intcode-computer
   (make-intcode-computer
    :program (read-intcode-computer-program "input")
    :input (constantly 1))))

(defun solve-day-5-part-2 ()
  (run-intcode-computer
   (make-intcode-computer
    :program (read-intcode-computer-program "input")
    :input (constantly 5))))
