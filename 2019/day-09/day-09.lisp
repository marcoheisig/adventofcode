(defpackage :adventofcode-2019-day-9
  (:use :cl :intcode-computer))

(in-package :adventofcode-2019-day-9)

(defun solve-day-09-part-1 ()
  (run-intcode-computer
   (make-intcode-computer
    :input (constantly 1)
    :program (read-intcode-computer-program "input"))))

(defun solve-day-09-part-2 ()
  (run-intcode-computer
   (make-intcode-computer
    :input (constantly 2)
    :program (read-intcode-computer-program "input"))))
