(defpackage :adventofcode-2018-day-11
  (:use :cl :petalisp))

;;; For this task, I decided to use Petalisp, the experimental parallel
;;; programming language that I'm working on.

(in-package :adventofcode-2018-day-11)

(defparameter *grid-serial-number* 9995)

(defun nth-digit (n integer &optional (base 10))
  (mod (floor integer (expt base n)) base))

(defun power-level (x y)
  (let ((rack-id (+ x 10)))
    (- (nth-digit 2 (* rack-id (+ (* rack-id y) *grid-serial-number*)))
       5)))

(defun make-power-grid (h w)
  (let ((shape (~ 1 h ~ 1 w)))
    (α #'power-level (indices shape 0) (indices shape 1))))

(defun hstack (grid n)
  (trivia:ematch grid
    ((strided-array (~ 1 h ~ 1 w))
     (apply #'fuse
            (mapcar
             (lambda (x)
               (reshape grid
                        (τ (i j) (i (- j x)))
                        (~ 1 h ~ 1 (- w (1- n)))
                        (make-transformation
                         :input-rank 2
                         :output-mask (vector nil 0 1)
                         :offsets (vector x 0 0))))
             (alexandria:iota n))))))

(defun vstack (grid n)
  (trivia:ematch grid
    ((strided-array (~ 1 h ~ 1 w))
     (apply #'fuse
            (mapcar
             (lambda (x)
               (reshape grid
                        (τ (i j) ((- i x) j))
                        (~ 1 (- h (1- n)) ~ 1 w)
                        (make-transformation
                         :input-rank 2
                         :output-mask (vector nil 0 1)
                         :offsets (vector x 0 0))))
             (alexandria:iota n))))))

(defun square-sum-grid (grid n)
  (β #'+ (vstack (β #'+ (hstack grid n)) n)))

(defun amax-aux (v1 x1 y1 v2 x2 y2)
  (if (> v1 v2)
      (values v1 x1 y1)
      (values v2 x2 y2)))

(defun amax (grid)
  (multiple-value-bind (v x y)
      (β #'amax-aux grid (indices grid 0) (indices grid 1))
    (β #'amax-aux v x y)))

(defun solve-exercise-1 ()
  (multiple-value-call #'compute
    (amax (square-sum-grid (make-power-grid 300 300) 3))))

(defun solve-exercise-2 ()
  (sort
   (mapcar
    (lambda (n)
      (print
       (list* n (multiple-value-list
                 (multiple-value-call #'compute
                   (amax (square-sum-grid (make-power-grid 300 300) n)))))))
    (alexandria:iota 300 :start 1))
   #'> :key #'second))
