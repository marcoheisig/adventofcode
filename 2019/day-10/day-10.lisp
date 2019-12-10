(defpackage :adventofcode-2019-day-10
  (:use :cl))

(in-package :adventofcode-2019-day-10)

(defun read-asteroid-map (filename)
  (with-open-file (stream filename :direction :input)
    (let ((lines (loop for line = (read-line stream nil nil)
                       while line
                       collect line)))
      (make-array (list (length lines) (length (first lines)))
                  :initial-contents lines))))

(defun angle (x1 y1 x2 y2)
  (phase (complex (- x2 x1) (- y2 y1))))

(defun map-asteroids (function asteroid-map)
  (loop for y below (array-dimension asteroid-map 0) do
    (loop for x below (array-dimension asteroid-map 1) do
      (when (char= (aref asteroid-map y x) #\#)
        (funcall function x y)))))

(defun best-location (asteroid-map)
  (let ((best-x nil) (best-y nil) (best-n 0))
    (map-asteroids
     (lambda (x1 y1)
       (let ((angles '()))
         (map-asteroids
          (lambda (x2 y2)
            (unless (and (= x1 x2) (= y1 y2))
              (push (angle x1 y1 x2 y2) angles)))
          asteroid-map)
         (let ((n (length (remove-duplicates angles))))
           (when (> n best-n)
             (setf best-x x1 best-y y1 best-n n)))))
     asteroid-map)
    (values best-x best-y best-n)))

(defun solve-day-10-part-1 ()
  (best-location (read-asteroid-map "input")))

(defun map-laser-debris (function asteroid-map x1 y1)
  (let ((targets '()))
    (map-asteroids
     (lambda (x2 y2)
       (unless (and (= x1 x2) (= y1 y2))
         (push (list (mod (+ (angle x1 y1 x2 y2) (* pi 1/2) +1.0e-7)
                          (* 2 pi))
                     x2 y2)
               targets)))
     asteroid-map)
    (flet ((distance (entry)
             (destructuring-bind (angle x2 y2) entry
               (declare (ignore angle))
               (sqrt (+ (expt (- x2 x1) 2)
                        (expt (- y2 y1) 2))))))
      (let* ((candidates (remove-duplicates (sort targets #'> :key #'distance) :key #'first))
             (sorted (sort candidates #'< :key #'first)))
        (dolist (candidate sorted)
          (destructuring-bind (angle x2 y2) candidate
            (declare (ignore angle))
            (setf (aref asteroid-map y2 x2) #\.)
            (funcall function x2 y2)))))))

(defun solve-day-10-part-2 ()
  (let ((asteroid-map (read-asteroid-map "input")))
    (multiple-value-bind (x y) (best-location asteroid-map)
      (format t "Best: (~D, ~D)" x y)
      (setf (aref asteroid-map y x) #\X)
      (let ((n 0))
        (loop repeat 10 do
          (map-laser-debris
           (lambda (x y)
             (print (list (incf n) x y)))
           asteroid-map x y))))))
