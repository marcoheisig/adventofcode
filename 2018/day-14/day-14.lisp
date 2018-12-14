(defpackage :adventofcode-2018-day-14
  (:use :cl))

(in-package :adventofcode-2018-day-14)

;;; *sigh* yet another doubly-linked circular list...

(defgeneric insert-before (recipe new))

(defgeneric map-recipes (function recipe))

(defgeneric all-scores (recipe))

(defgeneric n-recipes (recipe))

(defgeneric skip-recipes (recipe n))

(defclass recipe ()
  ((%score :initarg :score :reader score)
   (%predecessor :accessor predecessor)
   (%successor :accessor successor)))

(defun make-recipe (score)
  (let ((recipe (make-instance 'recipe :score score)))
    (setf (predecessor recipe) recipe)
    (setf (successor recipe) recipe)))

(defun make-recipe-chain (score &rest more-scores)
  (let ((initial-recipe (make-recipe score)))
    (loop for score in more-scores do
      (insert-before initial-recipe score))
    initial-recipe))

(defmethod insert-before ((current recipe) (new recipe))
  (flet ((insert-between (before new after)
           (psetf (successor before) new
                  (predecessor after) new
                  (successor new) after
                  (predecessor new) before)))
    (insert-between (predecessor current) new current))
  new)

(defmethod insert-before ((current recipe) (new integer))
  (insert-before current (make-recipe new)))

(defmethod map-recipes ((function function) (recipe recipe))
  (funcall function recipe)
  (loop for current = (successor recipe) then (successor current)
        until (eq current recipe)
        do (funcall function current)))

(defmethod all-scores ((recipe recipe))
  (let ((scores '()))
    (map-recipes
     (lambda (recipe)
       (push (score recipe) scores))
     recipe)
    (nreverse scores)))

(defmethod n-recipes ((recipe recipe))
  (let ((n 0))
    (map-recipes
     (lambda (recipe)
       (declare (ignore recipe))
       (incf n))
     recipe)
    n))

(defmethod skip-recipes ((recipe recipe) (n integer))
  (if (plusp n)
      (loop repeat (+ n) do (setf recipe (successor recipe)))
      (loop repeat (- n) do (setf recipe (predecessor recipe))))
  recipe)

(defmethod print-object ((recipe recipe) stream)
  (print-unreadable-object (recipe stream :type t)
    (write (all-scores recipe) :stream stream)))

(defun digits (integer &optional (base 10))
  (nreverse
   (loop for n = (abs integer) then q
         for (q r) = (multiple-value-list (floor n base))
         collect r
         until (zerop q))))

(defun experiment (initial-recipe stopping-criterion)
  (let ((r1 initial-recipe)
        (r2 (successor initial-recipe))
        (n-recipes (n-recipes initial-recipe)))
    (loop
      (let ((s1 (score r1))
            (s2 (score r2)))
        (loop for score in (digits (+ s1 s2)) do
          (when (funcall stopping-criterion n-recipes)
            (return-from experiment initial-recipe))
          (insert-before initial-recipe score)
          (incf n-recipes))
        (setf r1 (skip-recipes r1 (1+ s1)))
        (setf r2 (skip-recipes r2 (1+ s2)))))))

(defun solve-exercise-1 ()
  (last
   (all-scores
    (experiment
     (make-recipe-chain 3 7)
     (lambda (n-recipes)
       (>= n-recipes (+ 157901 10)))))
   10))

(defun solve-exercise-2 ()
  (let ((initial-recipe (make-recipe-chain 3 7))
        (pattern (reverse (digits 157901))))
    (experiment
     initial-recipe
     (lambda (n-recipes)
       (declare (ignore n-recipes))
       (loop for digit in pattern
             for recipe = (predecessor initial-recipe)
               then (predecessor recipe)
             always (= digit (score recipe)))))
    (format t "Pattern found after ~D recipes~%"
            (- (n-recipes initial-recipe)
               (length pattern)))))
