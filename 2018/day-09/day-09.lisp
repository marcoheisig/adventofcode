(defpackage :adventofcode-2018-day-9
  (:use :cl))

(in-package :adventofcode-2018-day-9)

(defgeneric insert-after (new current))

(defgeneric delete-node (node))

(defgeneric clockwise (node n))

(defgeneric counter-clockwise (node n))

(defclass node ()
  ((%number :initarg :number :reader node-number)
   (%successor :initarg :successor :accessor successor)
   (%predecessor :initarg :predecessor :accessor predecessor)))

(defmethod insert-after ((new node) (current node))
  (psetf (predecessor new) current
         (successor new) (successor current)
         (predecessor (successor current)) new
         (successor current) new)
  new)

(defmethod delete-node ((node node))
  (psetf (successor (predecessor node)) (successor node)
         (predecessor (successor node)) (predecessor node))
  (node-number node))

(defmethod insert-after ((new integer) (current node))
  (insert-after (make-instance 'node :number new) current))

(defmethod clockwise ((node node) (n integer))
  (loop repeat n do (setf node (successor node)))
  node)

(defmethod counter-clockwise ((node node) (n integer))
  (loop repeat n do (setf node (predecessor node)))
  node)

(defun make-initial-node ()
  (let ((node (make-instance 'node :number 0)))
    (setf (successor node) node)
    (setf (predecessor node) node)
    node))

(deftype non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(defun make-initial-score (n-players)
  (make-array n-players
              :initial-element 0
              :element-type 'non-negative-fixnum))

(defun play-marble-game (n-players n-marbles)
  (declare (optimize speed)) ; We want tail call optimization.
  (labels ((play (node player marble score)
             #1=(declare (non-negative-fixnum player marble n-players n-marbles)
                         (type (simple-array non-negative-fixnum (*)) score))
             (cond ((= marble n-marbles)
                    score)
                   ((zerop (mod marble 23))
                    (play-special-turn node player marble score))
                   (t
                    (play-regular-turn node player marble score))))
           (play-regular-turn (node player marble score)
             #1#
             (play (insert-after marble (successor node))
                   (mod (1+ player) n-players)
                   (1+ marble)
                   score))
           (play-special-turn (node player marble score)
             #1#
             (let ((next-node (counter-clockwise node 6)))
               (incf (aref score player) marble)
               (incf (aref score player)
                     (the non-negative-fixnum
                          (delete-node (predecessor next-node))))
               (play next-node
                     (mod (1+ player) n-players)
                     (1+ marble)
                     score))))
    (play (make-initial-node) 0 1 (make-initial-score n-players))))

(defun solve-exercise-1 ()
  (reduce #'max (play-marble-game 478 71240)))

(defun solve-exercise-2 ()
  (reduce #'max (play-marble-game 478 7124000)))
