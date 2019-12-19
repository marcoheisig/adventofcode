(in-package :adventofcode)

;;; A mover is an object that has a direction and a position.

(deftype direction () '(member #\^ #\> #\v #\<))

(defclass mover (position-mixin)
  ((%direction :initarg :direction :type direction :accessor direction)))

(defgeneric moverp (object)
  (:method ((object t)) nil)
  (:method ((mover mover)) t))

(defgeneric move (mover distance)
  (:method ((mover mover) (distance integer))
    (with-accessors ((direction direction) (x x) (y y)) mover
      (multiple-value-bind (dx dy)
          (ecase direction
            (#\^ (values 0 1))
            (#\> (values 1 0))
            (#\v (values 0 -1))
            (#\< (values -1 0)))
        (incf x (* dx distance))
        (incf y (* dy distance))
        mover))))

(defgeneric rotate-clockwise (mover)
  (:method ((mover mover))
    (with-accessors ((direction direction)) mover
      (setf direction (ecase direction (#\^ #\>) (#\> #\v) (#\v #\<) (#\< #\^))))))

(defgeneric rotate-counterclockwise (mover)
  (:method ((mover mover))
    (with-accessors ((direction direction)) mover
      (setf direction (ecase direction (#\^ #\<) (#\< #\v) (#\v #\>) (#\> #\^))))))
