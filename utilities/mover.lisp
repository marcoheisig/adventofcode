(in-package :adventofcode)

;;; A mover is an object that has a direction and a position.

(deftype direction () '(member #\^ #\> #\v #\<))

(defclass mover (position-mixin)
  ((%direction :initarg :direction :type direction :accessor direction)))

(defgeneric moverp (object)
  (:method ((object t)) nil)
  (:method ((mover mover)) t))

(defgeneric move (mover &optional distance)
  (:method ((mover mover) &optional (distance 1))
    (with-accessors ((direction direction) (x x) (y y)) mover
      (multiple-value-bind (dx dy) (direction-dx-and-dy direction)
        (incf x (* dx distance))
        (incf y (* dy distance)))
      mover)))

(defun rotate-direction (direction amount)
  (declare (direction direction) (integer amount))
  (let ((vector "^>v<"))
    (schar vector (mod (+ (position direction vector) amount) 4))))

(defun direction-dx-and-dy (direction)
  (ecase direction
    (#\^ (values 0 -1))
    (#\> (values 1 0))
    (#\v (values 0 1))
    (#\< (values -1 0))))

(defgeneric rotate-right (mover &optional amount)
  (:method ((mover mover) &optional (amount 1))
    (with-accessors ((direction direction)) mover
      (setf direction (rotate-direction direction amount)))))

(defgeneric rotate-left (mover &optional amount)
  (:method ((mover mover) &optional (amount 1))
    (with-accessors ((direction direction)) mover
      (setf direction (rotate-direction direction (- amount))))))

(defgeneric peek-forward (mover grid &optional distance)
  (:method ((mover mover) grid &optional (distance 1))
    (with-accessors ((direction direction) (x x) (y y)) mover
      (multiple-value-bind (dx dy) (direction-dx-and-dy direction)
        (let ((new-x (+ x (* dx distance)))
              (new-y (+ y (* dy distance))))
          (values (grid-ref grid new-x new-y) new-x new-y))))))

(defgeneric peek-left (mover grid &optional distance)
  (:method ((mover mover) grid &optional (distance 1))
    (with-accessors ((direction direction) (x x) (y y)) mover
      (multiple-value-bind (dx dy)
          (direction-dx-and-dy (rotate-direction direction -1))
        (let ((new-x (+ x (* dx distance)))
              (new-y (+ y (* dy distance))))
          (values (grid-ref grid new-x new-y) new-x new-y))))))

(defgeneric peek-right (mover grid &optional distance)
  (:method ((mover mover) grid &optional (distance 1))
    (with-accessors ((direction direction) (x x) (y y)) mover
      (multiple-value-bind (dx dy)
          (direction-dx-and-dy (rotate-direction direction 1))
        (let ((new-x (+ x (* dx distance)))
              (new-y (+ y (* dy distance))))
          (values (grid-ref grid new-x new-y) new-x new-y))))))
