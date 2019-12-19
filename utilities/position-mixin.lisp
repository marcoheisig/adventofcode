(in-package :adventofcode)

(defclass position-mixin ()
  ((%x :initarg :x :type integer :accessor x)
   (%y :initarg :y :type integer :accessor y)))
