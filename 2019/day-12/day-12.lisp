(defpackage :adventofcode-2019-day-12
  (:use :cl))

(in-package :adventofcode-2019-day-12)

(defstruct moon
  (px 0) (py 0) (pz 0)
  (vx 0) (vy 0) (vz 0))

(defmethod print-object ((moon moon) stream)
  (with-accessors ((px moon-px) (py moon-py) (pz moon-pz)
                   (vx moon-vx) (vy moon-vy) (vz moon-vz)) moon
    (print-unreadable-object (moon stream :type t :identity t)
      (format stream "pos=<~D ~D ~D> vel=<~D ~D ~D>" px py pz vx vy vz))))

(defun update-velocities (moons)
  (alexandria:map-combinations
   (lambda (ab)
     (destructuring-bind (a b) ab
       (with-accessors ((px1 moon-px) (py1 moon-py) (pz1 moon-pz)
                        (vx1 moon-vx) (vy1 moon-vy) (vz1 moon-vz)) a
         (with-accessors ((px2 moon-px) (py2 moon-py) (pz2 moon-pz)
                          (vx2 moon-vx) (vy2 moon-vy) (vz2 moon-vz)) b
           (macrolet ((pairwise-gravity (xa xb va vb)
                        `(cond ((< ,xa ,xb) (incf ,va +1) (incf ,vb -1))
                               ((= ,xa ,xb) (values))
                               ((> ,xa ,xb) (incf ,va -1) (incf ,vb +1)))))
             (pairwise-gravity px1 px2 vx1 vx2)
             (pairwise-gravity py1 py2 vy1 vy2)
             (pairwise-gravity pz1 pz2 vz1 vz2))))))
   moons :length 2 :copy nil))

(defun update-position (moon)
  (with-accessors ((px moon-px) (py moon-py) (pz moon-pz)
                   (vx moon-vx) (vy moon-vy) (vz moon-vz)) moon
    (incf px vx)
    (incf py vy)
    (incf pz vz)))

(defun total-energy (moon)
  (with-accessors ((px moon-px) (py moon-py) (pz moon-pz)
                   (vx moon-vx) (vy moon-vy) (vz moon-vz)) moon
    (let ((potential-energy (+ (abs px) (abs py) (abs pz)))
          (kinetic-energy (+ (abs vx) (abs vy) (abs vz))))
      (* potential-energy kinetic-energy))))

(defun solve-day-12-part-1 ()
  (let ((a (make-moon :px -6 :py 2 :pz -9))
        (b (make-moon :px 12 :py -14 :pz -4))
        (c (make-moon :px 9 :py 5 :pz -6))
        (d (make-moon :px -1 :py -4 :pz 9)))
    (let ((moons (list a b c d)))
      (loop repeat 1000 do
        (update-velocities moons)
        (mapc #'update-position moons))
      (reduce #'+ moons :key #'total-energy))))

;;; Prepare for sum ugly copy pasta...

(defun update-x-velocities (moons)
  (alexandria:map-combinations
   (lambda (ab)
     (destructuring-bind (a b) ab
       (with-accessors ((px1 moon-px) (py1 moon-py) (pz1 moon-pz)
                        (vx1 moon-vx) (vy1 moon-vy) (vz1 moon-vz)) a
         (with-accessors ((px2 moon-px) (py2 moon-py) (pz2 moon-pz)
                          (vx2 moon-vx) (vy2 moon-vy) (vz2 moon-vz)) b
           (macrolet ((pairwise-gravity (xa xb va vb)
                        `(cond ((< ,xa ,xb) (incf ,va +1) (incf ,vb -1))
                               ((= ,xa ,xb) (values))
                               ((> ,xa ,xb) (incf ,va -1) (incf ,vb +1)))))
             (pairwise-gravity px1 px2 vx1 vx2))))))
   moons :length 2 :copy nil))

(defun update-y-velocities (moons)
  (alexandria:map-combinations
   (lambda (ab)
     (destructuring-bind (a b) ab
       (with-accessors ((px1 moon-px) (py1 moon-py) (pz1 moon-pz)
                        (vx1 moon-vx) (vy1 moon-vy) (vz1 moon-vz)) a
         (with-accessors ((px2 moon-px) (py2 moon-py) (pz2 moon-pz)
                          (vx2 moon-vx) (vy2 moon-vy) (vz2 moon-vz)) b
           (macrolet ((pairwise-gravity (xa xb va vb)
                        `(cond ((< ,xa ,xb) (incf ,va +1) (incf ,vb -1))
                               ((= ,xa ,xb) (values))
                               ((> ,xa ,xb) (incf ,va -1) (incf ,vb +1)))))
             (pairwise-gravity py1 py2 vy1 vy2))))))
   moons :length 2 :copy nil))

(defun update-z-velocities (moons)
  (alexandria:map-combinations
   (lambda (ab)
     (destructuring-bind (a b) ab
       (with-accessors ((px1 moon-px) (py1 moon-py) (pz1 moon-pz)
                        (vx1 moon-vx) (vy1 moon-vy) (vz1 moon-vz)) a
         (with-accessors ((px2 moon-px) (py2 moon-py) (pz2 moon-pz)
                          (vx2 moon-vx) (vy2 moon-vy) (vz2 moon-vz)) b
           (macrolet ((pairwise-gravity (xa xb va vb)
                        `(cond ((< ,xa ,xb) (incf ,va +1) (incf ,vb -1))
                               ((= ,xa ,xb) (values))
                               ((> ,xa ,xb) (incf ,va -1) (incf ,vb +1)))))
             (pairwise-gravity pz1 pz2 vz1 vz2))))))
   moons :length 2 :copy nil))

(defun periodicity (moons update-velocities)
  (let ((initial-moons (mapcar #'copy-moon moons)))
    (loop for steps from 1 do
      (funcall update-velocities moons)
      (mapc #'update-position moons)
      (when (equalp moons initial-moons)
        (return steps)))))

(defun solve-day-12-part-2 ()
  (let ((a (make-moon :px -6 :py 2 :pz -9))
        (b (make-moon :px 12 :py -14 :pz -4))
        (c (make-moon :px 9 :py 5 :pz -6))
        (d (make-moon :px -1 :py -4 :pz 9)))
    (let ((moons (list a b c d)))
      (lcm (periodicity moons #'update-x-velocities)
           (periodicity moons #'update-y-velocities)
           (periodicity moons #'update-z-velocities)))))
