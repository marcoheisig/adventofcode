(defpackage :adventofcode-2018-day-15
  (:use :cl))

(in-package :adventofcode-2018-day-15)

(defvar *elf-attack-power* 3)

(defclass tile ()
  ((%battleground :initarg :battleground :reader battleground)))

(defclass wall (tile) ())

(defclass empty (tile) ())

(defclass unit (tile)
  ((%hit-points :initarg :hit-points :accessor hit-points)
   (%attack-power :initarg :attack-power :accessor attack-power))
  (:default-initargs :hit-points 200 :attack-power 3))

(defclass elf (unit) ()
  (:default-initargs :attack-power *elf-attack-power*))

(defclass goblin (unit) ())

(defclass battleground ()
  ((%tiles :initarg :tiles :reader tiles)))

(defgeneric tile-char (tile)
  (:method ((wall wall)) #\#)
  (:method ((empty empty)) #\.)
  (:method ((elf elf)) #\E)
  (:method ((goblin goblin)) #\G))

(defun char-tile (char battleground)
  (make-instance (ecase char (#\# 'wall) (#\. 'empty) (#\E 'elf) (#\G 'goblin))
    :battleground battleground))

(defgeneric map-tiles (function tiles predicate)
  (:method ((function function) (battleground battleground) (predicate function))
    (map-tiles function (tiles battleground) predicate))
  (:method ((function function) (tiles array) (predicate function))
    (loop for y below (array-dimension tiles 0) do
      (loop for x below (array-dimension tiles 1) do
        (let ((tile (aref tiles y x)))
          (when (funcall predicate tile)
            (funcall function tile y x)))))))

(defun map-units (function tiles)
  (map-tiles function tiles (lambda (x) (typep x 'unit))))

(defun gather (map-fn combine-fn &rest args)
  (let ((elements '()))
    (apply map-fn
           (lambda (&rest args)
             (push (apply combine-fn args) elements))
           args)
    (nreverse elements)))

(defun all-units (battleground)
  (gather #'map-units #'values battleground))

(defun map-elves (function tiles)
  (map-tiles function tiles (lambda (x) (typep x 'elf))))

(defun all-elves (battleground)
  (gather #'map-elves #'values battleground))

(defun map-goblins (function tiles)
  (map-tiles function tiles (lambda (x) (typep x 'goblin))))

(defun all-goblins (battleground)
  (gather #'map-goblins #'values battleground))

(defun tile-y-x (tile)
  (block nil
    (map-tiles
     (lambda (other-tile y x)
       (when (eq tile other-tile)
         (return (values y x))))
     (battleground tile)
     (constantly t))
    (error "Tile ~S not found" tile)))

(defmethod print-object ((unit unit) stream)
  (print-unreadable-object (unit stream :type t)
    (format stream "~S" (hit-points unit))))

(defun print-battleground (battleground &optional (stream *standard-output*))
  (with-accessors ((tiles tiles)) battleground
    (loop for y below (array-dimension tiles 0) do
      (let ((units '()))
        (loop for x below (array-dimension tiles 1) do
          (let ((tile (aref tiles y x)))
            (when (typep tile 'unit) (push tile units))
            (write-char (tile-char tile) stream)))
        (loop for unit in (nreverse units) do
          (write-char #\space stream)
          (write unit :stream stream))
        (terpri)))))

(defun read-battleground (file)
  (with-open-file (stream file :direction :input)
    (let* ((battleground (make-instance 'battleground))
           (list-of-lists
             (loop for line = (read-line stream nil nil)
                   while line
                   collect (coerce line 'list)))
           (dimensions (list (length list-of-lists)
                             (length (first list-of-lists))))
           (tiles (make-array dimensions :initial-contents list-of-lists)))
      (loop for index below (array-total-size tiles) do
        (setf (row-major-aref tiles index)
              (char-tile (row-major-aref tiles index) battleground)))
      (reinitialize-instance battleground :tiles tiles))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Combat AI

(defgeneric enemyp (a b)
  (:method ((a t) (b t)) nil)
  (:method ((elf elf) (goblin goblin)) t)
  (:method ((goblin goblin) (elf elf)) t))

(defun map-enemies (function unit)
  (map-tiles function (battleground unit)
             (lambda (x) (enemyp x unit))))

(defun map-neighbors (function battleground y x)
  (let ((tiles (tiles battleground)))
    (flet ((call (y x)
             (funcall function (aref tiles y x) y x)))
      (call (1- y) x)
      (call y (1- x))
      (call y (1+ x))
      (call (1+ y) x))))

(defun strike (attacker defender y x)
  (let ((bg1 (battleground attacker))
        (bg2 (battleground defender)))
    (assert (eq bg1 bg2))
    (unless (plusp (decf (hit-points defender)
                         (attack-power attacker)))
      ;; Defender dies.
      (setf (aref (tiles bg1) y x)
            (char-tile #\. bg1)))))

(define-modify-macro minf (number &rest more-numbers) min)

(defun compute-distance-map (initial-tile y x)
  (let* ((battleground (battleground initial-tile))
         (tiles (tiles battleground))
         (distance-map
           (make-array (array-dimensions tiles) :initial-element nil)))
    (labels ((update-seam (seam distance)
               (unless (null seam)
                 (let ((new-seam '()))
                   (loop for (tile y x) in seam do
                     (setf (aref distance-map y x) distance)
                     (when (or (eq tile initial-tile)
                               (typep tile 'empty))
                       (map-neighbors
                        (lambda (tile y x)
                          (when (null (aref distance-map y x))
                            (pushnew (list tile y x) new-seam :key #'first)))
                        battleground y x)))
                   (update-seam new-seam (1+ distance))))))
      (update-seam (list (list initial-tile y x)) 0))
    distance-map))

(defun distance (battleground y0 x0 y1 x1)
  (let ((initial-tile (aref (tiles battleground) y0 x0)))
    (aref (compute-distance-map initial-tile y0 x0) y1 x1)))

(defun one-turn (unit y x)
  (let ((battleground (battleground unit)))
    (labels ((end-turn () (return-from one-turn))
             ;; If possible, attack and end the turn.
             (maybe-attack (y x)
               (let ((nearby-enemies '()))
                 (map-neighbors
                  (lambda (neighbor y x)
                    (when (enemyp unit neighbor)
                      (push (list neighbor y x) nearby-enemies)))
                  battleground y x)
                 (unless (null nearby-enemies)
                   (destructuring-bind (enemy y x)
                       (first (stable-sort (nreverse nearby-enemies) #'<
                                           :key (alexandria:compose #'hit-points #'first)))
                     (strike unit enemy y x)
                     (end-turn))))))
      (maybe-attack y x)
      ;; Otherwise, move.
      (let ((adjacent-tiles '())
            (enemy-count 0))
        (map-enemies
         (lambda (enemy y x)
           (declare (ignore enemy))
           (incf enemy-count)
           (map-neighbors
            (lambda (tile y x)
              (when (typep tile 'empty)
                (pushnew (list tile y x) adjacent-tiles :key #'first)))
            battleground y x))
         unit)
        (when (zerop enemy-count)
          (throw 'combat-ends nil))
        (when (null adjacent-tiles) (end-turn))
        ;; Pick the closest adjacent tile.
        (let ((distance-map (compute-distance-map unit y x)))
          (flet ((adjacent-tile-distance (list)
                   (destructuring-bind (tile y x) list
                     (declare (ignore tile))
                     (or (aref distance-map y x) most-positive-fixnum))))
            (destructuring-bind (closest closest-y closest-x)
                (first (stable-sort (nreverse adjacent-tiles) #'< :key #'adjacent-tile-distance))
              (declare (ignore closest))
              ;; Move to the closest empty neighbor.
              (let ((neighbors '()))
                (map-neighbors
                 (lambda (tile y x)
                   (when (typep tile 'empty)
                     (let ((d (distance battleground closest-y closest-x y x)))
                       (when d (push (list d y x) neighbors)))))
                 battleground y x)
                (when (null neighbors) (end-turn))
                (destructuring-bind (distance target-y target-x)
                    (first (stable-sort (nreverse neighbors) #'< :key #'first))
                  (declare (ignore distance))
                  (rotatef
                   (aref (tiles battleground) y x)
                   (aref (tiles battleground) target-y target-x))
                  (maybe-attack target-y target-x))))))))))

(defun one-round (battleground)
  (let ((units '()))
    (map-units
     (lambda (unit y x)
       (push (list unit y x) units))
     battleground)
    (loop for (unit y x) in (nreverse units)
          when (plusp (hit-points unit))
          do (funcall #'one-turn unit y x))))

(defun simulate-battle (file)
  (let* ((battleground (read-battleground file))
         (full-rounds 0)
         (n-elves (length (all-elves battleground)))
         (n-goblins (length (all-goblins battleground))))
    (print-battleground battleground)
    (catch 'combat-ends
      (loop
        (one-round battleground)
        (incf full-rounds)))
    (print-battleground battleground)
    (format t "Rounds: ~D~%Outcome: ~D~%Casualties: ~D Elves, ~D Goblins~%"
            full-rounds
            (* full-rounds
               (reduce #'+ (all-units battleground)
                       :key #'hit-points))
            (- n-elves (length (all-elves battleground)))
            (- n-goblins (length (all-goblins battleground))))))

(defun solve-exercise-1 ()
  (simulate-battle "input"))

(defun solve-exercise-2 ()
  (loop for power from 4 do
    (let ((*elf-attack-power* power))
      (simulate-battle "input")
      (break))))
