(defpackage :adventofcode-2019-day-20
  (:use :cl))

(in-package :adventofcode-2019-day-20)

(defclass tile (aoc:position-mixin)
  ((%neighbors :initform '() :accessor tile-neighbors)
   (%distances :initform (make-hash-table :test #'eql) :accessor tile-distances)))

(defmacro tile-distance (tile level)
  `(gethash ,level (tile-distances ,tile)))

(defclass wall-tile (tile) ())
(defclass empty-tile (tile) ())
(defclass void-tile (tile) ())
(defclass char-tile (tile)
  ((%char :initarg :char :reader tile-char)
   (%label :accessor tile-label)))

(defmethod print-object ((tile tile) stream)
  (print-unreadable-object (tile stream :type t)
    (format stream ":X ~D :Y ~D"
            (aoc:x tile)
            (aoc:y tile))))

(defun wall-tile-p (tile) (typep tile 'wall-tile))
(defun empty-tile-p (tile) (typep tile 'empty-tile))
(defun void-tile-p (tile) (typep tile 'empty-tile))
(defun char-tile-p (tile) (typep tile 'char-tile))

(defgeneric tile-char (tile)
  (:method ((tile wall-tile)) #\#)
  (:method ((tile empty-tile)) #\.)
  (:method ((tile t)) #\space))

(defun make-tile (x y char)
  (case char
    (#\. (make-instance 'empty-tile :x x :y y))
    (#\# (make-instance 'wall-tile :x x :y y))
    (#\space (make-instance 'void-tile :x x :y y))
    (otherwise (make-instance 'char-tile :char char :x x :y y))))

(defun read-maze (file)
  (with-open-file (stream file :direction :input)
    (let* ((grid (aoc:read-grid stream #'make-tile (lambda (x y) (make-tile x y #\space))))
           initial-tile final-tile
           (char-tiles (aoc:filter-grid grid
                                        (lambda (x y tile)
                                          (declare (ignore x y))(char-tile-p tile)))))
      ;; Compute the labels of each char tile.
      (loop for char-tile in char-tiles do
        (with-accessors ((x aoc:x) (y aoc:y)) char-tile
          (loop for (dx . dy) in '((1 . 0) (0 . 1)) do
            (let ((neighbor (aoc:grid-ref grid (+ x dx) (+ y dy))))
              (when (char-tile-p neighbor)
                (let ((label (format nil "~C~C" (tile-char char-tile) (tile-char neighbor))))
                  (setf (tile-label char-tile) label)
                  (setf (tile-label neighbor) label)))))))
      ;; Now compute the neighbors of each tile.
      (aoc:map-sparse-grid
       (lambda (x y tile)
         (when (empty-tile-p tile)
           (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)) do
             (let ((neighbor (aoc:grid-ref grid (+ x dx) (+ y dy))))
               (typecase neighbor
                 (empty-tile (pushnew neighbor (tile-neighbors tile)))
                 (char-tile
                  (let ((label (tile-label neighbor)))
                    (when (string= label "AA")
                      (setf initial-tile tile))
                    (when (string= label "ZZ")
                      (setf final-tile tile))
                    (loop for other-char-tile in (remove label char-tiles :test-not #'string= :key #'tile-label) do
                      (unless (eq other-char-tile neighbor)
                        (with-accessors ((x aoc:x) (y aoc:y)) other-char-tile
                          (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)) do
                            (let ((neighbor (aoc:grid-ref grid (+ x dx) (+ y dy))))
                              (when (empty-tile-p neighbor)
                                (pushnew neighbor (tile-neighbors tile)))))))))))))))
       grid)
      (values grid initial-tile final-tile))))

(defun solve-day-20-part-1 ()
  (multiple-value-bind (maze initial-tile final-tile) (read-maze "input")
    (let ((seam (list initial-tile))
          (new-seam '()))
      (loop for distance from 0 while seam do
        (format t "~&Distance: ~3D Seam: ~3D~%" distance (length seam))
        (loop for tile in seam do
          (unless (tile-distance tile 0)
            (setf (tile-distance tile 0) distance)
            (dolist (neighbor (tile-neighbors tile))
              (push neighbor new-seam))))
        (shiftf seam new-seam '()))
      (aoc:print-grid maze #'tile-char)
      (tile-distance final-tile 0))))

(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defun solve-day-20-part-2 ()
  (multiple-value-bind (grid initial-tile final-tile) (read-maze "input")
    (multiple-value-bind (center-x center-y) (aoc:grid-center grid)
      (let ((seam (list (cons initial-tile 0)))
            (new-seam '()))
        (loop for distance from 0 while seam do
          (format t "~&Distance: ~3D Seam: ~3D~%" distance (length seam))
          (loop for (tile . level) in seam do
            (with-accessors ((tile-x aoc:x) (tile-y aoc:y)) tile
              (unless (tile-distance tile level)
                (setf (tile-distance tile level) distance)
                (when (and (zerop level) (eq tile final-tile))
                  (return-from solve-day-20-part-2 distance))
                (dolist (neighbor (tile-neighbors tile))
                  (with-accessors ((neighbor-x aoc:x) (neighbor-y aoc:y)) neighbor
                    (let ((distance (manhattan-distance tile-x tile-y neighbor-x neighbor-y)))
                      (if (= 1 distance)
                          ;; Ordinary move.
                          (push (cons neighbor level) new-seam)
                          ;; Teleport.
                          (let ((before (manhattan-distance tile-x tile-y center-x center-y))
                                (after (manhattan-distance neighbor-x neighbor-y center-x center-y)))
                            (if (> after before)
                                ;; Teleport to a higher level.
                                (push (cons neighbor (1+ level)) new-seam)
                                ;; Teleport to a lower level.
                                (unless (zerop level)
                                  (push (cons neighbor (1- level)) new-seam)))))))))))
          (shiftf seam new-seam '()))))))
