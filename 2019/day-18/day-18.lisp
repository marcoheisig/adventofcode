(defpackage :adventofcode-2019-day-18
  (:use :cl))

(in-package :adventofcode-2019-day-18)

;;; Vault Tiles

(defclass key-mixin ()
  ((%key :initarg :key :reader key)
   (%id :initarg :id :reader id)))

(defclass tile (aoc:position-mixin)
  ;; The table tracks whether this tile has already been visited with a
  ;; certain set of known key ids.
  ((%table :initform (make-hash-table :test #'eql) :reader table)))

(defclass empty-tile (tile)
  ())

(defclass initial-tile (empty-tile)
  ())

(defclass wall-tile (tile)
  ())

(defclass key-tile (tile key-mixin)
  ())

(defclass door-tile (tile key-mixin)
  ())

(defgeneric tile-char (tile)
  (:method ((tile empty-tile)) #\.)
  (:method ((tile initial-tile)) #\@)
  (:method ((tile wall-tile)) #\#)
  (:method ((tile key-tile)) (key tile))
  (:method ((tile door-tile)) (char-upcase (key tile))))

;;; The Vault

(defun read-vault (stream)
  (let* (initial-tile
         (key-id 1)
         (key-id-alist '())
         (all-keys 0)
         (grid (aoc:read-grid
                stream
                (lambda (x y char)
                  (case char
                    (#\. (make-instance 'empty-tile :x x :y y))
                    (#\# (make-instance 'wall-tile  :x x :y y))
                    (#\@ (setf initial-tile (make-instance 'initial-tile :x x :y y)))
                    (otherwise
                     (let* ((key (char-downcase char))
                            (id (or (cdr (assoc key key-id-alist))
                                    (let ((id key-id))
                                      (setf key-id (ash key-id 1))
                                      (setf all-keys (logior all-keys id))
                                      (push (cons key id) key-id-alist)
                                      id))))
                       (if (upper-case-p char)
                           (make-instance 'door-tile :key key :id id :x x :y y)
                           (make-instance 'key-tile :key key :id id :x x :y y)))))))))
    (print key-id-alist)
    (values
     grid
     initial-tile
     all-keys)))

;;; Solving the Puzzle

(defun solve-day-18-part-1 ()
  (multiple-value-bind (grid initial-tile all-keys)
      (with-open-file (stream "input") (read-vault stream))
    (aoc:print-grid grid #'tile-char)
    (let ((seam (list (cons initial-tile 0)))
          (new-seam '()))
      (loop for steps from 0 do
        (format t "~&Steps ~3D Keys: ~2D Seam size: ~3D~%"
                steps (reduce #'max seam :key (lambda (x) (logcount (cdr x)))) (length seam))
        (loop for (tile . unlocked-key-mask) in seam do
          (with-accessors ((x aoc:x) (y aoc:y) (table table)) tile
            (unless (gethash unlocked-key-mask table)
              (setf (gethash unlocked-key-mask table) t)
              (when (eql unlocked-key-mask all-keys)
                (return-from solve-day-18-part-1 steps))
              (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)) do
                (let ((neighbor (aoc:grid-ref grid (+ x dx) (+ y dy))))
                  (etypecase neighbor
                    (wall-tile)
                    (empty-tile
                     (push (cons neighbor unlocked-key-mask) new-seam))
                    (door-tile
                     (when (logtest (id neighbor) unlocked-key-mask)
                       (push (cons neighbor unlocked-key-mask) new-seam)))
                    (key-tile
                     (push (cons neighbor (logior unlocked-key-mask (id neighbor)))
                           new-seam))))))))
        (shiftf seam new-seam '())))))

(defun compute-key-mask (grid initial-tile)
  (multiple-value-bind (min-x max-x min-y max-y) (aoc:grid-bounding-box grid)
    (with-accessors ((x aoc:x) (y aoc:y)) initial-tile
      (let ((mid-x (floor (- max-x min-x) 2))
            (mid-y (floor (- max-y min-y) 2)))
        (flet ((key-mask (predicate)
                 (let ((mask 0))
                   (aoc:map-sparse-grid
                    (lambda (x y tile)
                      (when (and (typep tile 'key-tile)
                                 (funcall predicate x y))
                        (setf mask (logior mask (id tile)))))
                    grid)
                   mask)))
          (if (<= x mid-x)
              (if (<= y mid-y)
                  (key-mask (lambda (x y) (and (<= x mid-x) (<= y mid-y))))
                  (key-mask (lambda (x y) (and (<= x mid-x) (>  y mid-y)))))
              (if (<= y mid-y)
                  (key-mask (lambda (x y) (and (>  x mid-x) (<= y mid-y))))
                  (key-mask (lambda (x y) (and (>  x mid-x) (>  y mid-y)))))))))))

(defun compute-paths (grid initial-tile initial-mask full-mask)
  (let ((seam (list (list initial-tile initial-mask '())))
        (new-seam '())
        (paths '()))
    (loop for steps from 1 while seam do
      (loop for (tile unlocked-key-mask path) in seam do
        (with-accessors ((x aoc:x) (y aoc:y) (table table)) tile
          (unless (gethash unlocked-key-mask table)
            (setf (gethash unlocked-key-mask table) t)
            (if (eql unlocked-key-mask full-mask)
                (push (reverse path) paths)
                (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)) do
                  (let ((neighbor (aoc:grid-ref grid (+ x dx) (+ y dy))))
                    (etypecase neighbor
                      (wall-tile)
                      (empty-tile
                       (push (list neighbor unlocked-key-mask path) new-seam))
                      (door-tile
                       (let ((id (id neighbor)))
                         (when (logtest id unlocked-key-mask)
                           (push (list neighbor
                                       unlocked-key-mask
                                       `((:use ,id ,steps) . ,path))
                                 new-seam))))
                      (key-tile
                       (let ((id (id neighbor)))
                         (push (list neighbor
                                     (logior unlocked-key-mask id)
                                     (if (logtest id unlocked-key-mask)
                                         path
                                         `((:get ,id ,steps) . ,path)))
                               new-seam))))))))))
      (shiftf seam new-seam '()))
    paths))

(defun path-total-steps (path)
  (if (null path)
      0
      (reduce #'max path :key #'third)))

(defun merge-paths (&rest paths)
  (print paths)
  (let* ((total-size (reduce #'+ paths :key #'length))
         (total-steps (reduce #'+ paths :key #'path-total-steps))
         (path-vector (coerce paths 'simple-vector))
         (key-mask 0))
    (loop repeat total-size do
      (loop for index below (length path-vector) do
        (symbol-macrolet ((path (svref path-vector index)))
          (unless (null path)
            (destructuring-bind (action id steps) (first path)
              (declare (ignore steps))
              (ecase action
                (:get (setf key-mask (logior id key-mask)) (pop path))
                (:use (when (logtest id key-mask) (pop path)))))))))
    (if (every #'null path-vector)
        total-steps
        nil)))

(defun solve-day-18-part-2 ()
  (let* ((grid (with-open-file (stream "input2") (read-vault stream)))
         (initial-tiles (aoc:filter-grid grid (lambda (x y tile)
                                                (declare (ignore x y))
                                                (typep tile 'initial-tile))))
         (key-masks
           (loop for initial-tile in initial-tiles
                 collect (compute-key-mask grid initial-tile)))
         (full-key-mask (apply #'logior key-masks)))
    (destructuring-bind (tile-1 tile-2 tile-3 tile-4) initial-tiles
      (destructuring-bind (mask-1 mask-2 mask-3 mask-4) key-masks
        (alexandria:map-product
         (lambda (path-1 path-2 path-3 path-4)
           (merge-paths path-1 path-2 path-3 path-4))
         (compute-paths grid tile-1 (logandc2 full-key-mask mask-1) full-key-mask)
         (compute-paths grid tile-2 (logandc2 full-key-mask mask-2) full-key-mask)
         (compute-paths grid tile-3 (logandc2 full-key-mask mask-3) full-key-mask)
         (compute-paths grid tile-4 (logandc2 full-key-mask mask-4) full-key-mask))))))

