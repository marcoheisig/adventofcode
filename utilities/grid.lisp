(in-package :adventofcode)

;;; Since so many AoC exercises work on 2D grids, I decided to factor out
;;; the grid code into a reusable module.

(defstruct (grid (:constructor make-grid)
                 (:predicate gridp)
                 (:copier nil))
  (table (make-hash-table :test #'equal) :type hash-table :read-only t)
  (default (constantly nil) :type function :read-only t))

(defun grid-ref (grid x y)
  (check-type x integer)
  (check-type y integer)
  (let ((table (grid-table grid))
        (key (cons x y)))
    (multiple-value-bind (value present-p) (gethash key table)
      (if present-p
          value
          (setf (gethash key table)
                (funcall (grid-default grid) x y))))))

(defun (setf grid-ref) (value grid x y)
  (check-type x integer)
  (check-type y integer)
  (setf (gethash (cons x y) (grid-table grid)) value))

(defun read-grid (stream x-y-char-fn &optional (default (constantly nil)))
  (let ((grid (make-grid :default default)))
    (loop for line = (read-line stream nil nil) for y from 0 while line do
      (loop for char across line for x from 0 do
        (setf (grid-ref grid x y)
              (funcall x-y-char-fn x y char))))
    grid))

(defun copy-grid (grid copy-entry)
  (declare (grid grid))
  (let ((new-grid (make-grid :default (grid-default grid))))
    (maphash
     (lambda (key entry)
       (destructuring-bind (x . y) key
         (setf (grid-ref new-grid x y)
               (funcall copy-entry entry))))
     (grid-table grid))
    new-grid))

(defun grid-bounding-box (grid)
  (let ((min-x most-positive-fixnum) (max-x most-negative-fixnum)
        (min-y most-positive-fixnum) (max-y most-negative-fixnum))
    (loop for (x . y) being the hash-keys of (grid-table grid) do
      (setf min-x (min x min-x))
      (setf max-x (max x max-x))
      (setf min-y (min y min-y))
      (setf max-y (max y max-y)))
    (values min-x max-x min-y max-y)))

(defun print-grid (grid element-char &key (stream t) (origin :top-left))
  (multiple-value-bind (min-x max-x min-y max-y) (grid-bounding-box grid)
    (fresh-line stream)
    (macrolet ((printer (beg-x dir-x end-x beg-y dir-y end-y)
                 `(loop for y from ,beg-y ,dir-y ,end-y do
                   (loop for x from ,beg-x ,dir-x ,end-x do
                     (write-char (funcall element-char (grid-ref grid x y)) stream))
                   (terpri stream))))
      (ecase origin
        (:top-left (printer min-x to max-x min-y to max-y))
        (:top-right (printer max-x downto min-x min-y to max-y))
        (:bottom-left (printer min-x to max-x max-y downto min-y))
        (:bottom-right (printer max-x downto min-x max-y downto min-y))))
    (finish-output stream)))

(defun map-sparse-grid (function grid)
  ;; The behavior of MAPHASH is undefined once we mutate entries apart from
  ;; the current one, so we create a temporary copy of all entries first.
  (let ((entries '()))
    (maphash
     (lambda (key entry)
       (push (cons key entry) entries))
     (grid-table grid))
    (loop for ((x . y) . entry) in entries do
      (funcall function x y entry))))

(defun map-dense-grid (function grid)
  (multiple-value-bind (min-x max-x min-y max-y) (grid-bounding-box grid)
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
        (funcall function x y (grid-ref grid x y))))))

(defun filter-grid (grid predicate)
  (let ((matching '()))
    (map-sparse-grid
     (lambda (x y tile)
       (when (funcall predicate x y tile)
         (push tile matching)))
     grid)
    matching))

(defun grid-center (grid)
  (multiple-value-bind (min-x max-x min-y max-y) (grid-bounding-box grid)
    (values (floor (- max-x min-x) 2) (floor (- max-y min-y) 2))))
