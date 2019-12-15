(in-package :adventofcode)

;;; Since so many AoC exercises work on 2D grids, I decided to factor out
;;; the grid code into a reusable module.

(defstruct (grid (:constructor make-grid (&key table default))
                 (:predicate gridp)
                 (:copier nil))
  (table (make-hash-table :test #'equal) :type hash-table :read-only t)
  (default (constantly nil) :type function :read-only t))

(defun grid-ref (grid x y)
  (let ((table (grid-table grid))
        (key (cons x y)))
    (multiple-value-bind (value present-p) (gethash key table)
      (if present-p
          value
          (setf (gethash key table)
                (funcall (grid-default grid) x y))))))

(defun (setf grid-ref) (value grid x y)
  (setf (gethash (cons x y) (grid-table grid)) value))

(defun copy-grid (grid copy-entry)
  (declare (grid grid))
  (let ((new-grid (make-grid)))
    (maphash
     (lambda (key entry)
       (destructuring-bind (x . y) key
         (setf (grid-ref grid x y) (funcall copy-entry entry))))
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

(defun draw-grid (grid element-char &optional (stream t))
  (multiple-value-bind (min-x max-x min-y max-y) (grid-bounding-box grid)
    (fresh-line stream)
    (loop for y from max-y downto min-y do
      (loop for x from min-x to max-x do
        (write-char (funcall element-char (grid-ref grid x y)) stream))
      (terpri stream))
    (finish-output stream)))

(defun map-sparse-grid (function grid)
  (maphash
   (lambda (key entry)
     (destructuring-bind (x . y) key
       (funcall function x y entry)))
   (grid-table grid)))

(defun map-dense-grid (function grid)
  (multiple-value-bind (min-x max-x min-y max-y) (grid-bounding-box grid)
    (loop for y from min-y to max-y do
      (loop for x from min-x to max-x do
        (funcall function x y (grid-ref grid x y))))))
