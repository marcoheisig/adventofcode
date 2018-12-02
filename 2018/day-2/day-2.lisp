(defpackage :adventofcode-2018-day-2
  (:use :cl))

(in-package :adventofcode-2018-day-2)

(defun read-all-ids (file)
  (with-open-file (stream file :direction :input)
    (loop for id = (read-line stream nil nil)
          while id
          collect id)))

(defun count-2s-and-3s (id)
  (let ((table (make-hash-table :test #'eql)))
    (loop for char across id do
      (incf (gethash char table 0)))
    (loop for k being the hash-values of table
          count (= k 2) into 2s
          count (= k 3) into 3s
          finally (return (values 2s 3s)))))

(defun checksum (list-of-ids)
  (let ((a 0) (b 0))
    (loop for id in list-of-ids do
      (multiple-value-bind (2s 3s) (count-2s-and-3s id)
        (when (plusp 2s)
          (incf a))
        (when (plusp 3s)
          (incf b))))
    (* a b)))

(defun similarity (string-1 string-2)
  (let ((mask (map 'list #'char= string-1 string-2)))
    (if (/= 1 (count nil mask))
        nil
        (with-output-to-string (stream)
          (loop for char across string-1
                for m in mask
                when m do (write-char char stream))))))

(defun map-pairs (function list)
  (loop for sublist on list for elt-1 = (first sublist) do
    (loop for elt-2 in (rest sublist) do
      (funcall function elt-1 elt-2))))

(defun find-similarity (list-of-ids)
  (block nil
    (map-pairs
     (lambda (a b)
       (let ((s (similarity a b)))
         (when s
           (return s))))
     list-of-ids)))
