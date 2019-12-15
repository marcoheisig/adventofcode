(defpackage :adventofcode-2019-day-14
  (:use :cl))

(in-package :adventofcode-2019-day-14)

(defstruct (reaction (:constructor make-reaction (output)))
  ;; What is produced.
  (output nil :type string)
  ;; How much is produced per step of the reaction.
  (quantity 1 :type integer)
  ;; An alist of counts and other reactions.
  (inputs nil :type list)
  ;; How much of the output is currently in stock.
  (stock 0 :type integer))

(defmethod print-object ((reaction reaction) stream)
  (print-unreadable-object (reaction stream :type t)
    (format stream "~D ~A"
            (reaction-quantity reaction)
            (reaction-output reaction))))

(defun consume (reaction count)
  (with-accessors ((output reaction-output)
                   (quantity reaction-quantity)
                   (inputs reaction-inputs)
                   (stock reaction-stock)) reaction
    ;; Ensure that enough items are in stock.
    (let ((runs (ceiling (max 0 (- count stock)) quantity)))
      (loop for (n . input) in inputs do
        (consume input (* n runs)))
      (incf stock (* quantity runs)))
    ;; Decrease the stock.
    (decf stock count)))

(defun read-reactions (filename)
  (let ((table (make-hash-table :test #'equal)))
    (flet ((ensure-reaction (name)
             (or (gethash name table)
                 (setf (gethash name table)
                       (make-reaction name)))))
      (with-open-file (stream filename :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
                (let ((recipe '()))
                  (cl-ppcre:do-register-groups (count name)
                      ("([0-9]+)[ ,=>]+([A-Z]+)" line)
                    (push (cons (parse-integer count) (ensure-reaction name))
                          recipe))
                  (destructuring-bind (quantity . reaction) (first recipe)
                    (setf (reaction-quantity reaction) quantity)
                    (setf (reaction-inputs reaction) (rest recipe)))))))
    table))

(defun solve-day-14-part-1 ()
  (let* ((table (read-reactions "input"))
         (ore (gethash "ORE" table))
         (fuel (gethash "FUEL" table)))
    (setf (reaction-stock ore) most-positive-fixnum)
    (consume fuel 1)
    (- most-positive-fixnum (reaction-stock ore))))

(defun copy-reaction-table (table)
  (let ((copy (make-hash-table :test #'equal :size (hash-table-size table))))
    ;; Copy all entries.
    (maphash
     (lambda (key reaction)
       (setf (gethash key copy)
             (copy-reaction reaction)))
     table)
    ;; Update all inputs to use the new entries.
    (maphash
     (lambda (key reaction)
       (declare (ignore key))
       (loop for cons in (reaction-inputs reaction) do
         (setf (cdr cons) (gethash (reaction-output (cdr cons)) copy))))
     copy)
    copy))

(defun solve-day-14-part-2 ()
  (let* ((table (read-reactions "input"))
         (initial-ore (expt 10 12))
         (produced-fuel 0))
    (setf (reaction-stock (gethash "ORE" table)) initial-ore)
    (loop for amount = (expt 10 11) then (floor amount 2)
          until (zerop amount) do
      (loop
        (let* ((copy (copy-reaction-table table))
               (ore (gethash "ORE" copy))
               (fuel (gethash "FUEL" copy)))
          (consume fuel amount)
          (cond ((zerop (reaction-stock ore))
                 (return))
                (t
                 (incf produced-fuel amount)
                 (setf table copy))))))
    produced-fuel))
