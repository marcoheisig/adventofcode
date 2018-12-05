(defpackage :adventofcode-2018-day-4
  (:use :cl))

(in-package :adventofcode-2018-day-4)

(defclass record ()
  ((%who :initarg :who :accessor record-who)
   (%what :initarg :what :reader record-what)
   (%when :initarg :when :reader record-when)))

(defclass guard ()
  ((%id :initarg :id :reader guard-id)
   (%sleep-vector :reader sleep-vector
                  :initform (make-array 60 :initial-element 0))))

(defmethod print-object ((record record) stream)
  (print-unreadable-object (record stream :type t)
    (format stream "~A ~A ~A"
            (record-when record)
            (record-who record)
            (record-what record))))

(defmethod print-object ((guard guard) stream)
  (print-unreadable-object (guard stream :type t)
    (format stream "~A" (guard-id guard))))

(defvar *guards* nil
  "A hash table, mapping from guards ids to guards.")

(defun guard (id)
  (multiple-value-bind (guard present-p)
      (gethash id *guards*)
    (if present-p
        guard
        (let ((guard (make-instance 'guard :id id)))
          (setf (gethash id *guards*) guard)
          guard))))

(defun parse-record (string)
  (multiple-value-bind (match substrings)
      (cl-ppcre:scan-to-strings
       "\\[([0-9]+)-([0-9]+)-([0-9]+) +([0-9]+):([0-9]+)\\] (.*)"
       string)
    (assert match)
    (let ((action (aref substrings 5)))
      (multiple-value-bind (who what)
          (ecase (schar action 0)
            (#\w (values '? :wakes-up))
            (#\f (values '? :falls-asleep))
            (#\G (values
                  (guard
                   (parse-integer action :start (1+ (position #\# action))
                                         :junk-allowed t))
                  :begins-shift)))
        (make-instance 'record
          :who who
          :what what
          :when (local-time:encode-timestamp
                 0
                 0
                 (parse-integer (aref substrings 4))
                 (parse-integer (aref substrings 3))
                 (parse-integer (aref substrings 2))
                 (parse-integer (aref substrings 1))
                 (parse-integer (aref substrings 0))))))))

(defun read-records (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-record line))))

(defun fixup-records (records)
  (let ((who '?)
        (records (sort (copy-list records) #'local-time:timestamp<=
                       :key #'record-when)))
    (loop for record in records do
      (if (eq (record-what record) :begins-shift)
          (setf who (record-who record))
          (setf (record-who record) who)))
    records))

(defun process-records (records)
  (let ((records (fixup-records records))
        (sleep-start nil))
    (labels ((call-with-next-record (fn)
               (unless (null records)
                 (with-accessors ((who record-who)
                                  (when record-when)
                                  (what record-what))
                     (pop records)
                   (funcall fn who what when))))
             (initial-state (who what when)
               (declare (ignore who when))
               (unless (eq what :begins-shift)
                 (error "Cannot ~A before any shift has begun." what))
               (call-with-next-record #'awake-state))
             (awake-state (who what when)
               (ecase what
                 (:begins-shift
                  (call-with-next-record #'awake-state))
                 (:wakes-up
                  (error "Guard ~D cannot wake up twice." who))
                 (:falls-asleep
                  (setf sleep-start when)
                  (call-with-next-record #'sleeping-state))))
             (sleeping-state (who what when)
               (ecase what
                 ((:begins-shift :wakes-up)
                  (let ((start (local-time:timestamp-minute sleep-start))
                        (end (local-time:timestamp-minute when))
                        (sleep-vector (sleep-vector who)))
                    (loop for index from start below end do
                      (incf (svref sleep-vector index))))
                  (call-with-next-record #'awake-state))
                 (:falls-asleep
                  (error "Guard ~D cannot fall asleep twice." who)))))
      (call-with-next-record #'initial-state))))

(defun read-guards (input)
  (let ((*guards* (make-hash-table)))
    (process-records (read-records "input"))
    (loop for guard being the hash-values of *guards*
          collect guard)))

(defun total-sleep (guard)
  (reduce #'+ (sleep-vector guard)))

(defun max-sleep (guard)
  (reduce #'max (sleep-vector guard)))

(defun solve-problem-1 ()
  (let ((sleepy (first (sort (read-guards "input") #'> :key #'total-sleep))))
    (* (guard-id sleepy)
       (position (max-sleep sleepy) (sleep-vector sleepy)))))

(defun solve-problem-2 ()
  (let ((sleepy (first (sort (read-guards "input") #'> :key #'max-sleep))))
    (* (guard-id sleepy)
       (position (max-sleep sleepy) (sleep-vector sleepy)))))
