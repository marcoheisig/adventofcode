(defpackage :adventofcode-2018-day-7
  (:use :cl))

(in-package :adventofcode-2018-day-7)

(defclass task ()
  ((%name :initarg :name :reader task-name :type character)
   (%cost :initarg :cost :reader task-cost )
   (%deps :initform '() :accessor task-deps)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type t)
    (write-char (task-name task) stream)))

(defun make-task (name)
  (assert (and (characterp name)
               (upper-case-p name)))
  (make-instance 'task
    :name name
    :cost (+ 61 (char-int name) (- (char-int #\A)))))

(defvar *task-table*)

(defun intern-task (name)
  (or (gethash name *task-table*)
      (setf (gethash name *task-table*)
            (make-task name))))

(defun add-dependency (task dependency)
  (pushnew dependency (task-deps task)))

(defun remove-dependency (task dependency)
  (setf (task-deps task)
        (remove dependency (task-deps task))))

(defun read-dependency-graph (file)
  (let ((*task-table* (make-hash-table)))
    (with-open-file (stream file :direction :input)
      (loop for line = (read-line stream nil nil)
            while line do
              (add-dependency
               (intern-task (find-if #'upper-case-p line :from-end t))
               (intern-task (find-if #'upper-case-p line :start 1)))))
    (loop for task being the hash-values of *task-table*
          collect task)))

(defun solve-exercise-1 ()
  (let ((tasks (sort (read-dependency-graph "input") #'char< :key #'task-name)))
    (coerce
     (loop while tasks
           for current = (or (find-if #'null tasks :key #'task-deps)
                             (error "Invalid schedule."))
           collect (task-name current)
           do (setf tasks (remove current tasks))
              (mapc (lambda (task) (remove-dependency task current)) tasks))
     'string)))

(defclass worker ()
  ((%task :initform nil :accessor task)
   (%counter :initform 0 :accessor counter)))

(defun make-worker ()
  (make-instance 'worker))

(defvar *task-states*)

(defun task-done-p (task)
  (eq (gethash task *task-states*) :done))

(defun task-ready-p (task)
  (and (not (gethash task *task-states*))
       (every #'task-done-p (task-deps task))))

(defun advance-worker (worker)
  (when (task worker)
    (decf (counter worker))
    (when (zerop (counter worker))
      (setf (gethash (task worker) *task-states*) :done)
      (setf (task worker) nil))))

(defun grab-task (worker task)
  (setf (gethash task *task-states*) :busy)
  (setf (task worker) task)
  (setf (counter worker) (task-cost task)))

(defun worker-task-name (worker)
  (if (task worker)
      (task-name (task worker))
      #\.))

(defun solve-exercise-2 ()
  (let ((*task-states* (make-hash-table :test #'eq))
        (tasks (sort (read-dependency-graph "input") #'char< :key #'task-name))
        (workers (loop repeat 5 collect (make-worker)))
        (time 0))
    (loop named outer
          until (every #'task-done-p tasks) do
      (loop for worker in workers do
        (when (not (task worker))
          (let ((next-task (find-if #'task-ready-p tasks)))
            (when next-task
              (grab-task worker next-task)))))
      (format t "~3@A ~{~A~^ ~} ~{~A~}~%"
              time
              (mapcar #'worker-task-name workers)
              (mapcar #'task-name (remove-if-not #'task-done-p tasks)))
      (incf time)
      (loop for worker in workers do
        (advance-worker worker)))
    time))
