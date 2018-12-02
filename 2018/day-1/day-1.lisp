(in-package :cl-user)

(defun read-all (file)
  (with-open-file (stream file :direction :input)
    (loop for value = (read stream nil nil)
          while value
          collect value)))

(defun sum-file (file)
  (reduce #'+ (read-all file)))

(defun make-circular (list)
  (setf (cdr (last list)) list))

(defun find-duplicate-frequency (file)
  (let ((duplicates (make-hash-table :test #'eql))
        (frequency 0))
    (loop for value in (make-circular (read-all file))
          do (setf (gethash frequency duplicates) t)
             (incf frequency value)
             (when (gethash frequency duplicates)
               (return frequency)))))
