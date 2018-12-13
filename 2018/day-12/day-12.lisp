(defpackage :adventofcode-2018-day-12
  (:use :cl))

(in-package :adventofcode-2018-day-12)

(defun read-input-from-file (file)
  (with-open-file (stream file :direction :input)
    (values
     (prog1 (subseq (read-line stream) 15)
       (read-line stream)) ; Skip the second line.
     (loop for line = (read-line stream nil nil)
           while line
           collect
           (cons (subseq line 0 5) (aref line 9))))))

(defun compile-rules (rules)
  (let ((stream-fn (compile nil (make-stream-update-lambda rules))))
    (lambda (state offset)
      (let ((input-stream
              (make-concatenated-stream
               (make-string-input-stream state offset)
               (make-string-input-stream "..."))))
        (with-output-to-string (output-stream)
          (funcall stream-fn input-stream output-stream))))))

(defun make-stream-update-lambda (rules)
  (labels ((next-pattern (pattern next-char)
             (concatenate 'string (subseq pattern 1) next-char))
           (next-output-char (pattern next-char)
             (cdr (assoc (next-pattern pattern next-char) rules
                         :test #'string=)))
           (translate-rule (rule)
             (destructuring-bind (pattern . result) rule
               (declare (ignore result))
               `(,(intern pattern)
                 (let ((next (read-char src nil #\!)))
                   (cond ((char= next #\#)
                          (write-char ,(next-output-char pattern "#") dst)
                          (go ,(intern (next-pattern pattern "#"))))
                         ((char= next #\.)
                          (write-char ,(next-output-char pattern ".") dst)
                          (go ,(intern (next-pattern pattern "."))))
                         (t (return))))))))
    `(lambda (src dst)
       (declare (optimize speed))
       (block nil
         (tagbody
            (go ,(intern "....."))
            ,@(apply #'append (mapcar #'translate-rule rules)))))))

(defun simulate-plant-growth (state rules steps)
  (let ((fn (compile-rules rules))
        (total-offset 0)
        (previous-state state))
    (loop repeat steps
          for step from 1
          for offset = (position #\# state) do
            (shiftf previous-state state (funcall fn state offset))
            (incf total-offset (- offset 2))
            ;; Detect fixpoints (necessary for exercise 2)
            (when (string= previous-state state)
              (incf total-offset (* (- offset 2) (- steps step)))
              (loop-finish)))
    (loop for pot across state
          for pot-number from total-offset
          when (char= pot #\#)
            sum pot-number)))

(defun solve-exercise-1 ()
  (multiple-value-bind (state rules)
      (read-input-from-file "input")
    (simulate-plant-growth state rules 20)))

(defun solve-exercise-2 ()
  (multiple-value-bind (state rules)
      (read-input-from-file "input")
    (simulate-plant-growth state rules 50000000000)))
