(defpackage :adventofcode-2019-day-2
  (:use :cl))

(in-package :adventofcode-2019-day-2)

;; Requires split sequence

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (map 'vector
         #'parse-integer
         (split-sequence:split-sequence #\, (read-line stream nil nil)))))

(defun invalid-opcode (opcode)
  (error "Invalid opcode ~D." opcode))

(defvar *opcode-table* (make-array 100 :initial-element 'invalid-opcode))

(defmacro instruction (n)
  `(svref *opcode-table* ,n))

(defmacro tape (address)
  (declare (ignore address))
  (error "Only instructions can access the tape."))

(defmacro define-instruction (name opcode lambda-list &body body)
  (let ((tape (gensym "TAPE"))
        (position (gensym "POSITION")))
    `(progn
       (defun ,name (,tape ,position)
         (declare (simple-vector ,tape)
                  (type (integer 0 *) ,position))
         (macrolet ((tape (address)
                      `(svref ,',tape ,address)))
           (let ,(loop for variable in lambda-list
                       for offset from 0
                       collect
                       `(,variable (tape (+ ,position ,offset))))
             (declare (ignorable ,(first lambda-list)))
             ,@body
             (+ ,position ,(length lambda-list)))))
       (setf (instruction ,opcode) ',name))))

(define-instruction add 1 (opcode a b c)
  (setf (tape c) (+ (tape a) (tape b))))

(define-instruction mul 2 (opcode a b c)
  (setf (tape c) (* (tape a) (tape b))))

(define-instruction hlt 99 (opcode)
  (throw 'halt (tape 0)))

(defun intcode-eval (tape)
  (declare (simple-vector tape))
  (catch 'halt
    (let ((ip 0))
      (loop
        (setf ip (funcall (instruction (svref tape ip)) tape ip))))))

(defun solve-day-1-part-1 ()
  (let ((tape (read-input "input")))
    (setf (svref tape 1) 12)
    (setf (svref tape 2) 2)
    (intcode-eval tape)))

(defun solve-day-1-part-2 (desired-output)
  (let ((tape (read-input "input")))
    (loop for noun below 100 do
      (loop for verb below 100 do
        (let ((tape (copy-seq tape)))
          (setf (svref tape 1) noun)
          (setf (svref tape 2) verb)
          (when (= (intcode-eval tape) desired-output)
            (return-from solve-day-1-part-2
              (+ (* 100 noun) verb))))))))
