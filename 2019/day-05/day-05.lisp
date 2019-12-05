(defpackage :adventofcode-2019-day-5
  (:use :cl))

(in-package :adventofcode-2019-day-5)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (map 'vector
         #'parse-integer
         (split-sequence:split-sequence #\, (read-line stream nil nil)))))

;;; The Intcode Computer.

(defvar *intcode-computer-input* *standard-input*)
(defvar *intcode-computer-output* *standard-output*)

(defun invalid-opcode (tape ip prefix)
  (declare (ignore prefix))
  (error "Invalid opcode ~D." (nth-value 1 (floor (svref tape ip) 100))))

(defvar *opcode-table* (make-array 100 :initial-element 'invalid-opcode))

(defmacro instruction (n)
  `(svref *opcode-table* ,n))

(defun intcode-eval (tape)
  (catch 'halt
    (let ((ip 0))
      (loop
        (multiple-value-bind (prefix opcode) (floor (svref tape ip) 100)
          (setf ip (funcall (instruction opcode) tape ip prefix)))))))

(defun access (tape address mode)
  (ecase mode
    (0 (svref tape (svref tape address)))
    (1 (svref tape address))))

(defun (setf access) (value tape address mode)
  (ecase mode
    (0 (setf (svref tape (svref tape address)) value))))

(defmacro define-instruction (opcode name lambda-list &body body)
  (let ((tape (gensym "TAPE"))
        (ip (gensym "IP"))
        (prefix (gensym "PREFIX"))
        (mvars (loop repeat (length lambda-list) collect (gensym "MVAR"))))
    `(progn
       (defun ,name (,tape ,ip ,prefix)
         (destructuring-bind ,mvars (prefix-modes ,prefix ,(length lambda-list))
           (declare (ignorable ,@mvars))
           (symbol-macrolet
               ,(loop for variable in lambda-list
                      for mvar in mvars
                      for offset from 1
                      collect
                      `(,variable (access ,tape (+ ,ip ,offset) ,mvar)))
             (flet ((tape (address)
                      (access ,tape address 0))
                    (jump-to (address)
                      (return-from ,name address)))
               (declare (ignorable #'tape))
               ,@body
               (jump-to (+ ,ip ,(1+ (length lambda-list))))))))
       (setf (instruction ,opcode) ',name))))

(defun prefix-modes (prefix length)
  (loop repeat length collect
                      (multiple-value-bind (rest mode) (floor prefix 10)
                        (setf prefix rest)
                        mode)))

;;; The instructions.

(define-instruction 1 add (a b c)
  (setf c (+ a b)))

(define-instruction 2 mul (a b c)
  (setf c (* a b)))

(define-instruction 3 input (dst)
  (setf dst (read *intcode-computer-input*)))

(define-instruction 4 output (src)
  (write src :stream *intcode-computer-output*))

(define-instruction 5 jump-if-true (a b)
  (unless (zerop a)
    (jump-to b)))

(define-instruction 6 jump-if-false (a b)
  (when (zerop a)
    (jump-to b)))

(define-instruction 7 less-than (a b c)
  (setf c (if (< a b) 1 0)))

(define-instruction 8 equals (a b c)
  (setf c (if (= a b) 1 0)))

(define-instruction 99 hlt ()
  (throw 'halt (tape 0)))

;;; End of instructions.

(defun solve-day-5-part-1 ()
  (with-input-from-string (*intcode-computer-input* "1")
    (intcode-eval (read-input "input"))))

(defun solve-day-5-part-2 ()
  (with-input-from-string (*intcode-computer-input* "5")
    (intcode-eval (read-input "input"))))
