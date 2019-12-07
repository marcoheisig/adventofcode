(defpackage :adventofcode-2019-day-7
  (:use :cl))

(in-package :adventofcode-2019-day-7)

;;; The Intcode Computer.

(defun read-intcode-program (filename)
  (with-open-file (stream filename :direction :input)
    (map 'vector
         #'parse-integer
         (split-sequence:split-sequence #\, (read-line stream nil nil)))))

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
  (write src :stream *intcode-computer-output*)
  (write-char #\space *intcode-computer-output*))

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

(defun best-permutation (function sequence)
  (let ((best-value nil)
        (best-permutation nil))
    (alexandria:map-permutations
     (lambda (permutation)
       (let ((value (funcall function permutation)))
         (when (or (null best-value) (< best-value value))
           (setf best-value value)
           (setf best-permutation permutation))))
     sequence)
    (values best-permutation best-value)))

(defun amplifier (program phase-setting)
  (lambda (input-signal)
    (parse-integer
     (with-output-to-string (*intcode-computer-output*)
       (with-input-from-string (*intcode-computer-input*
                                (format nil "~D ~D" phase-setting input-signal))
         (intcode-eval (copy-seq program)))))))

(defun solve-day-07-part-1 ()
  (let ((program (read-intcode-program "input")))
    (best-permutation
     (lambda (phase-settings)
       (funcall
        (apply #'alexandria:compose
               (mapcar (lambda (phase-setting) (amplifier program phase-setting))
                       phase-settings))
        0))
     '(0 1 2 3 4))))

;;; A part-2 amplifier is different from an intcode machine, in that each
;;; output instruction transfers control.
(defstruct amplifier tape ip next phase-setting)

(defun run-amplifier (amplifier input-value)
  (with-accessors ((tape amplifier-tape)
                   (ip amplifier-ip)
                   (phase-setting amplifier-phase-setting))
      amplifier
    (with-input-from-string (*intcode-computer-input*
                             (cond ((null phase-setting)
                                    (format nil "~D" input-value))
                                   (t
                                    (prog1 (format nil "~D ~D" phase-setting input-value)
                                      (setf phase-setting nil)))))
      (parse-integer
       (with-output-to-string (*intcode-computer-output*)
         (loop named amplifier-loop do
           (multiple-value-bind (prefix opcode) (floor (svref tape ip) 100)
             (setf ip (funcall (instruction opcode) tape ip prefix))
             (when (= opcode 4)
               (return-from amplifier-loop)))))))))

(defun run-amplifier-loop (tape phase-settings)
  (let* ((n (length phase-settings))
         (amplifiers
           (loop for phase-setting in phase-settings
                 collect
                 (make-amplifier
                  :tape (copy-seq tape)
                  :ip 0
                  :phase-setting phase-setting))))
    ;; Connect the amplifiers.
    (loop for amplifier in amplifiers
          for index from 0 do
            (setf (amplifier-next amplifier)
                  (nth (mod (1+ index) n) amplifiers)))
    ;; Run the loop.
    (let ((value 0))
      (catch 'halt
        (loop for amplifier = (first amplifiers) then (amplifier-next amplifier) do
          (setf value (run-amplifier amplifier value))))
      value)))

(defun solve-day-07-part-2 ()
  (let ((program (read-intcode-program "input")))
    (best-permutation
     (lambda (phase-settings)
       (run-amplifier-loop program phase-settings))
     '(5 6 7 8 9))))
