(defpackage :adventofcode-2019-day-11
  (:use :cl))

(in-package :adventofcode-2019-day-11)

(defvar *robot*)
(defvar *grid*)

;;; The Intcode Computer.

(defstruct (intcode-computer
            (:copier nil)
            (:predicate intcode-computer-p)
            (:constructor make-intcode-computer
                (&key (ip 0) (rb 0) program
                 &aux (memory (replace (make-array 10000 :initial-element 0) program)))))
  (ip nil :type unsigned-byte)
  (rb nil :type unsigned-byte)
  (memory nil :type simple-vector))

(defun read-intcode-computer-program (filename)
  (with-open-file (stream filename :direction :input)
    (map 'vector
         #'parse-integer
         (split-sequence:split-sequence #\, (read-line stream nil nil)))))

(defun invalid-opcode (intcode-computer opcode prefix)
  (declare (ignore prefix))
  (with-accessors ((memory intcode-computer-memory)
                   (ip intcode-computer-ip)) intcode-computer
    (error "Invalid opcode ~D." opcode)))

(defvar *opcode-table* (make-array 100 :initial-element 'invalid-opcode))

(defmacro instruction (n)
  `(svref *opcode-table* ,n))

(defun run-intcode-computer (intcode-computer)
  (with-accessors ((memory intcode-computer-memory)
                   (ip intcode-computer-ip)
                   (rb (intcode-computer-rb))) intcode-computer
    (catch 'halt
      (loop
        (multiple-value-bind (prefix opcode) (floor (svref memory ip) 100)
          (funcall (instruction opcode) intcode-computer opcode prefix))))))

(defun access (ic offset mode)
  (with-accessors ((memory intcode-computer-memory)
                   (ip intcode-computer-ip)
                   (rb intcode-computer-rb)) ic
    (ecase mode
      (0 (svref memory (svref memory (+ ip offset))))
      (1 (svref memory (+ ip offset)))
      (2 (svref memory (+ rb (svref memory (+ ip offset))))))))

(defun (setf access) (value ic offset mode)
  (with-accessors ((memory intcode-computer-memory)
                   (ip intcode-computer-ip)
                   (rb intcode-computer-rb)) ic
    (ecase mode
      (0 (setf (svref memory (svref memory (+ ip offset))) value))
      (2 (setf (svref memory (+ rb (svref memory (+ ip offset)))) value)))))

(defmacro define-instruction (opcode name lambda-list &body body)
  (let ((ic (gensym "IC"))
        (oc (gensym "OPCODE"))
        (prefix (gensym "PREFIX"))
        (mvars (loop repeat (length lambda-list) collect (gensym "MVAR"))))
    `(progn
       (defun ,name (,ic ,oc ,prefix)
         (declare (intcode-computer ,ic) (ignorable ,oc))
         (destructuring-bind ,mvars (prefix-modes ,prefix ,(length lambda-list))
           (declare (ignorable ,@mvars))
           (symbol-macrolet
               ((rb (intcode-computer-rb ,ic))
                ,@(loop for variable in lambda-list
                        for mvar in mvars
                        for offset from 1
                        collect
                        `(,variable (access ,ic ,offset ,mvar))))
             (flet ((tape (address)
                      (access ,ic address 0))
                    (jump-to (address)
                      (setf (intcode-computer-ip ,ic) address)
                      (return-from ,name)))
               (declare (ignorable #'tape))
               ,@body
               (jump-to (+ (intcode-computer-ip ,ic) ,(1+ (length lambda-list))))))))
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
  (with-accessors ((x aoc:x) (y aoc:y)) *robot*
    (setf dst (aoc:grid-ref *grid* x y))))

(define-instruction 4 output (src)
  (with-accessors ((x aoc:x) (y aoc:y) (mode robot-mode)) *robot*
    (ecase mode
      (:paint
       (setf (aoc:grid-ref *grid* x y) src)
       (setf mode :move))
      (:move
       (ecase src
         (0 (aoc:rotate-left *robot*))
         (1 (aoc:rotate-right *robot*)))
       (aoc:move *robot* 1)
       (setf mode :paint)))))

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

(define-instruction 9 adjust-relative-base (a)
  (incf rb a))

(define-instruction 99 hlt ()
  (throw 'halt (tape 0)))

;;; End of Instructions.

(defclass robot (aoc:mover)
  ((%brain :initarg :brain :reader robot-brain)
   (%mode :initform :paint :accessor robot-mode :type (member :paint :move))))

(defun run-robot (robot)
  (run-intcode-computer (robot-brain robot)))

(defun solve-day-11-part-1 ()
  (let ((*robot* (make-instance 'robot
                   :x 0 :y 0 :direction #\^
                   :brain (make-intcode-computer
                           :program (read-intcode-computer-program "input"))))
        (*grid* (aoc:make-grid :default (constantly 0))))
    (run-robot *robot*)
    (let ((painted-tiles 0))
      (aoc:map-sparse-grid
       (lambda (x y entry)
         (declare (ignore x y entry))
         (incf painted-tiles))
       *grid*)
      painted-tiles)))

(defun solve-day-11-part-2 ()
  (let ((*robot* (make-instance 'robot
                   :x 0 :y 0 :direction #\^
                   :brain (make-intcode-computer
                           :program (read-intcode-computer-program "input"))))
        (*grid* (aoc:make-grid :default (constantly 0))))
    (setf (aoc:grid-ref *grid* 0 0) 1)
    (run-robot *robot*)
    (aoc:print-grid
     *grid*
     (lambda (entry)
       (case entry
         (0 #\space)
         (1 #\#)
         (otherwise #\?))))))
