(defpackage :adventofcode-2019-day-19
  (:use :cl))

(in-package :adventofcode-2019-day-19)

(defvar *coordinates*)
(defvar *drone-x*)
(defvar *drone-y*)
(defvar *grid*)
(defvar *new-tiles*)

;;; The intcode computer.

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
  (cond ((null *drone-x*)
         (setf *drone-x* (pop *coordinates*))
         (setf dst *drone-x*))
        ((null *drone-y*)
         (setf *drone-y* (pop *coordinates*))
         (setf dst *drone-y*))
        (t (error "Inconsistent drone state."))))

(define-instruction 4 output (src)
  (setf (aoc:grid-ref *grid* *drone-x* *drone-y*) src)
  (push (list src *drone-x* *drone-y*) *new-tiles*)
  (setf *drone-x* nil)
  (setf *drone-y* nil))

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

(defun element-char (element)
  (case element
    (0 #\.)
    (1 #\#)
    (2 #\O)
    (otherwise #\space)))

(defun unveil-tile (x y program)
  (cond ((minusp x) 0)
        ((minusp y) 0)
        ((aoc:grid-ref *grid* x y))
        (t
         (let ((*coordinates* (list x y)))
           (run-intcode-computer
            (make-intcode-computer :program program)))
         (aoc:grid-ref *grid* x y))))

(defun solve-day-19-part-1 ()
  (let* ((program (read-intcode-computer-program "input"))
         (*grid* (aoc:make-grid))
         (*drone-x* nil)
         (*drone-y* nil)
         (*new-tiles* '()))
    (loop for y below 50 do
      (loop for x below 50 do
        (unveil-tile x y program)))
    (aoc:print-grid *grid* #'element-char)
    (let ((count 0))
      (aoc:map-sparse-grid
       (lambda (x y element)
         (declare (ignore x y))
         (incf count element))
       *grid*)
      count)))

(defun distance (x y)
  (sqrt (+ (expt x 2) (expt y 2))))

(defun solve-day-19-part-2 ()
  (let* ((program (read-intcode-computer-program "input"))
         (*grid* (aoc:make-grid))
         (*drone-x* nil)
         (*drone-y* nil)
         (*new-tiles* '())
         (point nil)
         (min-distance most-positive-fixnum))
    (loop for y below 10 do
      (loop for x below 10 do
        (unveil-tile x y program)))
    (loop for worklist = *new-tiles* until (null worklist) do
      (setf *new-tiles* '())
      (loop for (id x y) in worklist when (eql id 1) do
        (let ((distance (distance x y)))
          (when (< distance min-distance)
            ;; Check whether the new tile fulfills the criterion.
            (when (and (eql 1 (unveil-tile (+ x 99) y program))
                       (eql 1 (unveil-tile x (+ y 99) program)))
              (setf min-distance distance)
              (setf point (list x y)))
            ;; Scan neighbors.
            (loop for (dx . dy) in '((0 . 1) (1 . 0) (-1 . 0) (0 . -1)) do
              (unveil-tile (+ x dx) (+ y dy) program))))))
    (destructuring-bind (x y) point
      (loop for dy below 100 do
        (loop for dx below 100 do
          (assert (eql 1 (unveil-tile (+ x dx) (+ y dy) program)))
          (setf (aoc:grid-ref *grid* (+ x dx) (+ y dy)) 2)))
      #+(or)
      (with-open-file (stream "debug" :direction :output)
        (aoc:print-grid *grid* #'element-char stream))
      (values x y (+ (* 10000 x) y)))))
