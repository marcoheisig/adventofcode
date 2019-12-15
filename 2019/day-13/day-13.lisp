(defpackage :adventofcode-2019-day-13
  (:use :cl))

(in-package :adventofcode-2019-day-13)

(defvar *screen*)

(defvar *score*)

(defstruct (screen (:constructor make-screen ()))
  (grid (aoc:make-grid :default (constantly 0)))
  (mode :x :type (member :x :y :id))
  (ball-x nil)
  (paddle-x nil)
  x y)

(defmacro screen-ref (screen x y)
  `(aoc:grid-ref (screen-grid ,screen) ,x ,y))

(defun draw-screen (screen &optional (stream t))
  (aoc:draw-grid
   (screen-grid screen)
   (lambda (id)
     (ecase id
       (0 #\space)
       (1 #\#)
       (2 #\X)
       (3 #\-)
       (4 #\o)))
   stream))

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
  (print *score*)
  (draw-screen *screen*)
  (with-accessors ((ball-x screen-ball-x)
                   (paddle-x screen-paddle-x)) *screen*
    (cond ((< ball-x paddle-x) (setf dst -1))
          ((= ball-x paddle-x) (setf dst 0))
          ((> ball-x paddle-x) (setf dst 1)))))

(define-instruction 4 output (src)
  (with-accessors ((x screen-x)
                   (y screen-y)
                   (ball-x screen-ball-x)
                   (paddle-x screen-paddle-x)
                   (mode screen-mode)) *screen*
    (ecase mode
      (:x (setf x src mode :y))
      (:y (setf y src mode :id))
      (:id
       (if (and (= x -1) (= y 0))
           (setf *score* src)
           (setf (screen-ref *screen* x y) src))
       (when (= src 3) (setf paddle-x x))
       (when (= src 4) (setf ball-x x))
       (setf mode :x)))))

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

(defun solve-day-13-part-1 ()
  (let ((*screen* (make-screen))
        (ic (make-intcode-computer :program (read-intcode-computer-program "input"))))
    (run-intcode-computer ic)
    (let ((block-tiles 0))
      (aoc:map-sparse-grid
       (lambda (x y id)
         (declare (ignore x y))
         (when (= id 2)
           (incf block-tiles)))
       (screen-grid *screen*))
      block-tiles)))

(defun solve-day-13-part-2 ()
  (let ((*screen* (make-screen))
        (*score* 0)
        (ic (make-intcode-computer :program (read-intcode-computer-program "input"))))
    (setf (aref (intcode-computer-memory ic) 0) 2)
    (run-intcode-computer ic)
    *score*))
