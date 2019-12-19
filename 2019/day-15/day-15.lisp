(defpackage :adventofcode-2019-day-15
  (:use :cl))

(in-package :adventofcode-2019-day-15)

(defvar *output*)
(defvar *commands*)

(defclass tile (aoc:position-mixin)
  ((%oxygenp :initform nil :accessor oxygenp)))

(defclass initial-tile (tile) ())
(defclass unknown-tile (tile) ())
(defclass wall-tile (tile) ())
(defclass empty-tile (tile) ())
(defclass oxygen-system-tile (tile) ())

(defgeneric tile-char (tile)
  (:method ((tile initial-tile)) #\R)
  (:method ((tile unknown-tile)) #\space)
  (:method ((tile wall-tile)) #\#)
  (:method ((tile empty-tile)) #\.)
  (:method ((tile oxygen-system-tile)) #\o)
  (:method :around ((tile tile))
    (if (oxygenp tile)
        #\O
        (call-next-method))))

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
  (if (null *commands*)
      (throw 'halt *output*)
      (setf dst (pop *commands*))))

(define-instruction 4 output (src)
  (setf *output* src))

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

(defun final-position (commands)
  (let ((x 0) (y 0))
    (dolist (command commands)
      (ecase command
        (1 (decf y 1))
        (2 (incf y 1))
        (3 (decf x 1))
        (4 (incf x 1))))
    (values x y)))

(defun explore-tile (program commands)
  (let ((*commands* (reverse commands))
        (*output* nil))
    (run-intcode-computer
     (make-intcode-computer :program program))))

(defun explore-grid (program)
  (let ((grid (aoc:make-grid :default (lambda (x y) (make-instance 'unknown-tile :x x :y y))))
        (worklist '((1) (2) (3) (4)))
        (new-worklist '()))
    (prog1 grid
      (change-class (aoc:grid-ref grid 0 0) 'initial-tile)
      (loop while worklist do
        (loop for commands in worklist do
          (multiple-value-bind (x y) (final-position commands)
            (let ((tile (aoc:grid-ref grid x y)))
              (when (typep tile 'unknown-tile)
                (ecase (explore-tile program commands)
                  ;; Wall.
                  (0 (change-class tile 'wall-tile))
                  ;; Empty.
                  (1 (change-class tile 'empty-tile)
                   (loop for command in '(1 2 3 4) do
                     (push (cons command commands) new-worklist)))
                  ;; Oxygen.
                  (2 (change-class tile 'oxygen-system-tile)
                   (format t "~&Oxygen found within ~D steps.~%" (length commands))))))))
        (shiftf worklist new-worklist '())))))

(defun solve-day-15-part-1 ()
  (explore-grid (read-intcode-computer-program "input")))

(defun find-oxygen-system-tile (grid)
  (aoc:map-sparse-grid
   (lambda (x y tile)
     (declare (ignore x y))
     (when (typep tile 'oxygen-system-tile)
       (return-from find-oxygen-system-tile tile)))
   grid))

(defun tile-neighbors (grid tile)
  (with-accessors ((x aoc:x) (y aoc:y)) tile
    (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))
          collect (aoc:grid-ref grid (+ x dx) (+ y dy)))))

(define-modify-macro appendf (&rest more-lists) append)

(defun solve-day-15-part-2 ()
  (let* ((grid (explore-grid (read-intcode-computer-program "input")))
         (oxygen-system-tile (find-oxygen-system-tile grid)))
    (setf (oxygenp oxygen-system-tile) t)
    (let ((worklist (tile-neighbors grid oxygen-system-tile))
          (new-worklist '())
          (minute 0))
      (loop while worklist do
        (format t "~&Minute ~2D~%" minute)
        (aoc:print-grid grid #'tile-char)
        (loop for tile in worklist do
          (unless (typep tile '(or wall-tile unknown-tile))
            (unless (oxygenp tile)
              (setf (oxygenp tile) t)
              (appendf new-worklist (tile-neighbors grid tile)))))
        (shiftf worklist new-worklist '())
        (incf minute)))))
