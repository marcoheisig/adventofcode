(defpackage :adventofcode-2019-day-17
  (:use :cl))

(in-package :adventofcode-2019-day-17)

(defvar *ascii-codes*)

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
  (setf dst (pop *ascii-codes*)))

(define-instruction 4 output (src)
  (if (< src 256)
      (write-char (code-char src))
      (write src)))

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

(defun read-grid ()
  (let ((ascii (with-output-to-string (*standard-output*)
                 (run-intcode-computer
                  (make-intcode-computer
                   :program (read-intcode-computer-program "input"))))))
    (with-input-from-string (stream ascii)
      (aoc:read-grid
       stream
       (lambda (x y char)
         (declare (ignore x y))
         char)
       (constantly #\space)))))

(defun solve-day-17-part-1 ()
  (let ((grid (read-grid))
        (alignment-parameters '()))
    (aoc:print-grid grid #'identity)
    (aoc:map-sparse-grid
     (lambda (x y char)
       (when (and (char= char #\#)
                  (loop for (dx . dy) in '((1 . 0) (0 . 1) (-1 . 0) (0 . -1))
                        always (char= (aoc:grid-ref grid (+ x dx) (+ y dy)) #\#)))
         (push (* x y) alignment-parameters)))
     grid)
    (reduce #'+ alignment-parameters)))

(defclass robot (aoc:mover) ())

(defun find-robot (grid)
  (aoc:map-sparse-grid
   (lambda (x y char)
     (when (member char '(#\< #\^ #\> #\v))
       (return-from find-robot
         (make-instance 'robot :x x :y y :direction char))))
   grid))

(defun plan-movements (grid)
  (let ((robot (find-robot grid))
        (movements '()))
    (loop do
      (cond ((eql (aoc:peek-forward robot grid) #\#)
             (if (integerp (first movements))
                 (incf (first movements))
                 (push 1 movements))
             (aoc:move robot))
            ((eql (aoc:peek-left robot grid) #\#)
             (push #\L movements)
             (aoc:rotate-left robot))
            ((eql (aoc:peek-right robot grid) #\#)
             (push #\R movements)
             (aoc:rotate-right robot))
            (t (loop-finish))))
    (nreverse movements)))

(defun ascii-codes (movements)
  (let* ((vector (coerce movements 'vector))
         (n (length vector)))
    (labels ((char-pattern (char patterns)
               (destructuring-bind (char start end)
                   (find char patterns :key #'first)
                 (declare (ignore char))
                 (coerce (subseq vector start end) 'list)))
             (apply-patterns (index main patterns)
               (tagbody retry
                  (when (= index n)
                    (return-from ascii-codes
                      (map 'list #'char-code
                           (format nil "~{~{~A~^,~}~%~}n~%"
                                   (list (reverse main)
                                         (char-pattern #\A patterns)
                                         (char-pattern #\B patterns)
                                         (char-pattern #\C patterns))))))
                  (loop for (char start end) in patterns do
                    (let ((size (- end start)))
                      (when (<= (+ index size) n)
                        (unless (mismatch vector vector
                                          :start1 start :end1 end
                                          :start2 index :end2 (+ index size))
                          (push char main)
                          (incf index size)
                          (go retry))))))
               (values index main)))
      (loop for a-size from 2 to 10 by 2 do
        (let ((a-pattern (list #\A 0 a-size)))
          (multiple-value-bind (b-start main)
              (apply-patterns 0 '() (list a-pattern))
            (loop for b-size from 2 to 10 by 2 do
              (let ((b-pattern (list #\B b-start (+ b-start b-size))))
                (multiple-value-bind (c-start main)
                    (apply-patterns b-start main (list a-pattern b-pattern))
                  (loop for c-size from 2 to 10 by 2 do
                    (let ((c-pattern (list #\C c-start (+ c-start c-size))))
                      (apply-patterns c-start main (list a-pattern b-pattern c-pattern)))))))))))))

(defun solve-day-17-part-2 ()
  (let ((*ascii-codes* (ascii-codes (plan-movements (read-grid))))
        (program (read-intcode-computer-program "input")))
    (setf (svref program 0) 2)
    (run-intcode-computer
     (make-intcode-computer :program program))))
