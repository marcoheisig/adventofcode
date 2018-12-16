(defpackage :adventofcode-2018-day-16
  (:use :cl))

(in-package :adventofcode-2018-day-16)

(defvar *registers* '(reg0 reg1 reg2 reg3))

(defvar *ops* '())

(defmacro with-bindings ((name type) &body body)
  (ecase type
    (val `(progn ,@body))
    (ignore `(let ((,name ,name))
               (declare (ignore ,name))
               ,@body))
    (reg `(ecase ,name
            ,@(loop for register in *registers*
                    for n from 0
                    collect `(,n (symbol-macrolet ((,name ,register)) ,@body)))))))

(defmacro defop (name arguments &body body)
  (destructuring-bind ((asym atype) (bsym btype)) arguments
    (alexandria:with-gensyms (csym)
      `(progn
         (defun ,name (,asym ,bsym ,csym)
           (with-bindings (,asym ,atype)
             (with-bindings (,bsym ,btype)
               (with-bindings (,csym reg)
                 (lambda ,*registers*
                   (declare (integer ,@*registers*))
                   (setf ,csym (progn ,@body))
                   (values ,@*registers*))))))
         (pushnew ',name *ops*)))))

(defop addr ((a reg) (b reg)) (+ a b))
(defop addi ((a reg) (b val)) (+ a b))

(defop mulr ((a reg) (b reg)) (* a b))
(defop muli ((a reg) (b val)) (* a b))

(defop banr ((a reg) (b reg)) (logand a b))
(defop bani ((a reg) (b val)) (logand a b))

(defop borr ((a reg) (b reg)) (logior a b))
(defop bori ((a reg) (b val)) (logior a b))

(defop setr ((a reg) (b ignore)) a)
(defop seti ((a val) (b ignore)) a)

(defop gtir ((a val) (b reg)) (if (> a b) 1 0))
(defop gtri ((a reg) (b val)) (if (> a b) 1 0))
(defop gtrr ((a reg) (b reg)) (if (> a b) 1 0))

(defop eqir ((a val) (b reg)) (if (= a b) 1 0))
(defop eqri ((a reg) (b val)) (if (= a b) 1 0))
(defop eqrr ((a reg) (b reg)) (if (= a b) 1 0))

;; Read all integers in STRING, ignoring all garbage.
(defun all-integers (string)
  (labels ((recurse (pos acc)
             (let ((start (position-if #'digit-char-p string :start pos)))
               (if (null start)
                   (nreverse acc)
                   (multiple-value-bind (integer end)
                       (parse-integer string :start start :junk-allowed t)
                     (recurse end (cons integer acc)))))))
    (recurse 0 '())))

(defun matching-opcode-p (op args before after)
  (destructuring-bind (b0 b1 b2 b3) before
    (destructuring-bind (a0 a1 a2 a3) after
      (destructuring-bind (a b c) args
        (multiple-value-bind (r0 r1 r2 r3)
            (funcall (funcall op a b c) b0 b1 b2 b3)
          (and (eql r0 a0)
               (eql r1 a1)
               (eql r2 a2)
               (eql r3 a3)))))))

(defun read-observations (file)
  (with-open-file (stream file :direction :input)
    (let ((lines (loop for line = (read-line stream nil nil)
                       while line
                       collect line)))
      (loop for (before opcode after empty-line . rest) on lines by #'cddddr
            collect (list (all-integers before)
                          (all-integers opcode)
                          (all-integers after))))))

(defun solve-exercise-1 ()
  (loop for (before opcode after) in (read-observations "input1")
        count
        (<= 3 (loop for op in *ops*
                    count (matching-opcode-p op (rest opcode) before after)))))

(defun position-of-smallest-list (sequence)
  (loop for n from 1 thereis (position n sequence :key #'length)))

(defun backtrack (vector-of-alternatives solutions)
  (cond ((notany #'null solutions)
         solutions)
        ((some (lambda (a s) (and (null a) (null s)))
               vector-of-alternatives solutions)
         nil)
        (t
         (let ((index (position-of-smallest-list vector-of-alternatives)))
           (loop for alternative in (svref vector-of-alternatives index)
                   thereis
                   (backtrack
                    (map 'vector
                         (lambda (x) (remove alternative x))
                         vector-of-alternatives)
                    (let ((copy (copy-seq solutions)))
                      (setf (svref copy index) alternative)
                      copy)))))))

(defun uniquify (vector-of-alternatives)
  (backtrack
   vector-of-alternatives
   (make-array (length vector-of-alternatives)
               :initial-element nil)))

(defun solve-exercise-2 ()
  (let ((opcodes (make-array 16 :initial-element *ops*)))
    ;; Determine the set of possible ops of each opcode.
    (loop for (before (opcode . arguments) after) in (read-observations "input1") do
      (setf (aref opcodes opcode)
            (remove-if-not
             (lambda (op) (matching-opcode-p op arguments before after))
             (aref opcodes opcode))))
    ;; Now narrow the sets to one op per opcode.
    (setf opcodes (uniquify opcodes))
    ;; Now execute the final program.
    (with-open-file (stream "input2" :direction :input)
      (let ((r0 0) (r1 0) (r2 0) (r3 0))
        (loop for line = (read-line stream nil nil)
              while line
              do (destructuring-bind (opcode a b c) (all-integers line)
                   (multiple-value-setq (r0 r1 r2 r3)
                     (funcall
                      (funcall (svref opcodes opcode) a b c)
                      r0 r1 r2 r3))))
        r0))))
