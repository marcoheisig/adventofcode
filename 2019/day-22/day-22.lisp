(defpackage :adventofcode-2019-day-22
  (:use :cl))

(in-package :adventofcode-2019-day-22)

(defstruct (deck (:constructor %make-deck))
  (size nil :type unsigned-byte)
  (top-card nil :type integer)
  (step 1 :type integer))

(defun make-deck (size)
  (%make-deck :top-card 0 :size size))

(defun deck-cards (deck &key (start 0) (end (deck-size deck)))
  (with-accessors ((top-card deck-top-card)
                   (step deck-step)
                   (size deck-size)) deck
    (let ((cards '())
          (card top-card))
      (setf card (mod (+ card (* step start)) size))
      (loop repeat (- end start) do
        (push card cards)
        (setf card (mod (+ card step) size)))
      (nreverse cards))))

(defmethod print-object ((deck deck) stream)
  (print-unreadable-object (deck stream :type t)
    (if (<= (deck-size deck) 10)
        (format stream "~{~S~^ ~}" (deck-cards deck))
        (format stream "~{~S~^ ~}..." (deck-cards deck :end 10)))))

(defun cut (deck n)
  (prog1 deck
    (with-accessors ((top-card deck-top-card)
                     (step deck-step)
                     (size deck-size)) deck
      (setf top-card (+ top-card (* step n))))))

(defun deal-into-new-stack (deck)
  (prog1 deck
    (cut deck -1)
    (with-accessors ((step deck-step)) deck
      (setf step (- step)))))

(defun step-size (n size)
  (let ((x 0)
        (k 0))
    (loop until (= x 1) do
      (multiple-value-bind (quotient remainder)
          (ceiling (- size x) n)
        (setf x (abs remainder))
        (incf k quotient)))
    k))

(defun deal-with-increment (deck n)
  (prog1 deck
    (with-accessors ((step deck-step)
                     (size deck-size)) deck
      (setf step (* step (step-size n size))))))

(defun read-shuffling-instructions (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect
          (let* ((number-part (1+ (position-if #'alpha-char-p line :from-end t)))
                 (n (parse-integer line :start number-part :junk-allowed t)))
            (cond ((eql (schar line 0) #\c)
                   `(cut ,n))
                  ((eql (schar line 5) #\i)
                   `(deal-into-new-stack ,n))
                  ((eql (schar line 5) #\w)
                   `(deal-with-increment ,n))
                  (t (error "Unknown shuffling instruction:~% ~S" line)))))))

(defun shuffle (deck)
  (dolist (instruction (read-shuffling-instructions "input"))
    (destructuring-bind (fn &optional n) instruction
        (if (integerp n)
            (funcall fn deck n)
            (funcall fn deck))))
  deck)

(defun solve-day-22-part-1 ()
  (position 2019 (deck-cards (shuffle (shuffle (make-deck 10007))))))

;;; For Part 2, we are looking for the value in a certain position, after
;;; shuffling a ridiculously large deck a ridiculous number of times.

(defun reverse-cut (position n-cards amount)
  (mod (+ position amount) n-cards))

(defun reverse-deal-into-new-stack (position n-cards)
  (mod (- n-cards position 1) n-cards))

(defun reverse-deal-with-increment (position n-cards increment)
  (mod (* position (step-size increment n-cards)) n-cards))

(defun reverse-shuffle (position n-cards)
  (dolist (instruction (reverse (read-shuffling-instructions "input")))
    (destructuring-bind (fn &optional n) instruction
      (setf position
            (ecase fn
              (cut
               (reverse-cut position n-cards n))
              (deal-into-new-stack
               (reverse-deal-into-new-stack position n-cards))
              (deal-with-increment
               (reverse-deal-with-increment position n-cards n))))))
  position)

;;; Any number of reverse shuffle operations can be expressed with the
;;; formula (mod (+ (* a postion) b) n).  However, we want to
;;; reverse-shuffle an awful lot of times.  So we need to derive a closed
;;; formula for undoing R steps:
;;;
;;; frs(p, a, b, n, r) := p*a^r + b*sum(a^k, k, 0, r-1);
;;;
;;; frs(p, a, b, n, r) := p*a^r + b*(a^r-1)/(a-1);

(defun exptmod (base power divisor)
  (let ((result 1))
    (do ((power power (floor power 2))
         (base base (mod (* base base) divisor)))
        ((zerop power) (mod result divisor))
      (when (oddp power)
        (setf result (mod (* base result) divisor))))))

(defun fast-reverse-shuffle (p a b n r)
  (let ((a^r (exptmod a r n)))
    (mod (+ (* p a^r)
            (* b (* (- a^r 1) (exptmod (1- a) (- n 2) n))))
         n)))

(defun solve-day-22-part-2 ()
  (let* ((n-cards 119315717514047)
         (repetitions 101741582076661)
         (b (reverse-shuffle 0 n-cards))
         (a (- (reverse-shuffle 1 n-cards) b)))
    (fast-reverse-shuffle 2020 a b n-cards repetitions)))

