(defpackage :adventofcode-2018-day-5
  (:use :cl))

(in-package :adventofcode-2018-day-5)

(defun opposite-case-p (char-1 char-2)
  (and (char-equal char-1 char-2)
       (not (char= char-1 char-2))))

(defun reduce-string (string)
  (with-input-from-string (stream string)
    (let ((stack '()))
      (loop for char = (read-char stream nil nil)
            while char do
              (if (and (not (null stack))
                       (opposite-case-p (car stack) char))
                  (pop stack)
                  (push char stack)))
      (coerce (nreverse stack) 'string))))

(defun read-file-into-string (file)
  (with-open-file (input file :direction :input)
    (values
     (read-line input))))

(defun solve-problem-1 ()
  (length
   (reduce-string
    (read-file-into-string "input"))))

(defun solve-problem-2 ()
  (let ((string (read-file-into-string "input"))
        (letters "abcdefghijklmnopqrstuvwxyz"))
    (loop for letter across letters
          minimize (length
                    (reduce-string
                     (remove letter string :test #'char-equal))))))
