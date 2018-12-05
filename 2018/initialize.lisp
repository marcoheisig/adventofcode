(in-package :cl-user)

(defun ensure-adventofcode-directory (day)
  (let* ((package (intern (format nil "ADVENTOFCODE-2018-DAY-~D" day) :keyword))
         (dir (pathname (format nil "./day-~2,'0D/" day)))
         (file (make-pathname :directory (pathname-directory dir)
                              :name (format nil "day-~2,'0D" day)
                              :type "lisp")))
    (ensure-directories-exist dir)
    (unless (probe-file file)
      (with-open-file (stream file :direction :output
                                   :if-exists nil
                                   :if-does-not-exist :create)
        (with-standard-io-syntax
          (let ((*print-case* :downcase))
            (format stream "(defpackage ~S~%  (:use :cl))~%~%(in-package ~S)~%~%"
                    package package)))))))

(defun ensure-adventofcode-directories ()
  (loop for n from 1 to 24 do (ensure-adventofcode-directory n)))
