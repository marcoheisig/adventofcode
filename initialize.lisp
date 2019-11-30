(in-package :cl-user)

(defun ensure-adventofcode-directory (year day)
  (let* ((package (intern (format nil "ADVENTOFCODE-~S-DAY-~D" year day) :keyword))
         (dir (pathname (format nil "./~D/day-~2,'0D/" year day)))
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

(defun ensure-adventofcode-directories (year)
  (assert (<= 2000 year 3000))
  (loop for day from 1 to 25 do
    (ensure-adventofcode-directory year day)))
