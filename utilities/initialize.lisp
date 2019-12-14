(in-package :adventofcode)

(defun ensure-adventofcode-directories (pathname year)
  (assert (<= 2000 year 3000))
  (when (yes-or-no-p "~&Create adventofcode directories for ~D in ~A?"
                     year pathname)
    (ensure-year
     (merge-pathnames
      (make-pathname :directory `(:relative ,(format nil "~D" year)))
      pathname)
     year)))

(defun ensure-year (directory year)
  (let* ((system (intern (format nil "ADVENTOFCODE-~D" year) :keyword))
         (days (loop for day from 1 to 25 collect day))
         (files (loop for day in days collect (format nil "day-~2,'0D" day))))
    (ensure-directories-exist directory)
    (ensure-asd-file
     (make-pathname
      :directory (pathname-directory directory)
      :name (string-downcase system)
      :type "asd")
     system
     files)
    (loop for day in days and file in files do
      (ensure-day
       (merge-pathnames
        (make-pathname :directory `(:relative ,file))
        directory)
       year
       day))))

(defun ensure-asd-file (pathname system files)
  (with-open-file (stream pathname :direction :output
                                   :if-exists nil
                                   :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-case* :downcase)
            (*package* (find-package :asdf-user))
            (*print-right-margin* 70)
            (*print-readably* nil))
        (pprint
         `(asdf:defsystem ,system
            :depends-on ("adventofcode")
            :serial t
            :components
            ,(loop for file in files
                   collect
                   `(:module ,file :components ((:file ,file)))))
         stream)))))

(defun ensure-day (directory year day)
  (let* ((package (intern (format nil "ADVENTOFCODE-~S-DAY-~D" year day) :keyword))
         (file (make-pathname :directory (pathname-directory directory)
                              :name (format nil "day-~2,'0D" day)
                              :type "lisp")))
    (ensure-directories-exist directory)
    (with-open-file (stream file :direction :output
                                 :if-exists nil
                                 :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-case* :downcase))
          (format stream "(defpackage ~S~%  (:use :cl))~%~%(in-package ~S)~%~%"
                  package package))))))
