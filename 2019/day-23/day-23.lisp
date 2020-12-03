(defpackage :adventofcode-2019-day-23
  (:use :cl :intcode-computer))

(in-package :adventofcode-2019-day-23)

(defun solve-day-23-part-1 ()
  (let* ((n 50)
         (program (read-intcode-computer-program "input"))
         (queues (make-array n))
         (threads (make-array n)))
    (loop for index below n do
      (setf (aref queues index)
            (lparallel.queue:make-queue)))
    (loop for index below n do
      (let ((queue (aref queues index))
            send-d send-x send-y (recv-d index) recv-x recv-y)
        (setf (aref threads index)
              (bt:make-thread
               (lambda ()
                 (run-intcode-computer
                  (make-intcode-computer
                   :program program
                   :input
                   (lambda ()
                     (cond (recv-d (shiftf recv-d nil))
                           (recv-x (shiftf recv-x nil))
                           (recv-y (shiftf recv-y nil))
                           ((let ((value (lparallel.queue:try-pop-queue queue)))
                              (if value
                                  (prog1 (first value)
                                    (setf recv-y (second value)))
                                  -1)))))
                   :output
                   (lambda (value)
                     (tagbody retry
                        (cond ((null send-d) (setf send-d value))
                              ((null send-x) (setf send-x value))
                              ((null send-y) (setf send-y value))
                              (t (lparallel.queue:push-queue
                                  (list send-x send-y)
                                  (aref queues send-d))
                                 (print (list send-d send-y send-x))
                                 (setf send-d nil send-x nil send-y nil)
                                 (go retry))))))))
               :name (format nil "NIC-~D" index)))))))
