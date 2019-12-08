(defpackage :adventofcode-2019-day-8
  (:use :cl))

(in-package :adventofcode-2019-day-8)

(defun read-image (filename width height)
  (with-open-file (stream filename :direction :input)
    (let* ((pixels/layer (* width height))
           (pixels
             (loop for char = (read-char stream nil nil) while char
                   collect (- (char-code char) (char-code #\0))))
           (n-layers (floor (length pixels) pixels/layer)))
      (loop repeat n-layers
            for list = pixels then (nthcdr pixels/layer list)
            collect (subseq list 0 pixels/layer)))))

(defun solve-day-08-part-1 (&aux (w 25) (h 6))
  (loop for layer in (read-image "input" w h) do
    (format t "~&~3D ~3D~%"
            (count 0 layer)
            (* (count 1 layer) (count 2 layer)))))

(defun solve-day-08-part-2 (&aux (w 25) (h 6))
  (flet ((decode-pixels (&rest pixels)
           (ecase (find 2 pixels :test-not #'=)
             (0 #\space)
             (1 #\#))))
    (let ((decoded-pixels
            (apply #'mapcar #'decode-pixels (read-image "input" w h))))
      (loop for row below h do
        (format t "~&~{~A~}~%" (subseq decoded-pixels (* row w) (* (1+ row) w)))))))
