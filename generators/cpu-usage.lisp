#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass cpu-usage (value-generator)
  ((previous-time :initform 0 :accessor previous-time)
   (previous-idle :initform 0 :accessor previous-idle))
  (:default-initargs
   :text "CPU ~4,1f%"
   :markup '((0 3 :color #x0088EE))))

(defmethod compute-value ((generator cpu-usage))
  (let* ((line (with-open-file (i "/proc/stat") (read-line i)))
         (parts (parse-all-integers line :start 4))
         (current-time (reduce #'+ parts))
         (current-idle (+ (nth 3 parts) (nth 4 parts)))
         (diff-time (- current-time (previous-time generator)))
         (diff-idle (- current-idle (previous-idle generator))))
    (setf (previous-time generator) current-time)
    (setf (previous-idle generator) current-idle)
    (list
     (float (/ (+ 5 (* 1000 (/ (- diff-time diff-idle) diff-time)))
               10)))))
