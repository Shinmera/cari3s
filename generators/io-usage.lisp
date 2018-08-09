#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass io-usage (value-generator)
  ((device :initarg :device :accessor device)
   (direction :initarg :direction :accessor direction)
   (previous-time :initform (get-internal-real-time) :accessor previous-time)
   (previous-value :initform 0 :accessor previous-value))
  (:default-initargs
   :label "I/O"
   :value-format "~4,1fm/s"
   :device T
   :direction :read-write))

(defmethod compute-value ((generator io-usage))
  (let ((lines (with-open-file (i "/proc/diskstats")
                 (loop for line = (read-line i NIL)
                       for name-end = (position #\  line :start 13)
                       while line
                       collect (list* (subseq line 13 name-end)
                                      (parse-all-integers line :start (1+ name-end)))))))
    (unless (eql T (device generator))
      (setf lines (remove (device generator) lines :key #'first :test #'string-equal)))
    (let* ((value (/ (* 512
                        (loop for device in lines
                              sum (ecase (direction generator)
                                    (:read (nth 6 device))
                                    (:write (nth 10 device))
                                    (:read-write (+ (nth 6 device) (nth 10 device))))))
                     1024 1024))
           (diff (- value (previous-value generator)))
           (tdiff (/ (- (get-internal-real-time) (previous-time generator))
                     INTERNAL-TIME-UNITS-PER-SECOND)))
      (setf (previous-value generator) value)
      (setf (previous-time generator) (get-internal-real-time))
      (if (= 0 tdiff) 0.0 (float (/ diff tdiff))))))
