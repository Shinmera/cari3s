#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass mem-usage (value-generator)
  ((include-swap :initarg :include-swap :accessor include-swap)
   (include-cache :initarg :include-cache :accessor include-cache))
  (:default-initargs
   :text "MEM ~4,1f%"
   :markup '((0 3 :color #x0088EE))
   :include-swap NIL
   :include-cache NIL))

(defun parse-meminfo ()
  (with-open-file (i "/proc/meminfo")
    (let ((table (make-hash-table :test 'equal)))
      (loop for line = (read-line i NIL)
            while line
            do (let ((pos (position #\: line)))
                 (setf (gethash (subseq line 0 pos) table)
                       (parse-integer line :start (1+ pos) :junk-allowed T))))
      table)))

(defmethod compute-value ((generator mem-usage))
  (let* ((meminfo (parse-meminfo))
         (total (+ (gethash "MemTotal" meminfo)
                   (if (include-swap generator) (gethash "SwapTotal" meminfo) 0)))
         (free (gethash "MemFree" meminfo)
               (gethash "Buffers" meminfo)
               (if (include-cache generator) 0 (gethash "Cached" meminfo))
               (if (include-swap generator)
                   (+ (gethash "SwapFree" meminfo)
                      (if (include-cache generator) 0 (gethash "SwapCached" meminfo)))
                   0)))
    (list (float (* 100 (/ (- total free) total)))
          (float (/ total 1024))
          (float (/ free 1024)))))
