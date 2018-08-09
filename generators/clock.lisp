#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass clock (value-generator)
  ()
  (:default-initargs
   :label "DATE"))

(defmethod compute-value ((generator clock))
  (multiple-value-bind (s m h dd mm yy day) (decode-universal-time (get-universal-time))
    (declare (ignore yy))
    (format NIL "~a ~2d ~a ~2d:~2,'0d:~2,'0d"
            (day-short-name day) dd (month-short-name mm) h m s)))
