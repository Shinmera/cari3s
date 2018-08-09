#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass single-generator (block)
  ())

(defmethod generate :around ((generator single-generator) event)
  (call-next-method)
  generator)

(defclass value-generator (single-generator)
  ((label :initarg :label :accessor label)
   (value-format :initarg :value-format :accessor value-format))
  (:default-initargs
   :text NIL
   :value-format "~a"))

(defgeneric compute-value (value-generator))

(defmethod generate ((generator value-generator) event)
  (setf (text generator) (format NIL "~@[~a ~]~@?"
                                 (label generator)
                                 (value-format generator)
                                 (compute-value generator))))
