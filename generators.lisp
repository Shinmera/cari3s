#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass generator ()
  ())

(defgeneric generate (generator event))

(defclass single-generator (pango-block)
  ())

(defmethod generate :around ((generator single-generator) event)
  (call-next-method)
  generator)

(defclass value-generator (single-generator)
  ((value :initform NIL :accessor value)))

(defgeneric compute-value (value-generator))

(defmethod text ((generator value-generator))
  (markup-regions (apply #'format NIL (slot-value generator 'text)
                         (value generator))
                  (markup generator)))

(defmethod generate ((generator value-generator) event)
  (setf (value generator) (compute-value generator)))
