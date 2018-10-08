#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass generator ()
  ((last-generation :initform 0 :accessor last-generation)
   (interval :initarg :interval :accessor interval))
  (:default-initargs :interval 1))

(defgeneric generate (generator))

(defmethod process-event ((event event) (generator generator)))

(defmethod generate :after ((generator generator))
  (setf (last-generation generator) (get-internal-real-time)))

(defclass single-generator (generator pango-block)
  ())

(defmethod initialize-instance :after ((generator single-generator) &key name)
  (unless name (setf (name generator) (string-downcase (class-name (class-of generator))))))

(defmethod generate :around ((generator single-generator))
  (call-next-method)
  (list generator))

(defclass value-generator (single-generator)
  ((value :initform NIL :accessor value)))

(defgeneric compute-value (value-generator))

(defmethod text ((generator value-generator))
  (pango-markup:markup-regions
   (apply #'format NIL (slot-value generator 'text) (value generator))
   (markup generator)))

(defmethod short-text ((generator value-generator))
  (let ((text (slot-value generator 'short-text)))
    (when text
      (pango-markup:markup-regions
       (apply #'format NIL text (value generator))
       (short-markup generator)))))

(defmethod generate ((generator value-generator))
  (setf (value generator) (compute-value generator)))
