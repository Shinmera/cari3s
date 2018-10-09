#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass event ()
  ())

(defclass generate (event)
  ())

(defclass echo (event)
  ((message :initarg :message :reader message)))

(defclass click (event)
  ((name :initarg :name :reader name)
   (instance :initarg :instance :reader instance)
   (button :initarg :button :reader button)
   (location :initarg :location :reader location)
   (relative-location :initarg :relative-location :reader relative-location)
   (block-size :initarg :block-size :reader block-size))
  (:default-initargs
   :name NIL
   :instance NIL
   :button 1
   :location NIL
   :relative-location NIL
   :block-size NIL))

(defmethod from-table ((type (eql 'click)) table)
  (flet ((k (name) (gethash name table)))
    (make-instance type :name (k "name")
                        :instance (k "instance")
                        :button (k "button")
                        :location (cons (k "x") (k "y"))
                        :relative-location (cons (k "relative_x") (k "relative_y"))
                        :block-size (cons (k "width") (k "height")))))

(defgeneric process-event (event processor))

;;; FIXME: Implement custom validation in order to emit specific, helpful errors.

(defmethod parse-event-or-lose ((stream stream))
  (parse-event-or-lose (read-line stream)))

(defmethod parse-event-or-lose ((string string))
  (let* ((*package* #.*package*)
         (init (loop for start = 0 then next-start
                    for (token next-start) = (multiple-value-list (read-from-string string NIL NIL :start start))
                    while token collect token)))
    (destructuring-bind (class &rest initargs &key &allow-other-keys) init
      (apply #'make-instance class initargs))))

(defclass event-server ()
  ((port :initarg :port :reader port)
   (listener :initform NIL :accessor listener)
   (connections :initform NIL :accessor connections))
  (:default-initargs :port 2424))

(defmethod start ((server event-server))
  (when (listener server)
    (error "Server already started."))
  (setf (listener server) (usocket:socket-listen "localhost" (port server) :reuse-address T)))

(defmethod stop ((server event-server))
  (dolist (socket (connections server))
    (ignore-errors (usocket:socket-close socket)))
  (ignore-errors (usocket:socket-close (listener server)))
  (setf (connections server) ())
  (setf (listener server) ()))

(defmethod object-initargs (object)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        for initargs = (c2mop:slot-definition-initargs slot)
        for include-p = (and initargs (slot-boundp object name))
        when include-p collect (first initargs)
        when include-p collect (slot-value object name)))

(defmethod serialize-object ((object (eql T)) stream)
  (format stream "T"))

(defmethod serialize-object ((object condition) stream)
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~s~{ ~s~}" (type-of object) (object-initargs object))))

(defmethod serialize-object ((object standard-object) stream)
  (let ((*print-case* :downcase)
        (*package* #.*package*))
    (format stream "~s~{ ~s~}" (type-of object) (object-initargs object))))

(defmethod process-connections ((server event-server))
  (let ((connections (connections server))
        (listener (listener server)))
    (when (usocket:wait-for-input listener :timeout 0 :ready-only T)
      (push (usocket:socket-accept listener) connections)
      (format *error-output* "~&~a connected~%" (first connections)))
    (dolist (socket connections)
      (flet ((send-object (o)
               (serialize-object o (usocket:socket-stream socket))
               (terpri (usocket:socket-stream socket))
               (finish-output (usocket:socket-stream socket))))
        (handler-case
            (when (usocket:wait-for-input socket :timeout 0 :ready-only T)
              (handler-bind ((warning #'send-object))
                (send-object (process-event (parse-event-or-lose (usocket:socket-stream socket)) server))))
          ((or stream-error usocket:socket-error) (e)
            (ignore-errors (usocket:socket-close socket))
            (setf connections (remove socket connections))
            (format *error-output* "~&~a disconnected: ~a~%" socket e))
          (error (e)
            (format *error-output* "~&~a error: ~a~%" socket e)
            (send-object e)))))
    (setf (connections server) connections)))
