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

(defmethod object-initargs (object)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        for initargs = (c2mop:slot-definition-initargs slot)
        for include-p = (and initargs (slot-boundp object name))
        when include-p collect (first initargs)
        when include-p collect (slot-value object name)))

(defmethod serialize-object ((object (eql T)) stream)
  (format stream "T"))

(defmethod serialize-object ((object (eql NIL)) stream)
  (format stream "NIL"))

(defmethod serialize-object ((object symbol) stream)
  (when (keywordp object)
    (write-char #\: stream))
  (write-string (string-downcase object) stream))

(defmethod serialize-object ((object real) stream)
  (write object :stream stream))

(defmethod serialize-object ((object cons) stream)
  (write-char #\( stream)
  (unwind-protect
       (progn (serialize-object (car object) stream)
              (dolist (item (cdr object))
                (write-char #\  stream)
                (serialize-object item stream)))
    (write-char #\) stream)))

(defmethod serialize-object ((object string) stream)
  (write object :stream stream))

(defun %serialize-object (object stream)
  (serialize-object (type-of object) stream)
  (dolist (item (object-initargs object))
    (write-char #\  stream)
    (serialize-object item stream)))

(defmethod serialize-object ((object condition) stream)
  ;; I know this is not portable and I don't care.
  (%serialize-object object stream))

(defmethod serialize-object ((object structure-object) stream)
  ;; I know this is not portable and I don't care.
  (%serialize-object object stream))

(defmethod serialize-object ((object standard-object) stream)
  (%serialize-object object stream))

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

(defmethod process-connections ((server event-server))
  (let ((connections (connections server))
        (listener (listener server)))
    (unwind-protect
         (progn
           (loop while (usocket:wait-for-input listener :timeout 0 :ready-only T)
                 do (push (usocket:socket-accept listener) connections)
                    (eformat "~a connected" (first connections)))
           (dolist (socket connections)
             (handler-case
                 (flet ((send-object (o)
                          (serialize-object o (usocket:socket-stream socket))
                          (terpri (usocket:socket-stream socket))
                          (finish-output (usocket:socket-stream socket))))
                   (loop while (usocket:wait-for-input socket :timeout 0 :ready-only T)
                         do (handler-case
                                (handler-bind ((warning #'send-object))
                                  (mapc #'send-object
                                        (multiple-value-list
                                         (process-event (parse-event-or-lose (usocket:socket-stream socket)) server))))
                              ((or stream-error usocket:socket-error) (e)
                                (ignore-errors (usocket:socket-close socket))
                                (setf connections (remove socket connections))
                                (eformat "~a disconnected: ~a" socket e))
                              (error (e)
                                (eformat "~a error: ~a" socket e)
                                (send-object e)))))
               (error (e)
                 (ignore-errors (usocket:socket-close socket))
                 (setf connections (remove socket connections))
                 (eformat "~a disconnected: ~a" socket e)))))
      (setf (connections server) connections))))
