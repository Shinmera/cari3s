(in-package #:org.shirakumo.cari3s)

(defclass status-bar (event-server)
  ((interval :initarg :interval :accessor interval)
   (next-time :initform 0 :accessor next-time)
   (generators :initarg :generators :accessor generators)
   (blocks :initform (make-hash-table :test 'eq) :accessor blocks)
   (output :initarg :output :accessor output)
   (input :initarg :input :accessor input)
   (click-pause :initarg :click-pause :accessor click-pause))
  (:default-initargs
   :interval 1
   :click-pause 1
   :generators ()
   :output *standard-output*
   :input *standard-input*))

(defmethod process-event ((event event) (bar status-bar))
  (values-list
   (loop for generator in (generators bar)
         for response = (process-event event generator)
         when response collect response)))

(defmethod process-event ((event echo) (bar status-bar))
  event)

(defmethod process-event ((event generate) (bar status-bar))
  (dolist (generator (generators bar))
    (setf (last-generation generator) most-negative-fixnum))
  (produce-output bar (generate bar)))

(defmethod generate ((bar status-bar))
  (loop for generator in (generators bar)
        do (when (<= (* (interval generator) INTERNAL-TIME-UNITS-PER-SECOND)
                     (- (get-internal-real-time)
                        (last-generation generator)))
             (handler-case
                 (setf (gethash generator (blocks bar)) (generate generator))
               ((or error usocket:ns-try-again-condition) (e)
                 (eformat "~a failed to generate: ~a" generator e))))
        append (gethash generator (blocks bar))))

(defmethod produce-output ((bar status-bar) payload)
  (com.inuoe.jzon:stringify (map 'vector #'to-table payload) :stream (output bar))
  (format (output bar) ",~%"))

(defmacro with-input-ready ((stream) &body body)
  (let ((streamg (gensym "STREAM"))
        (char (gensym "CHAR")))
    `(let* ((,streamg ,stream)
            (,char (read-char-no-hang ,streamg)))
       (when ,char
         (unread-char ,char ,streamg)
         ,@body))))

(defmethod process-inputs ((bar status-bar))
  (with-input-ready ((input bar))
    (let* ((table (com.inuoe.jzon:parse (input bar)))
           (event (from-table 'click table)))
      (process-event event bar))
    (when (click-pause bar)
      (setf (next-time bar) (+ (get-internal-real-time)
                               (* (click-pause bar) INTERNAL-TIME-UNITS-PER-SECOND))))))

(defmethod process ((bar status-bar))
  (process-connections bar)
  (process-inputs bar)
  ;; Process periodic output
  (when (<= (next-time bar) (get-internal-real-time))
    (produce-output bar (generate bar))
    (setf (next-time bar) (+ (get-internal-real-time)
                             (* (interval bar) INTERNAL-TIME-UNITS-PER-SECOND)))))

(defun run-bar (bar &key (pause 1/30) (click-events-p NIL))
  (com.inuoe.jzon:stringify (to-table (make-instance 'header :send-click-events-p click-events-p)) :stream (output bar))
  (format (output bar) "~%[~%")
  (start bar)
  (unwind-protect
       (let ((start (get-internal-real-time)))
         (loop (process bar)
               (let* ((end (get-internal-real-time))
                      (loss (/ (- end start) INTERNAL-TIME-UNITS-PER-SECOND)))
                 (setf start end)
                 (sleep (max 0 (- pause loss))))))
    (stop bar)
    (write-line "]" (output bar))))
