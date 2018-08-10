#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass status-bar ()
  ((interval :initarg :interval :accessor interval)
   (next-time :initform 0 :accessor next-time)
   (generators :initarg :generators :accessor generators)
   (output :initarg :output :accessor output)
   (input :initarg :input :accessor input)
   (click-pause :initarg :click-pause :accessor click-pause))
  (:default-initargs
   :interval 1
   :click-pause 1
   :generators ()
   :output *standard-output*
   :input *standard-input*))

(defmethod generate ((bar status-bar) event)
  (loop for generator in (generators bar)
        append (generate generator event)))

(defmethod produce-output ((bar status-bar) payload)
  (jonathan:with-output ((output bar))
    (jonathan:%to-json payload))
  (format (output bar) ",~%"))

(defmacro with-input-ready ((stream) &body body)
  (let ((streamg (gensym "STREAM"))
        (char (gensym "CHAR")))
    `(let* ((,streamg ,stream)
            (,char (read-char-no-hang ,streamg)))
       (when ,char
         (unread-char ,char ,streamg)
         ,@body))))

(defmethod process ((bar status-bar))
  ;; Process potentially pending inputs
  (with-input-ready ((input bar))
    (let* ((table (jonathan:parse (input bar) :as :hash-table))
           (event (click-from-json table)))
      (loop for block in (generate bar event)
            do (produce-output bar block)))
    (when (click-pause bar)
      (setf (next-time bar) (+ (get-internal-real-time)
                               (* (click-pause bar) INTERNAL-TIME-UNITS-PER-SECOND)))))
  ;; Process periodic output
  (when (<= (next-time bar) (get-internal-real-time))
    (loop for block in (generate bar (make-instance 'tick))
          do (produce-output bar block))
    (setf (next-time bar) (+ (get-internal-real-time)
                             (* (interval bar) INTERNAL-TIME-UNITS-PER-SECOND)))))

(defun run-bar (bar &key (pause 1/30) (click-events-p T))
  (jonathan:with-output ((output bar))
    (jonathan:%to-json (make-instance 'header :send-click-events-p click-events-p)))
  (format (output bar) "~%[~%")
  (unwind-protect
       (let ((start (get-internal-real-time)))
         (loop (process bar)
               (let* ((end (get-internal-real-time))
                      (loss (/ (- end start) INTERNAL-TIME-UNITS-PER-SECOND)))
                 (setf start end)
                 (sleep (min 0 (- pause loss))))))
    (write-line "]" (output bar))))