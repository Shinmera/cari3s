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
        collect (generate generator event)))

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
      (produce-output bar (generate bar event)))
    (setf (next-time bar) (+ (get-internal-real-time)
                             (* (click-pause bar) INTERNAL-TIME-UNITS-PER-SECOND))))
  ;; Process periodic output
  (when (<= (next-time bar) (get-internal-real-time))
    (produce-output bar (generate bar (make-instance 'ping)))
    (setf (next-time bar) (+ (get-internal-real-time)
                             (* (interval bar) INTERNAL-TIME-UNITS-PER-SECOND)))))

(defun run-bar (bar &key (pause 1/30))
  (jonathan:with-output ((output bar))
    (jonathan:%to-json (make-instance 'header :send-click-events-p T)))
  (format (output bar) "~%[~%")
  (unwind-protect
       (loop (process bar)
             (sleep pause))
    (write-line "]" (output bar))))

(defun load-from-file (file)
  (with-open-file (i file)
    (let ((*package* #.*package*)
          (initargs ())
          (generators ()))
      (loop with eof = (make-symbol "EOF")
            for item = (read i NIL eof)
            until (eql item eof)
            do (etypecase item
                 (symbol
                  (push (read i) initargs)
                  (push item initargs))
                 (cons
                  (push (apply #'make-instance item) generators))))
      (apply #'make-instance 'status-bar :generators (nreverse generators) initargs))))

(defun run-bar-from-file (&optional (file #p"~/.config/i3/cari3s.conf"))
  (run-bar (load-from-file file)))

(defun toplevel ()
  (let ((args (uiop:command-line-arguments)))
    (apply #'run-bar-from-file args)))
