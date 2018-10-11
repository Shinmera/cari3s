#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defgeneric to-table (object))
(defgeneric from-table (type table))

(defclass header ()
  ((version :initarg :version :accessor version)
   (stop-signal :initarg :stop-signal :accessor stop-signal)
   (continue-signal :initarg :continue-signal :accessor continue-signal)
   (send-click-events-p :initarg :send-click-events-p :accessor send-click-events-p))
  (:default-initargs
   :version 1
   :stop-signal 19
   :continue-signal 18
   :send-click-events-p NIL))

(defmethod to-table ((header header))
  (let ((table (make-hash-table)))
    (setf (gethash "version" table) (version header))
    (setf (gethash "stop_signal" table) (stop-signal header))
    (setf (gethash "cont_signal" table) (continue-signal header))
    (setf (gethash "click_events" table) (send-click-events-p header))
    table))

(defclass block ()
  ((text :initarg :text :accessor text)
   (short-text :initarg :short-text :accessor short-text)
   (foreground :initarg :foreground :accessor foreground)
   (background :initarg :background :accessor background)
   (border :initarg :border :accessor border)
   (min-width :initarg :min-width :accessor min-width)
   (align :initarg :align :accessor align)
   (name :initarg :name :accessor name)
   (instance :initarg :instance :accessor instance)
   (urgent-p :initarg :urgent-p :accessor urgent-p)
   (separator :initarg :separator :accessor separator))
  (:default-initargs
   :text (error "TEXT required.")
   :short-text NIL
   :foreground NIL
   :background NIL
   :border NIL
   :min-width NIL
   :align :left
   :name NIL
   :instance NIL
   :urgent-p NIL
   :separator T))

(defmethod text-format ((block block)) :none)

(defmethod to-table ((block block))
  (let ((table (make-hash-table)))
    (macrolet ((maybe-output (name value &optional transform)
                 `(let ((%temp ,value))
                    (when %temp (setf (gethash ,name table) ,(if transform `(,transform %temp) '%temp))))))
      (setf (gethash "full_text" table) (text block))
      (maybe-output "short_text" (short-text block))
      (maybe-output "color" (foreground block) format-color-string)
      (maybe-output "background" (background block) format-color-string)
      (maybe-output "border" (border block) format-color-string)
      (maybe-output "min_width" (min-width block))
      (unless (eql :left (align block))
        (setf (gethash "align" table) (ecase (align block)
                                            (:center "center")
                                            (:right "right"))))
      (maybe-output "name" (name block))
      (maybe-output "instance" (instance block))
      (maybe-output "urgent" (urgent-p block))
      (if (separator block)
          (when (numberp (separator block))
            (setf (gethash "separator_block_width" table) (separator block)))
          (setf (gethash "separator" table) 'yason:false))
      (unless (eql :none (text-format block))
        (setf (gethash "markup" table) (string-downcase (text-format block)))))
    table))

(defclass pango-block (block)
  ((markup :initarg :markup :accessor markup)
   (short-markup :initarg :short-markup :accessor short-markup))
  (:default-initargs
   :markup ()
   :short-markup ()))

(defmethod text-format ((block pango-block)) :pango)

(defmethod text ((block pango-block))
  (pango-markup:markup-regions (call-next-method) (markup block)))

(defmethod short-text ((block pango-block))
  (let ((text (call-next-method)))
    (when text
      (pango-markup:markup-regions text (short-markup block)))))
