#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defun parse-all-integers (string &key (start 0) (end (length string)))
  (loop for offset = start then next
        for (int next) = (multiple-value-list
                          (parse-integer string :start offset :end end :junk-allowed T))
        while int
        collect int))

(defun enlist (list &rest items)
  (if (listp list) list (list* list items)))

(defun null-if (nullable item)
  (if (eql nullable item) NIL item))

(defun html-escape-char (char stream)
  (case char
    (#\< (write-string "&lt;" stream))
    (#\> (write-string "&gt;" stream))
    (#\& (write-string "&amp;" stream))
    (#\' (write-string "&#39;" stream))
    (#\" (write-string "&quot;" stream))
    (T (write-char char stream))))

(defmethod format-escape (o thing &rest args)
  (declare (ignore args))
  (format-escape o (princ-to-string thing)))

(defmethod format-escape (o (symbol symbol) &rest args)
  (declare (ignore args))
  (format-escape o (string-downcase symbol)))

(defmethod format-escape (o (string string) &rest args)
  (declare (ignore args))
  (loop for char across string
        do (html-escape-char char o)))

(defmethod format-color (o color &rest args)
  (declare (ignore args))
  (format-escape o color))

(defmethod format-color (o (color integer) &rest args)
  (declare (ignore args))
  (format o "#~6,'0x" color))

(defmethod format-color (o (color list) &rest args)
  (declare (ignore args))
  (apply #'format o "#~2,'0x~2,'0x~2,'0x~@[~2,'0x~]" color))

(defun format-color-string (color)
  (with-output-to-string (o)
    (format-color o color)))

(defun month-short-name (mm)
  (case mm
    (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") (6 "Jun") (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct") (11 "Nov") (12 "Dec")))

(defun day-short-name (day)
  (case day
    (0 "Mon") (1 "Tue") (2 "Wed") (3 "Thu") (4 "Fri") (5 "Sat") (6 "Sun")))
