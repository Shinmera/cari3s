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

(defun format-color-string (color)
  (with-output-to-string (o)
    (pango-markup::format-color o color)))

(defun month-short-name (mm)
  (case mm
    (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") (6 "Jun") (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct") (11 "Nov") (12 "Dec")))

(defun day-short-name (day)
  (case day
    (0 "Mon") (1 "Tue") (2 "Wed") (3 "Thu") (4 "Fri") (5 "Sat") (6 "Sun")))
 
