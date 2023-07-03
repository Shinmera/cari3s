(in-package #:org.shirakumo.cari3s)

(defclass clock (value-generator)
  ()
  (:default-initargs
   :text "DATE ~7@*~a ~2@*~2d ~8@*~a ~4@*~2d:~2,'0d:~2,'0d"
   :markup '((0 4 :color #x0088EE))))

(defmethod compute-value ((generator clock))
  (multiple-value-bind (s m h dd mm yy day) (decode-universal-time (get-universal-time))
    (list yy mm dd day h m s
          (day-short-name day)
          (month-short-name mm))))
