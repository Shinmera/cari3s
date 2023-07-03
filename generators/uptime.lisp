(in-package #:org.shirakumo.cari3s)

(defclass uptime (value-generator)
  ()
  (:default-initargs
   :text "UP~@[ ~dd~] ~d:~2,'0d"
   :markup '((0 2 :color #x0088EE))))

(defmethod compute-value ((generator uptime))
  (let ((uptime (with-open-file (stream "/proc/uptime")
                  (read stream))))
    (values (unless (= 0 (floor uptime (* 60 60 24)))
              (floor uptime (* 60 60 24)))
            (mod (floor uptime (* 60 60)) 24)
            (mod (floor uptime 60) 60)
            (mod (floor uptime 1) 60))))
