(in-package #:org.shirakumo.cari3s)

(defparameter *weather-api* "https://api.darksky.net/forecast/~a/~f,~f")

(defclass weather (value-generator)
  ((api-key :initarg :api-key :accessor api-key)
   (location :initarg :location :accessor location))
  (:default-initargs
   :text "WEATHER ~a at ~d°C~:[ (~d°C)~;~*~], ~d%, ~dkm/h, ~dhPa"
   :markup '((0 7 :color #x0088EE))
   :interval (* 60 60) ; every hour
   :api-key (error "API-KEY required")
   :location (error "LOCATION required")))

(defmethod compute-value ((generator weather))
  (let ((stream (drakma:http-request (format NIL *weather-api* (api-key generator) (car (location generator)) (cdr (location generator)))
                                     :parameters `(("units" . "si")
                                                   ("exclude" .,(format NIL "~{~(~a~)~^,~}" '(:minutely :hourly :daily :flags :alerts))))
                                     :external-format-in :utf-8
                                     :want-stream T)))
    (unwind-protect
         (let ((data (gethash "currently" (com.inuoe.jzon:parse stream))))
           (flet ((d (field) (gethash field data)))
             (let ((summary (d "summary"))
                   (temperature (round (d "temperature")))
                   (apparent (round (d "apparentTemperature")))
                   (humidity (round (* 100 (d "humidity"))))
                   (wind (round (d "windSpeed")))
                   (pressure (round (d "pressure"))))
               (list summary temperature (= temperature apparent) apparent humidity wind pressure))))
      (close stream))))
