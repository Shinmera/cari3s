(in-package #:org.shirakumo.cari3s)

(defclass battery (value-generator)
  ((battery-name :initarg :battery :accessor battery-name))
  (:default-initargs
   :text "BAT ~4,1f%~:[~; 🡅~]~:[~; 🡇~]~@[ ~d:~2,'0d~]"
   :battery T
   :markup '((0 3 :color #x0088EE))))

(defmethod compute-value ((generator battery))
  (let ((battery (or (if (eql T (battery-name generator))
                         (first (directory #p"/sys/class/power_supply/BAT*/"))
                         (make-pathname :directory `(:absolute "sys" "class" "power_supply" ,(battery-name generator))))
                     (error "No Battery"))))
    (flet ((r (file) (with-open-file (stream (merge-pathnames file battery) :if-does-not-exist NIL)
                       (let ((*package* #.*package*))
                         (when stream (read stream))))))
      (let ((energy-full (r "energy_full"))
            (charge-full (r "charge_full"))
            (energy-now (r "energy_now"))
            (charge-now (r "charge_now"))
            (voltage-now (r "voltage_now"))
            (power-now (or (r "current_now") (r "power_now")))
            (charging-p (eql (r "status") 'charging))
            (discharging-p (eql (r "status") 'discharging)))
        (when (and energy-full (not charge-full))
          (setf charge-full (/ (* 1000 energy-full) (or voltage-now 1000))))
        (when (and energy-now (not charge-now))
          (setf charge-now (/ (* 1000 energy-now) (or voltage-now 1000)))
          (when power-now
            (setf power-now (/ (* 1000 power-now) (or voltage-now 1000)))))
        (let ((seconds (when (and power-now (< 1 power-now))
                         (if charging-p
                             (* 3600 (/ (- charge-full charge-now) power-now))
                             (* 3600 (/ charge-now power-now))))))
          (list (float (min 100 (max 0 (* 100 (/ charge-now charge-full)))))
                charging-p
                discharging-p
                (when seconds (floor seconds 3600))
                (when seconds (floor (mod seconds 3600) 60))
                (when seconds (mod seconds 60))))))))
