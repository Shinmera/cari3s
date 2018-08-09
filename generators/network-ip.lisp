#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass network-ip (value-generator)
  ((device :initarg :device :accessor device))
  (:default-initargs
   :label "IP"
   :device T))

(cffi:defcstruct (sockaddr :conc-name sockaddr-)
  (family :ushort)
  (data :char :count 14))

(cffi:defcstruct (ifaddrs :conc-name ifaddrs-)
  (next :pointer)
  (name :string)
  (flags :uint)
  (address :pointer)
  (netmask :pointer)
  (broadcast/destination-address :pointer)
  (data :pointer))

(cffi:defcfun "getifaddrs" :int
  (ifaddrs :pointer))

(cffi:defcfun "freeifaddrs" :void
  (ifaddrs :pointer))

(cffi:defcfun "getnameinfo" :int
  (addr :pointer)
  (len socklen_t)
  (host :string)
  (hostlen socklen_t)
  (serv :string)
  (servlen socklen_t)
  (flags :int))

(defconstant NI-MAXHOST 1025)
(defconstant NI-NUMERICHOST 1)

(defmethod compute-value ((generator network-ip))
  (cffi:with-foreign-objects ((pointer :pointer)
                              (host :char NI-MAXHOST))
    (cond ((= 0 (getifaddrs pointer))
           (let ((pointer (cffi:mem-ref pointer :pointer)))
             (unwind-protect
                  (loop for addrs = pointer then (ifaddrs-next addrs)
                        until (cffi:null-pointer-p addrs)
                        unless (cffi:null-pointer-p (ifaddrs-address addrs))
                        do (when (and (find (sockaddr-family (ifaddrs-address addrs)) '(2 10))
                                      (or (eql T (device generator))
                                          (string= (device generator) (ifaddrs-name addrs)))) 
                             ;; AF_INET / AF_INET6
                             (cond ((= 0 (getnameinfo (ifaddrs-address addrs)
                                                      (if (= 2 (sockaddr-family (ifaddrs-address addrs)))
                                                          16
                                                          28)
                                                      host NI-MAXHOST
                                                      (cffi:null-pointer) 0 NI-NUMERICHOST))
                                    (let ((address (cffi:foreign-string-to-lisp host)))
                                      (unless (find address '("127.0.0.1" "::1") :test #'string=)
                                        (return address))))
                                   (T
                                    (return "Error"))))
                        finally (return "No Device"))
               (freeifaddrs pointer))))
          (T
           "Error"))))
