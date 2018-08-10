#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.cari3s)

(defclass disk-usage (value-generator)
  ((device :initarg :device :accessor device))
  (:default-initargs
   :text "DISK ~4,1f%"
   :markup '((0 4 :color #x0088EE))
   :device "/"))

(cffi:defctype __fsword_t #+x86-64 :uint64 #+x86 :uint32)
(cffi:defctype fsblkcnt_t #+x86-64 :uint64 #+x86 :uint32)
(cffi:defctype fsfilcnt_t #+x86-64 :uint64 #+x86 :uint32)
(cffi:defctype socklen_t :uint)

(cffi:defcstruct file-system-id
  (val :int :count 2))

(cffi:defcstruct file-system
  (type __fsword_t)
  (block-size __fsword_t)
  (blocks fsblkcnt_t)
  (free-blocks fsblkcnt_t)
  (available-blocks fsblkcnt_t)
  (file-nodes fsfilcnt_t)
  (free-file-nodes fsfilcnt_t)
  (id (:struct file-system-id))
  (name-length __fsword_t)
  (fragment-size __fsword_t)
  (mount-flags __fsword_t)
  (spare __fsword_t :count 5))
;; The man page does not say how many items there should be in the spare array. Cool!

(cffi:defcfun "statfs" :int
  (path :string)
  (buf :pointer))

(defmethod compute-value ((generator disk-usage))
  (cffi:with-foreign-object (buffer '(:struct file-system))
    (list
     (cond ((= 0 (statfs (device generator) buffer))
            (cffi:with-foreign-slots ((blocks available-blocks) buffer (:struct file-system))
              (float (* (/ (- blocks available-blocks) blocks) 100))))
           (T
            "Error")))))
