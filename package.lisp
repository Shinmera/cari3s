#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cari3s
  (:nicknames #:org.shirakumo.cari3s)
  (:use #:cl)
  (:shadow #:block)
  ;; generators
  (:export
   #:battery
   #:clock
   #:cpu-usage
   #:disk-usage
   #:io-usage
   #:mem-usage
   #:network-ip)
  ;; generators.lisp
  (:export
   #:generator
   #:generate
   #:single-generator
   #:generate
   #:value-generator
   #:compute-value)
  ;; protocol.lisp
  (:export
   #:header
   #:version
   #:stop-signal
   #:continue-signal
   #:send-click-events-p
   #:block
   #:text
   #:short-text
   #:foreground
   #:background
   #:border
   #:min-width
   #:align
   #:name
   #:instance
   #:urgent-p
   #:separator)
  ;; toolkit.lisp
  (:export)
  ;; toplevel.lisp
  (:export
   #:status-bar
   #:interval
   #:next-time
   #:generators
   #:output
   #:input
   #:click-pause
   #:produce-output
   #:process
   #:run-bar
   #:load-from-file
   #:run-bar-from-file
   #:toplevel))
