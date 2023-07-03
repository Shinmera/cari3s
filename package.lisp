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
   #:network-ip
   #:uptime
   #:weather)
  ;; event.lisp
  (:export
   #:event
   #:generate
   #:echo
   #:message
   #:click
   #:name
   #:instance
   #:button
   #:location
   #:relative-location
   #:block-size
   #:process-event
   #:parse-event-or-lose
   #:object-initargs
   #:serialize-object
   #:event-server
   #:port
   #:listener
   #:connections
   #:start
   #:stop
   #:process-connections)
  ;; generators.lisp
  (:export
   #:generator
   #:interval
   #:generate
   #:single-generator
   #:generate
   #:value-generator
   #:compute-value)
  ;; protocol.lisp
  (:export
   #:to-table
   #:from-table
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
   #:separator
   #:text-format
   #:pango-block
   #:markup
   #:short-markup)
  ;; status-bar.lisp
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
   #:run-bar)
  ;; toolkit.lisp
  (:export)
  ;; toplevel.lisp
  (:export
   #:load-from-file
   #:run-bar-from-file
   #:toplevel))
