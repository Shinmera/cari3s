(asdf:defsystem cari3s
  :version "2.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A generator for the i3 status bar."
  :homepage "https://Shinmera.github.io/cari3s/"
  :bug-tracker "https://github.com/Shinmera/cari3s/issues"
  :source-control (:git "https://github.com/Shinmera/cari3s.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "event")
               (:file "generators")
               (:file "status-bar")
               (:file "toplevel")
               (:file "generators/battery")
               (:file "generators/clock")
               (:file "generators/cpu-usage")
               (:file "generators/disk-usage")
               (:file "generators/io-usage")
               (:file "generators/mem-usage")
               (:file "generators/network-ip")
               (:file "generators/uptime")
               (:file "generators/weather")
               (:file "documentation"))
  :depends-on (:com.inuoe.jzon
               :cffi
               :usocket
               :pango-markup
               :drakma
               :closer-mop
               :documentation-utils)
  :build-operation "program-op"
  :build-pathname "cari3s"
  :entry-point "cari3s:toplevel")
