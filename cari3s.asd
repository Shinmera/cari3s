#|
 This file is a part of cari3s
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cari3s
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A generator for the i3 status bar."
  :homepage "https://github.com/Shinmera/cari3s"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "generators")
               (:file "toplevel")
               (:file "generators/battery")
               (:file "generators/clock")
               (:file "generators/cpu-usage")
               (:file "generators/disk-usage")
               (:file "generators/io-usage")
               (:file "generators/mem-usage")
               (:file "generators/network-ip")
               (:file "documentation"))
  :depends-on (:jonathan
               :cffi
               :pango-markup
               :documentation-utils)
  :build-operation "program-op"
  :build-pathname "cari3s"
  :entry-point "cari3s:toplevel")
