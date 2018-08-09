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
               (:file "pango")
               (:file "protocol")
               (:file "generators")
               (:file "toplevel")
               (:file "documentation"))
  :depends-on (:jonathan
               :documentation-utils))
