(asdf:defsystem #:matrix
  :description "Matrix client library"
  :license "Apache"
  :serial t
  :depends-on (:alexandria
               :log4cl
               :lunamech-matrix-api
               :ironclad
               :mcclim
               :containers)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "queue")
                             (:file "matrix")
                             (:file "channel-frame")))))
