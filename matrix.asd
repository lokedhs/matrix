(asdf:defsystem #:matrix
  :description "Matrix client library"
  :license "Apache"
  :serial t
  :depends-on (:alexandria
               :lunamech-matrix-api
               :ironclad
               :mcclim)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "channel-frame")))))
