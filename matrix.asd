(asdf:defsystem #:matrix
  :description "Matrix client library"
  :license "Apache"
  :serial t
  :depends-on (:alexandria
               :drakma
               :st-json
               :do-urlencode
               :log4cl
               :ironclad
               ;; UI
               :mcclim)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "matrix")))))
