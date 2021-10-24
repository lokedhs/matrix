(defpackage :matrix
  (:use :cl)
  (:documentation "Matrix client library for Common Lisp")
  (:export #:make-connection-queue
           #:stop-connection
           #:start-connection
           #:connection-queue/connection
           #:load-connection
           #:save-connection))

(defpackage :matrix.ui
  (:use :cl)
  (:documentation "CLIM-based user interface to Matrix")
  (:export #:open-frame))
