(defpackage :matrix
  (:use :cl)
  (:documentation "Matrix client library for Common Lisp")
  (:export))

(defpackage :matrix.ui
  (:use :cl)
  (:documentation "CLIM-based user interface to Matrix")
  (:export #:open-frame))
