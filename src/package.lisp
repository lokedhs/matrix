(defpackage :matrix
  (:use :cl)
  (:documentation "Matrix client library for Common Lisp")
  (:export #:matrix-error
           #:matrix-error/http-code
           #:matrix-error/code
           #:matrix-error/message
           #:server
           #:server/homeserver
           #:server/identity-server
           #:server/access-token
           #:server-save
           #:server-load
           #:find-server
           #:login
           #:find-login-types
           #:lookup-room-alias
           #:list-users
           #:public-rooms
           #:join-room))

(defpackage :matrix.ui
  (:use :cl)
  (:documentation "CLIM-based user interface to Matrix")
  (:export
   #:open-frame))
