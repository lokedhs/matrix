(in-package :matrix)

(define-condition matrix-error (error)
  ((http-code     :initarg :http-code
                  :initform (error "~s not specified" :http-code)
                  :reader matrix-error/http-code)
   (error-code    :initarg :code
                  :initform (error "~s not specified" :code)
                  :reader matrix-error/code)
   (error-message :initarg :message
                  :initform (error "~s not specified" :message)
                  :reader matrix-error/message))
  (:report (lambda (condition stream)
             (format stream "Error from server. HTTP code: ~a. Code: ~a. Message: ~a"
                     (matrix-error/http-code condition)
                     (matrix-error/code condition)
                     (matrix-error/message condition)))))

(defun make-matrix-error (code stream)
  (let ((json (st-json:read-json stream)))
    (make-instance 'matrix-error
                   :http-code code
                   :code (st-json:getjso "errcode" json)
                   :message (st-json:getjso "error" json))))

(defclass server ()
  ((homeserver      :initarg :homeserver
                    :initform (error "No homeserver specified")
                    :reader server/homeserver)
   (identity-server :initarg :identity-server
                    :initform (error "No identity server specified")
                    :reader server/identity-server)
   (access-token    :initform nil
                    :initarg :access-token
                    :accessor server/access-token)))

(defun server-save (server)
  (list (cons :homeserver (server/homeserver server))
        (cons :identity-server (server/identity-server server))
        (cons :access-token (server/access-token server))))

(defun server-load (data)
  (apply #'make-instance 'server
         (loop
           for field in '(:homeserver :identity-server :access-token)
           append (list field (assoc-with-check field data)))))

(defun matrix-http-call (server url &key (method :get) content-json parameters)
  (let ((token (server/access-token server)))
    (multiple-value-bind (content code headers orig-url should-close reason)
        (apply #'drakma:http-request (format nil "~a~a" (server/homeserver server) url)
               :method method
               :additional-headers `(("Accept" . "application/json")
                                     ,@(if token `(("Authorization" . ,(format nil "Bearer ~a" token)))))
               :force-binary t
               :want-stream t
               `(,@(if content-json (list :content-type "application-json"
                                          :content (st-json:write-json-to-string content-json)
                                          :external-format-in :utf-8))
                 ,@(if parameters (list :parameters parameters))))
      (declare (ignore headers orig-url reason))
      (unwind-protect
           (progn
             (unless (= code 200)
               (error (make-matrix-error code content)))
             (let ((json (st-json:read-json content)))
               json))
        (when should-close
          (close content))))))

(defun find-server (domain)
  (let ((url (format nil "https://~a/.well-known/matrix/client" domain)))
    (multiple-value-bind (content code)
        (drakma:http-request url :force-binary t)
      (unless (= code 200)
        (error "Unable to load server information: ~s" url))
      (let ((json (st-json:read-json-from-string (babel:octets-to-string content :encoding :utf-8))))
        (make-instance 'server
                       :homeserver (json-element-from-path '("m.homeserver" "base_url") json)
                       :identity-server (json-element-from-path '("m.identity_server" "base_url") json))))))

(defun login (server username password)
  (unless (member "m.login.password" (find-login-types server) :test #'equal)
    (error "Server does not support password logins"))
  (let ((result (matrix-http-call server "/_matrix/client/r0/login"
                                  :method :post
                                  :content-json (st-json:jso "type" "m.login.password"
                                                             "identifier" (st-json:jso "type" "m.id.user"
                                                                                       "user" username)
                                                             "password" password))))
    (setf (server/access-token server) (st-json:getjso "access_token" result))
    server))

(defun find-login-types (server)
  (let ((json (matrix-http-call server "/_matrix/client/r0/login")))
    (let ((types (loop
                   for flow in (json-element-from-path '("flows") json)
                   collect (json-element-from-path '("type") flow))))
      types)))

(defun lookup-room-alias (server name)
  (let ((url (format nil "~a/_matrix/client/r0/directory/room/~a"
                     (server/homeserver server)
                     (urlencode:urlencode name))))
    (drakma:http-request url)))

(defun list-users (server term &key (limit 10))
  (let ((url (format nil "~a/_matrix/client/r0/user_directory/search" (server/homeserver server))))
    (drakma:http-request url
                         :method :post
                         :content (st-json:write-json-to-string (st-json:jso "limit" limit
                                                                             "search_term" term)))))

(defun public-rooms (srv &key limit server since)
  (check-type limit (or null (integer 0)))
  (check-type server (or null string))
  (check-type since (or null string))
  (let ((result (matrix-http-call srv "/_matrix/client/r0/publicRooms"
                                  :parameters (append (if limit `(("limit" . ,(format nil "~a" limit))))
                                                      (if server `(("server" . ,server)))
                                                      (if since `(("since" . ,since)))))))
    result))

(defun join-room (srv name &key reason)
  (check-type name string)
  (check-type reason (or null string))
  (let* ((content (apply #'st-json:jso
                         `(,@(if reason `(("reason" ,reason))))))
         (result (matrix-http-call srv (format nil "/_matrix/client/r0/join/~a" name)
                                   :method :post
                                   :content-json content)))
    (getjso-with-check "room_id" result)))
