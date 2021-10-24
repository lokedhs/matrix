(in-package :matrix.ui)

(clim:define-command-table channel-commands)

(defclass channel-interactor-pane (clim:interactor-pane)
  ())

(defclass channel-content-view (clim:view)
  ())

(defclass channel-list-view (clim:view)
  ())

(defclass channel-pointer-documentation-view (clim:pointer-documentation-view)
  ())

(defparameter +channel-pointer-documentation-view+ (make-instance 'channel-pointer-documentation-view))

(clim:define-presentation-type command-or-string
    (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

(defun create-interactor ()
  (clim:make-clim-stream-pane :type 'channel-interactor-pane
                              :name 'channel-interactor
                              :default-view +channel-pointer-documentation-view+
                              :display-function 'display-channel-interactor-content
                              :incremental-redisplay t
                              :text-margins '(:left (:absolute 2)
                                              :right (:relative 2))
                              :scroll-bars :both))

(defun display-channel-interactor-content (frame stream)
  (declare (ignore frame))
  (format stream "This is a test of the interactor pane~%"))

(defmethod clim:stream-present :around ((stream channel-interactor-pane) object type
                                        &rest args &key (single-box nil sbp) &allow-other-keys)
  (declare (ignore single-box sbp))
  (apply #'call-next-method stream object type :single-box t args))

(clim:define-presentation-method clim:accept ((type command-or-string) stream (view clim:textual-view) &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (`(or ,command-ptype clim:string))
        (object type event options)
        (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
          (cond ((member initial-char clim:*command-dispatchers*)
                 (clim:read-gesture :stream stream)
                 (clim:accept command-ptype :stream stream :view view :prompt nil :history 'clim:command))
                (t
                 (clim:accept 'string :stream stream :view view :prompt nil :history 'command-or-string))))
      (f
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-application-frame matrix-frame ()
  ((connection :accessor matrix-frame/connection))
  (:panes (channel-content :application
                           :default-view (make-instance 'channel-content-view)
                           :display-function 'display-channel-content
                           :scroll-bars :vertical
                           :incremental-redisplay t)
          (channel-list :application
                        :default-view (make-instance 'channel-list-view)
                        :display-function 'display-channel-list
                        :scroll-bars :vertical
                        :incremental-redisplay nil)
          (interactor-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (interactor (create-interactor))
          (doc :pointer-documentation :default-view +channel-pointer-documentation-view+))
  (:layouts (default (clim:vertically ()
                       (8/10 (clim:horizontally ()
                               (2/10 channel-list)
                               (8/10 channel-content)))
                       interactor-adjuster
                       (2/10 interactor)
                       doc))))

(defmethod initialize-instance :after ((frame matrix-frame) &key)
  (let ((s (open (config-file-pathname) :direction :input :if-does-not-exist nil)))
    (when s
      (unwind-protect
           (let ((conn (matrix:load-connection s)))
             (break)
             (setf (matrix-frame/connection frame) (matrix:make-connection-queue conn)))
        (close s)))))

(defmethod clim:note-frame-disabled :after (fm (frame matrix-frame))
  (log:info "Closing frame")
  (matrix:stop-connection (matrix-frame/connection frame)))

(define-matrix-frame-command (com-send-msg :name "Send" :menu t) ((message clim:string))
  (log:info "Sending message to channel: ~s" message ))

(define-matrix-frame-command (com-login :name "Server" :menu t)
  ((server clim:string)
   (user clim:string)
   (password clim:string))
  (let ((connection (lunamech-matrix-api/v2:make-connection user password
                                                            (format nil "https://~a" server)
                                                            "/_matrix/client/r0/")))
    (lunamech-matrix-api/v2:password-login connection)
    (format t "Login successful~%")
    (setf (matrix-frame/connection clim:*application-frame*) (matrix:make-connection-queue connection))
    (save-config clim:*application-frame*)))

(clim:define-presentation-type empty-input ())

(clim:define-presentation-method clim:present
    (object (type empty-input) stream view &key &allow-other-keys)
  (princ "" stream))

(defmethod clim:read-frame-command ((frame matrix-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\, #\/)))
        (clim:accept 'command-or-string :stream stream :prompt nil
                                        :default "hello" :default-type 'empty-input))
    (cond
      ((clim:presentation-subtypep type 'empty-input)
       ;; Do nothing.
       `(com-send-msg (values)))
      ((clim:presentation-subtypep type 'clim:command)
       (climi::ensure-complete-command object (clim:frame-command-table frame) stream))
      (t `(com-send-msg ,object)))))

(defun display-channel-content (frame stream)
  (declare (ignore frame))
  (format stream "Test message~%"))

(defun display-channel-list (frame stream)
  (declare (ignore frame))  
  (format stream "Channel list"))

(defun config-file-pathname ()
  #p"~/.matrix-clim.lisp")

(defun save-config (frame)
  (alexandria:when-let ((connection (matrix-frame/connection frame)))
    (with-open-file (s (config-file-pathname)
                       :direction :output
                       :if-exists :supersede)
      (matrix:save-connection connection s))))

(defun open-frame ()
  (let ((frame (clim:make-application-frame 'matrix-frame :width 900 :height 700)))
    (clim:run-frame-top-level frame)))
