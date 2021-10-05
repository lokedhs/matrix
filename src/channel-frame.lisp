(in-package :matrix.ui)

(defclass channel-content-view (clim:view)
  ())

(clim:define-application-frame matrix-frame ()
  ((server :reader matrix-frame/server))
  (:panes (channel-content :application
                           :default-view (make-instance 'channel-content-view)
                           :display-function 'display-channel-content
                           :scroll-bars :vertical
                           :incremental-redisplay t)
          (bottom-adjuster (clim:make-pane 'clim-extensions:box-adjuster-gadget))
          (input (clim:make-pane 'clim:text-editor)))
  (:layouts (default (clim:vertically ()
                       channel-content
                       bottom-adjuster
                       input))))

(defun display-channel-content (frame stream)
  (declare (ignore stream))
  (format stream "Test message~%"))

(defun open-frame ()
  (let ((frame (clim:make-application-frame 'matrix-frame
                                            :width 700 :height 500)))
    (clim:run-frame-top-level frame)))
