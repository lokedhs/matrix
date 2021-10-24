(in-package :matrix)

(defclass connection-queue ()
  ((connection  :initarg :connection
                :reader connection-queue/connection)
   (queue       :initform (containers:make-queue)
                :reader connection-queue/queue)
   (thread      :initform nil
                :accessor connection-queue/thread)
   (sync-thread :initform nil
                :accessor connection-queue/sync-thread)
   (state       :initform :stopped
                :accessor connection-queue/state)
   (lock        :initform (bordeaux-threads:make-lock)
                :reader connection-queue/lock)
   (condvar     :initform (bordeaux-threads:make-condition-variable)
                :reader connection-queue/condvar)))

(defun make-connection-queue (conn)
  (let ((queue (make-instance 'connection-queue :connection conn)))
    (start-connection queue)
    queue))

(defun start-connection (queue)
  (when (connection-queue/thread queue)
    (error "Thread already started"))
  (setf (connection-queue/state queue) :running)
  (setf (connection-queue/thread queue)
        (bordeaux-threads:make-thread (lambda ()
                                        (queue-main-loop queue))
                                      :name "Queue thread")))

(defun stop-connection (queue)
  (with-accessors ((lock connection-queue/lock)
                   (condvar connection-queue/condvar)
                   (state connection-queue/state))
      queue
    (bordeaux-threads:with-lock-held (lock)
      (when (eq state :running)
        (setf state :stopping)
        (bordeaux-threads:condition-notify condvar)))))

(defun queue-main-loop (queue)
  (with-accessors ((queue connection-queue/queue)
                   (lock connection-queue/lock)
                   (condvar connection-queue/condvar)
                   (state connection-queue/state))
      queue
    (loop
      for element = (bordeaux-threads:with-lock-held (lock)
                      (loop
                        while (and (eq state :running)
                                   (containers:empty-p queue))
                        do (bordeaux-threads:condition-wait condvar lock)
                        finally (return (containers:queue-pop queue :if-empty nil))))
      while element
        do (funcall element))
    (bordeaux-threads:with-lock-held (lock)
      (unless (eq state :stopping)
        (error "Not in stopping state"))
      (setf state :stopped)))
  (log:info "Stopping queue"))
