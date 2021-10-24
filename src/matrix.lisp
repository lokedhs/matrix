(in-package :matrix)

(defgeneric serialise-instance (obj)
  (:method (obj) obj)
  (:method ((obj cons)) (cons :list obj)))

(defgeneric deserialise-instance (class-name value)
  (:method (a b) (error "No method for ~s : ~s" a b)))

(defmacro define-serialisation-encoder (class-name &body overrides)
  (let ((class (find-class class-name)))
    (closer-mop:finalize-inheritance class)
    `(progn
       (defmethod serialise-instance ((obj ,class-name))
         (append '(:class ,class-name)
                 ,@(loop
                     for slot in (closer-mop:class-slots class)
                     for slot-name = (closer-mop:slot-definition-name slot)
                     for override-slot = (find slot-name overrides :key #'car)
                     for s = (if override-slot
                                 `(list ',slot-name (funcall ,(second override-slot) obj))
                                 `(if (slot-boundp obj ',slot-name)
                                      (list ',slot-name (serialise-value (slot-value obj ',slot-name)))
                                      nil))
                     when s
                       collect s)))
       (defmethod deserialise-instance ((name (eql ',class-name)) value)
         (let ((obj (allocate-instance (find-class ',class-name))))
           ,@(loop
               for slot in (closer-mop:class-slots class)
               for slot-name = (closer-mop:slot-definition-name slot)
               for override-slot = (find slot-name overrides :key #'car)
               collect `(alexandria:when-let ((s (getf value ',slot-name)))
                          (setf (slot-value obj ',slot-name)
                                ,(if override-slot
                                     `(funcall ,(third override-slot) s)
                                     `(deserialise-value s)))))
           (initialize-instance obj)
           obj)))))

(define-serialisation-encoder lunamech-matrix-api/v2:connection
  (lunamech-matrix-api/v2:con-lock (lambda (v) (declare (ignore v)) nil)
                                   (lambda (v) (declare (ignore v)) (bordeaux-threads:make-lock)))
  (lunamech-matrix-api/v2:password (lambda (v) (declare (ignore v)) nil)
                                   (lambda (v) (declare (ignore v)) nil)))
(define-serialisation-encoder lunamech-matrix-api/v2:status)
(define-serialisation-encoder lunamech-matrix-api/v2:auth)

(defun serialise-value (value)
  (serialise-instance value))

(defun deserialise-value (value)
  (if (consp value)
      (ecase (car value)
        (:class (deserialise-instance (cadr value) (cddr value)))
        (:list (mapcar #'deserialise-value (cdr value))))
      value))

(defun save-connection (conn stream)
  (print (serialise-value (connection-queue/connection conn)) stream))

(defun load-connection (stream)
  (let ((*read-eval* nil))
    (make-instance 'connection-queue :connection (deserialise-value (read stream)))))
