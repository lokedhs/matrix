(in-package :matrix)

(defun assoc-with-check (key alist)
  (let ((value (assoc key alist)))
    (unless value
      (error "No value for ~s in alis ~s" key alist))
    (cdr value)))

(defun getjso-with-check (key json)
  (multiple-value-bind (result present-p)
      (st-json:getjso key json)
    (unless present-p
      (error "Missing value ~s in ~s" key json))
    result))

(defun json-element-from-path (path json)
  (loop
    with current = json
    for m in path
    for result = (getjso-with-check m current)
    do (setq current result)
    finally (return current)))
