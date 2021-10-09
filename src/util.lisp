(in-package :matrix)

(defun assoc-with-check (key alist)
  (let ((value (assoc key alist)))
    (unless value
      (error "No value for ~s in alis ~s" key alist))
    (cdr value)))
