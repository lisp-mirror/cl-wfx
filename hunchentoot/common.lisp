(in-package :cl-wfx)


(defgeneric render (renderer &key &allow-other-keys))

(setf hunchentoot:*catch-errors-p* nil)
(setf hunchentoot:*show-lisp-errors-p* t)

(defmacro with-html (&body body)
  `(cl-who:with-html-output (*standard-output* nil :indent t)
     ,@body))

(defmacro with-html-string (&body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :indent t)
     ,@body))

(defun render-to-string* (renderer &rest args)
  (with-html-string
    (let* ((splits (split-sequence:split-sequence #\: renderer))
	   
	   (renderer-package (if (second splits)
				 (intern (string-upcase (id-string (first splits))) 
					 :keyword)
				 :cl-wfx))
	   (renderer* (if (second splits)
			  (second splits)
			  (first splits))))

      (apply #'render (intern (string-upcase (id-string renderer*)) 
			      (find-package renderer-package)) args))))

(defmacro with-debugging (&body body)
  ;; Using this as debugging tool because hunchentoot
  ;; swallows all errors here if set to swallow errors.
  `(handler-bind ((error #'invoke-debugger))
     ,@body))




