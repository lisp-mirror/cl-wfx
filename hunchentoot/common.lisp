(in-package :cl-wfx)

(defclass hunch-request (request)
  ((request-object :initarg :request-object
		   :accessor request-object)))

(defgeneric render (renderer &key &allow-other-keys))

(setf hunchentoot:*catch-errors-p* nil)
(setf hunchentoot:*show-lisp-errors-p* t)

(defmacro with-html (&body body)
  `(cl-who:with-html-output (*standard-output* nil :indent t)
     ,@body))

(defmacro with-html-string (&body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :indent t)
     ,@body))

(defmacro with-inner-htm (&body body)
  `(cl-who:str
    (cl-who:htm
     ,@body)))

(defun render-to-string* (renderer &rest args)
  (let* ((splits (split-sequence:split-sequence #\: renderer))
	   
	   (renderer-package (if (second splits)
				 (intern (string-upcase (id-string (first splits))) 
					 :keyword)
				 :cl-wfx))
	   (renderer* (if (second splits)
			  (second splits)
			  (first splits)))
	   (render-function (intern (string-upcase (id-string renderer*)) 
				 (find-package renderer-package)))
	   )
      
      (apply render-function args)))

(defmacro with-debugging (&body body)
  ;; Using this as debugging tool because hunchentoot
  ;; swallows all errors here if set to swallow errors.
  `(handler-bind ((error #'invoke-debugger))
     ,@body))




