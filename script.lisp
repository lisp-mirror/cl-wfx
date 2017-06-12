(in-package :cl-wfx)



#|
(defgeneric parse-sexp (module sexp &key &allow-other-keys)
  (:documentation "Break sexp up into constituent pieces for evaluation."))

(defmethod parse-sexp (sexp &key &allow-other-keys)
  (destructuring-bind (type . attributes-body) sexp     
      (values type
	      (loop while (keywordp (car attributes-body))
		 collect (pop attributes-body)
		 collect (pop attributes-body))
	      attributes-body)))

(defgeneric read-sexp (module scope &key attributes body &allow-other-keys))

(defun wfx-read-sexp (sexp)
  ;;TODO: Sort out error handling to come back to script repl!!!
  
  (multiple-value-bind (scope attributes body) 
      (parse-sexp *module* sexp)    
    (read-sexp *module* scope sexp :attributes attributes :body body)))

(defgeneric eval-sexp (module scope &key attributes body &allow-other-keys))
|#


(declaim (inline scriptc))
(defun scriptc (script)
  (frmt "~S" script))

(defun wfx-read-string (string)
  (let ((*read-eval* nil))
   (read-from-string string)))

(defun wfx-eval (script)
  (let ((*read-eval* nil))
    (when script
      (if (stringp script)
	  (eval (read-from-string script))
	  (eval script)))))

(defmacro with-script (&body body) 
  `(dolist (sexp ',body)
     (read-sexp *module* sexp)))
