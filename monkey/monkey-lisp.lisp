(in-package :monkey-lisp)

(defparameter *processor-class* nil)

(defvar *processor* nil)

(defclass processor ()
  ())

;;TODO: is this still needed?
(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

(defun monkey-sexp-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
           (and (consp (car form)) (funcall test (caar form))))))

(defun parse-explicit-attributes-monkey-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-monkey-sexp (sexp)
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (second rest))
     when (second rest)
     collect (first rest) into attributes and
     collect (second rest) into attributes
     end
     finally (return (values tag attributes rest))))





(defun parse-monkey-sexp (sexp)
  
  (destructuring-bind (type . attributes-body) 
      (if (and (consp sexp) 
	       (equalp (length sexp) 1))
	  (car sexp)
	  sexp)  
    
    (values type
	    (loop while (keywordp (car attributes-body))
	       collect (pop attributes-body)
	       collect (pop attributes-body))
	    attributes-body)))
#|
(defun parse-monkey-sexp (sexp)
  (if (consp (first sexp))
    (parse-explicit-attributes-monkey-sexp sexp)
    (parse-implicit-attributes-monkey-sexp sexp)))
|#

(defun language-operator-p (form)
  (and (consp form) 
       (symbolp (car form)) 
       (get (car form) 'monkey-lang-op)))

;;todo: loop through superclasses and get the correct code
(defun get-lang-op-code (processor op)
  
  (dolist (class (sb-mop::class-precedence-list (class-of processor)))
    (let ((code (find (class-name class) 
		      (get op 'monkey-lang-op) :key 'car :test 'equalp)))
 
      (when code
	(return-from get-lang-op-code (second code))))))

(defun process-language-operator (processor form)
  (apply (get-lang-op-code processor (car form)) processor (rest form)))

(defmacro define-language-operator (processor-class op code)  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ,op 'monkey-lang-op) 
	   (pushnew (list
		     ,processor-class
		     ,code)
		    (get ,op 'monkey-lang-op)
		    :test 'equal
		    :key #'car))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (make-symbol ,(string n))))
     ,@body))

(defun monkey-macro-form-p (form)
  (monkey-sexp-p form #'(lambda (x) (and (symbolp x) (get x 'macro)))))


(defun parse-monkey-macro-lambda-list (args)
  "Parse a lambda list that can include the &attributes lambda-list-keyword.
Exmaples:
>(parse-monkey-macro-lambda-list '(a b c))
NIL
(A B C)
>(parse-monkey-macro-lambda-list '(&attributes attrs a b c))
ATTRS
(A B C)
>(parse-monkey-macro-lambda-list '(a b c &attributes attrs))
ATTRS
(A B C)
;;Destructuring
> (parse-monkey-macro-lambda-list '(&attributes (&key x y) a b c))
(&KEY X Y)
(A B C)
"
  (let ((attr-cons (member '&attributes args)))
    (values 
     (cadr attr-cons)
     (nconc (ldiff args attr-cons) (cddr attr-cons)))))

(defun generate-monkey-macro-with-attributes (name attribute-args args body)
  (with-gensyms (attributes form-body)
    (if (symbolp attribute-args) (setf attribute-args `(&rest ,attribute-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'macro-wants-attributes) t)
       (setf (get ',name 'macro) 
             (lambda (,attributes ,form-body)
               (destructuring-bind (,@attribute-args) ,attributes
                 (destructuring-bind (,@args) ,form-body
                   ,@body)))))))

(defun generate-monkey-macro-no-attributes (name args body)
  (with-gensyms (form-body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'macro-wants-attributes) nil)
       (setf (get ',name 'macro)
             (lambda (,form-body)
               (destructuring-bind (,@args) ,form-body ,@body))))))
  
(defmacro define-monkey-macro (name (&rest args) &body body)
  (multiple-value-bind (attribute-var args)
      (parse-monkey-macro-lambda-list args)
    (if attribute-var
      (generate-monkey-macro-with-attributes name attribute-var args body)
      (generate-monkey-macro-no-attributes name args body))))

(defun expand-monkey-macro-form (form)
  (if (or (consp (first form))
          (get (first form) 'macro-wants-attributes))
    (multiple-value-bind (tag attributes body) (parse-monkey-sexp form)
;      (break "shit fuck shit ~%~A~%~A~%~A" (get tag 'macro) attributes body)
      (if attributes
	  (funcall (get tag 'macro) attributes body)
	  (funcall (get tag 'macro) body)
	  ))
    (destructuring-bind (tag &body body) form
;;      (break "shit fuck shit poes")
      (funcall (get tag 'macro) body))))

(defgeneric process-attribute (processor tag attribute-tag attribute-value))

(defmethod  process-attribute ((processor processor) tag attribute-tag attribute-value))

(defgeneric process-attributes (processor tag attributes))

(defmethod process-attributes ((processor processor) tag attributes)
  (loop while (keywordp (car attributes))
     do (let ((att-tag (pop attributes))
	      (att-val (pop attributes)))
	  (process-attribute 
	   processor
	   tag
	   att-tag
	   att-val
	   ))))

(defgeneric process-body (processor tag body))

(defmethod process-body (processor tag body)
  (declare (ignore tag))
  (process processor body))

#|
process takes care of this????
(defgeneric process-sexp (processor form))

(defmethod process-sexp ((processor processor) form)
  (loop for element in form
     do (process processor element)))
|#

(defgeneric pre-process-monkey-sexp (processor sexp tag attributes body))

(defmethod pre-process-monkey-sexp ((processor processor) sexp tag attributes body))

(defgeneric post-process-monkey-sexp (processor sexp tag attributes body))

(defmethod post-process-monkey-sexp ((processor processor) sexp tag attributes body))

(defgeneric process-monkey-sexp (processor form))

(defvar *sexp-cache* nil)

(defmethod process-monkey-sexp ((processor processor) form)
   (multiple-value-bind (tag attributes body) (parse-monkey-sexp form)
     (let ((*sexp-cache* (make-hash-table :test 'equalp)))
       (pre-process-monkey-sexp processor form tag attributes body)
       (process-attributes processor tag attributes)
       (process-body processor tag body)
       (post-process-monkey-sexp processor form tag attributes body)
       )))


(defgeneric process (processor form))

(defmethod process ((processor processor) form)
 ;;(break "?? ~A" form)
  (cond
    ((language-operator-p form) (process-language-operator processor form))
    ((monkey-macro-form-p form) (process processor (expand-monkey-macro-form form)))
    ((monkey-sexp-p form) (process-monkey-sexp processor form))
    ((consp form) (first form))
    
    (t form)
    ))

(defmacro monkey-lisp ((&key processor-class) 
			 &body body) 
    
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(let ((*processor* (make-instance (or ',processor-class 'processor))))
    ;;  (break "~A" *processor*)
      (process *processor* ',body))))

#|

(defmacro monkey-lisp (&whole whole (&key processor-class) 
			 &body body) 
    
    (declare (ignore body)
	     (ignore processor-class))
    `(macrolet ((monkey-lisp ((&key (processor-class *processor-class*)) &body body)
		    (let ((*processor* (make-instance (or processor-class 'processor))))
									     
			(process *processor* body))))     
	 ,whole))
|#
