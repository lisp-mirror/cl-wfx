(in-package :monkey-output-lisp)

(defvar *emit-output* *standard-output*)

(defun make-op-buffer () 
  (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buffer) 
  (vector-push-extend op ops-buffer))


(defclass output-processor (processor)
  ((ops :accessor ops 
	:initform (make-op-buffer))
   (pretty-printer :initarg :pretty-printer 
		   :accessor pretty-printer 
		   :initform nil)
   (printer-class :initarg :printer-class
		  :accessor printer-class
		  :initform nil)
   (indenter-class :initarg :indenter-class
		  :accessor indenter-class
		  :initform nil)))

(defclass pretty-printer ()
  ((indent-printer   :accessor indent-printer   :initarg :indent-printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2)))

(defclass indenting-printer ()
  ((out                 :accessor out                 :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation         :accessor indentation         :initform 0)
   (indenting-p         :accessor indenting-p         :initform t)))

(defun emit (ip string)
  (loop for start = 0 then (1+ pos)
     for pos = (position #\Newline string :start start)
     do (emit/no-newlines ip string :start start :end pos)
     when pos do (emit-newline ip)
     while pos))

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
  
    (loop repeat (indentation ip) 
       do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

(defun get-output-op-code (op)
  (get op 'output-op-code))

(defun get-output-op-emit (op)
  (get op 'output-op-emit))


(defun output-op-code (op &rest operands)
  (when (get-pretty-printer)
    (push-op `(,op  ,(apply (get-output-op-code op) operands)) (ops *processor*))))

(defun output-op-emit (op &rest operands)
  (apply (get-output-op-emit op) operands))

(defun get-pretty-printer ()
  (pretty-printer *processor*))

(defun get-indent-printer ()
  (indent-printer (pretty-printer *processor*)))

;;output-op-code is for use by the processor compiler (written to *emit-output*) and 
;;op-mit is what the operator puts out.

(defmacro define-output-operator (op (&key output-op-code output-op-emit) )  
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ,op 'output-op-code)
           ,output-op-code)
     (setf (get ,op 'output-op-emit)
	     ,output-op-emit)))

(defun output-generate-code (ops) 

  (loop for op across ops 
     collect 
       (if (stringp (second op))
	   `(output-op-emit :raw-string ,(second op) nil)
	   (second op)
	   )))

(defun compile-buffer (buf ops)
  "Compile a string possibly containing newlines into a sequence of
:raw-string and :newline ops."
  (loop with str = (get-output-stream-string buf)
     for start = 0 then (1+ pos)
     for pos = (position #\Newline str :start start)
     when (< start (length str))
     do (push-op `(:raw-string ,(subseq str start pos) nil) ops)
     when pos do (push-op '(:newline) ops)
     while pos))

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op) 
               (compile-buffer buf new-ops)
	       (push-op op new-ops)))
	
        (loop for op across ops do
             (ecase (first op)
               (:raw-string 
	
		(if (stringp (third (second op)))
		    (write-sequence (third (second op)) buf)
		    (add-op op)
		    ))
               ((:newline :embed-value :embed-code) (add-op op))
               ((:indent :unindent :freshline :toggle-indenting)		
                (add-op op))))
;;	(break "a ~A" new-ops)
	(compile-buffer buf new-ops)
;;	(break "b ~A" new-ops)
        ))
    new-ops))

;;TODO: figure out optomize shit
(defun output-codegen (ops)
  `(progn ,@(output-generate-code ;;ops 
				  (optimize-static-output ops)
				  
				  ) nil))
  
(defun sexp-ops (body &optional processor-class)
  (let ((*processor* (create-processor processor-class)))
   
       (loop for form in body 
	  do (process *processor* form)
	  finally (return (ops *processor*)))))


(defmethod process ((processor output-processor) form)
  (cond
    ((language-operator-p form) (process-language-operator processor form))
    ((monkey-macro-form-p form) (process processor (expand-monkey-macro-form form)))
    ((monkey-sexp-p form) (process-monkey-sexp processor form))
    ((consp form) (output-op-code :embed-code form))
;;    ((stringp form) (output-op-code :raw-string form ))
    (t 
     (output-op-code :embed-value form))
    ))

(defun create-processor (processor-class)
  (let ((processor (make-instance 
		      (or processor-class 'output-processor))))
    (when (printer-class processor)
      (setf (pretty-printer processor)	   
	    (make-instance 
	     (printer-class processor)
	     :indent-printer (if (indenter-class processor)
				 (make-instance 'indenting-printer 
						:out *emit-output*)))))
    processor
    )
  )

(defmacro monkey-output ((&key processor-class) 
			      &body body) 
  (output-codegen (sexp-ops body processor-class)))

(defmacro monkey-output-lisp ((&key processor-class) 
			      &body body) 
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (let ((*processor* (create-processor processor-class)))    
      `(let ((*processor* (create-processor ',processor-class)))
	 ,(output-codegen (sexp-ops body processor-class))))))
