(in-package :monkey-html-lisp)

(defclass html-processor (output-processor)
  ()
  (:default-initargs 
   :printer-class 'pretty-printer
    :indenter-class 'indenting-printer))

(defparameter *xhtml* nil)

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")
(defvar *escapes* *element-escapes*)

(defparameter *block-elements*
  '(:body :colgroup :dl :fieldset :form :head :html :div :map :noscript :object
    :ol :optgroup :pre :script :select :style :table :tbody :tfoot :thead
    :tr :ul))

(defparameter *paragraph-elements*
  '(:area :base :blockquote :br :button :caption :col :dd :div :dt :h1
    :h2 :h3 :h4 :h5 :h6 :hr :input :li :link :meta :option :p :param
    :td :textarea :th :title))

(defparameter *inline-elements*
  '(:a :abbr :acronym :address :b :bdo :big :cite :code :del :dfn :em
    :i :img :ins :kbd :label :legend :q :samp :small :span :strong :sub
    :sup :tt :var))

(defparameter *empty-elements*
  '(:area :base :br :col :hr :img :input :link :meta :param))

(defparameter *preserve-whitespace-elements* 
  '(:pre :script :style))

(defun block-element-p (tag) 
  (find tag *block-elements*))

(defun paragraph-element-p (tag) 
  (find tag *paragraph-elements*))

(defun empty-element-p (tag) 
  (find tag *empty-elements*))

(defun preserve-whitespace-p (tag) 
  (find tag *preserve-whitespace-elements*))

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'needs-escape-p in :start start)
            do (write-sequence in out :start start :end pos)
            when pos do (write-sequence (escape-char (char in pos)) out)
            while pos))))

(define-output-operator :raw-string
    (:output-op-code 
     (lambda (string &optional check-for-newlines)
       (if (get-pretty-printer)
	   `(output-op-emit :raw-string ,string ,check-for-newlines)
	   `(write-sequence ,string *emit-output*))) 
     :output-op-emit 
     (lambda (string &optional newlines-p)	
       (if newlines-p
	   (emit (get-indent-printer) string)
	   (emit/no-newlines (get-indent-printer) string)))))

(define-output-operator :newline
    (:output-op-code 
     (lambda ()
       (if (get-pretty-printer)
	   `(output-op-emit :newline))) 
     :output-op-emit 
     (lambda ()	
       (emit-newline (get-indent-printer)))))

(define-output-operator :freshline
    (:output-op-code 
     (lambda ()
       (if (get-pretty-printer)
	   `(output-op-emit :freshline))) 
     :output-op-emit 
     (lambda ()	
       (unless (beginning-of-line-p (get-indent-printer))
	 (emit-newline (get-indent-printer))))))

(define-output-operator :indent
    (:output-op-code 
     (lambda ()
       (if (get-pretty-printer)
	   `(output-op-emit :indent))) 
     :output-op-emit 
     (lambda ()	
       (incf (indentation (get-indent-printer)) 
	     (tab-width (get-pretty-printer))))))

(define-output-operator :unindent
    (:output-op-code 
     (lambda ()
       (if (get-pretty-printer)
	   `(output-op-emit :unindent))) 
     :output-op-emit 
     (lambda ()	
       (decf (indentation (get-indent-printer)) 
	     (tab-width (get-pretty-printer))))))

(define-output-operator :toggle-indenting
    (:output-op-code 
     (lambda ()
       (if (get-pretty-printer)
	   `(output-op-emit :toggle-indenting))) 
     :output-op-emit 
     (lambda ()	
       (with-slots (indenting-p) 
	   (get-indent-printer)
	 (setf indenting-p (not indenting-p))))))

(define-output-operator :embed-value
    (:output-op-code            
     (lambda (value &optional escapes)
       (if (get-pretty-printer)
           (if escapes
               `(output-op-emit :raw-string (escape (princ-to-string ,value) escapes) t)
               `(output-op-emit :raw-string (princ-to-string ,value) t)))) 
     :output-op-emit 
     (lambda (value &optional escapes)
       (declare (ignore escapes))
       value)))

(define-output-operator :embed-code
    (:output-op-code 
     
     (lambda (code)         
       `(output-op-emit :raw-string (eval ,code))) 
     :output-op-emit 
     (lambda (code)
      ;; (break "?? ~A" (eval code))
      code)))

(define-language-operator 'html-processor :print
    (lambda (processor value)
      (declare (ignore processor))
      (output-op-code :raw-string `,(princ-to-string value) t)))

(define-language-operator 'html-processor :format
    (lambda (processor &rest args)
      (declare (ignore processor))
      (output-op-code :raw-string `(format nil ,@args))))


(defun emit-attributes (processor attributes)
  (loop for (k v) on attributes by #'cddr do
       
       (output-op-code :raw-string (format nil " ~(~a~)='" k))
       (let ((*escapes* *attribute-escapes*))

	 (process processor (if (eql v t) 
				(string-downcase k) 
				v))
	 )
       (output-op-code :raw-string "'")))

(defun emit-open-tag (processor tag body-p attributes)
  (when (or (paragraph-element-p tag) (block-element-p tag))
   (output-op-code :freshline))
  (output-op-code :raw-string (format nil "<~(~a~)" tag))
  (emit-attributes processor attributes)
  (output-op-code :raw-string (if (and *xhtml* (not body-p)) "/>" ">")))

(defun emit-element-body (processor tag body) 
  (when (block-element-p tag)
    (output-op-code :freshline)
    (output-op-code :indent))
  (when (preserve-whitespace-p tag) 
    (output-op-code :toggle-indenting))
  (dolist (item body)  
    (process processor item))
  (when (preserve-whitespace-p tag) 
    (output-op-code :toggle-indenting))
  (when (block-element-p tag)
    (output-op-code :unindent)
    (output-op-code :freshline)))

(defun emit-close-tag (processor tag body-p)
  (declare (ignore processor))
  (unless (and (or *xhtml* 
		   (empty-element-p tag)) (not body-p))
    (output-op-code :raw-string (format nil "</~(~a~)>" tag)))
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (output-op-code :freshline)))



(defmethod process-monkey-sexp ((processor html-processor) form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  
  (multiple-value-bind (tag attributes body) (parse-monkey-sexp form)
    (emit-open-tag     processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag    processor tag body)))

(defmethod process-self-form ((processor html-processor) form)
  (output-op-code :raw-string 
	   (escape (princ-to-string form) *escapes*) t))

;;;;%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^%^

(defvar *emit-output-htm* *standard-output*)

(defmacro htm (&body body)
 ` (let ((*emit-output-htm* (make-string-output-stream )))
     
    (monkey-output (:processor-class html-processor)
       ,@body)
    (get-output-stream-string *emit-output-htm*))
  )

(defmacro with-html (&body body)   
  `(let ((monkey-output-lisp::*emit-output* (make-string-output-stream )))
     (monkey-output-lisp (:processor-class html-processor)
       ,@body)
     (get-output-stream-string monkey-output-lisp::*emit-output*)))

#|
(monkey-html-lisp:with-html 
  (:html
   (:body
    (:div (monkey-html-lisp:htm 
	    (format nil "aaah - ~A" "shit")))
    (:div :class "some-class"
     (let ((x (random 10)))
       (if (> x 5)
	   (monkey-html-lisp:htm (:p (:print "arst")))
	   (monkey-html-lisp:htm (:p (:print "arstarst")))
	   )))
    
    (:div :class (if (> (random 10) 5)
		     (monkey-html-lisp:htm "someless5")
		     (monkey-html-lisp:htm "somegreat5")
		     )
     (let ((x (random 10)))
       (if (> x 5)
	   (monkey-html-lisp:htm (:p "arsggggggggt"))
	   (monkey-html-lisp:htm (:p "araaaaaaaaaaast"))
	   )))
    
    (:div :class (if (> (random 10) 5)
		     "shit"
		     "fuck"
		      )
     (:format "~A" "fuck")
     (:div
      (:if (> (random 10) 5)
		      "shit"
		      "fuck"
		      )
      )
     )))
  )
|#
