(in-package :cl-wfx)

(defun parse-slots (sexp)
  (let ((slots))
    (dolist (slot sexp)
      (let ((name (first slot))
	    (attributes (cdr slot)))

	(setf slots (pushnew `(,name 
			       :initarg ,(getf attributes :initarg)
			       :accessor ,(getf attributes :accessor)
			       :initform ,(getf attributes :initform)
			       :key ,(getf attributes :key)
			       :db-type ,(getf attributes :db-type))
			     slots))))
    slots))

(defmethod monkey-lisp:pre-process-monkey-sexp ((processor data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						attributes body)
  )


(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :data-fields))
					  attribute-value)
 ;; (break "dataspec ~A ~A" attribute-tag attribute-value)
  (let ((slots))
    (dolist (field attribute-value)
      
      (setf slots (pushnew `(,(getf field :name) 
			     :initarg ,(getf field :initarg)
			     :accessor ,(getf field :accessor)
			     :initform ,(getf field :initform)
			     :key ,(getf field :key)
			     :db-type ,(getf field :db-type))
			   slots)))
    (setf (gethash 'slots monkey-lisp::*sexp-cache*) slots)))

(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :name))
					  attribute-value)
;;  (break "dataspec ~A ~A" attribute-tag attribute-value)
  (setf (gethash 'name monkey-lisp::*sexp-cache*) 
	attribute-value))

(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :super-classes))
					  attribute-value)
  (setf (gethash 'super-classes monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :metaclass))
					  attribute-value)
  
  (setf (gethash 'metaclass monkey-lisp::*sexp-cache*) attribute-value)
  
  (setf (gethash 'class-options monkey-lisp::*sexp-cache*) 
	(pushnew (list attribute-tag attribute-value )
		 (gethash 'class-options monkey-lisp::*sexp-cache*))))

(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :default-initargs))
					  attribute-value)
;;  (break "dataspec ~A ~A" attribute-tag attribute-value)
  (setf (gethash 'default-initargs monkey-lisp::*sexp-cache*) attribute-value)
  
  (setf (gethash 'class-options monkey-lisp::*sexp-cache*) 
	(pushnew (append (list attribute-tag) attribute-value)
		 (gethash 'class-options monkey-lisp::*sexp-cache*))))


(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-name))
					  attribute-value)
;;  (break "dataspec ~A ~A" attribute-tag attribute-value)
  (setf (gethash 'collection-name monkey-lisp::*sexp-cache*) attribute-value)
  #|
  (setf (gethash 'class-options monkey-lisp::*sexp-cache*) 
	(pushnew (list attribute-tag attribute-value )
		 (gethash 'class-options monkey-lisp::*sexp-cache*)
		 ))
  |#
  )

(defmethod monkey-lisp:process-attribute ((processor data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-type))
					  attribute-value)
;;  (break "dataspec ~A ~A" attribute-tag attribute-value)
  (setf (gethash 'collection-type monkey-lisp::*sexp-cache*) attribute-value)
  #|
  (setf (gethash 'class-options monkey-lisp::*sexp-cache*) 
	(pushnew  (list attribute-tag attribute-value )
		  (gethash 'class-options monkey-lisp::*sexp-cache*)))
  |#
  )


(defmethod  monkey-lisp:process-body ((processor data-spec-processor) tag body)
  (declare (ignore tag) (ignore body))
;;  (break "dataspec body ~A" body)
  (let ((class-name (gethash 'name monkey-lisp::*sexp-cache*))
	(classes (gethash 'super-classes monkey-lisp::*sexp-cache*))
	(slots (gethash 'slots monkey-lisp::*sexp-cache*))
	(class-options (gethash 'class-options monkey-lisp::*sexp-cache*)))
     
    (eval
     (if classes
	 `(defclass ,class-name ,classes 
	      (,@(parse-slots slots))
	      ,@class-options)
	     
	 `(defclass ,class-name ()
	    (,@(parse-slots slots))
	    ,@class-options)))))


(defmethod monkey-lisp:post-process-monkey-sexp ((processor data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						attributes body)
  
;;    (break "dataspec ~A~%~A" attributes body)
  
  (let* ((name (gethash 'name monkey-lisp::*sexp-cache*))
	 (data-spec (fetch-item "data-specs"
				:test (lambda (doc)
					(equalp (slot-value doc 'name) name)))))
    
    (when data-spec
      (setf (slot-value data-spec 'script) sexp)
      (setf (collection-name data-spec)
	    (gethash 'collection-name monkey-lisp::*sexp-cache*)
	    )
      (setf (collection-type data-spec)
	    (gethash 'collection-type monkey-lisp::*sexp-cache*)))
    
    (unless data-spec
      (setf data-spec (make-instance 'data-spec 
				     :name name
				     :collection-name 
				     (gethash 'collection-name monkey-lisp::*sexp-cache*)
				     :collection-type 
				     (gethash 'collection-type monkey-lisp::*sexp-cache*)
				     :script sexp)))
   
    (unless *system*
      ;;Put data specs in temp store for loading into db on startup.
      (pushnew data-spec *data-specs*))
    
    (when *system*      
      (persist-data data-spec))
    )
  )

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
    
    (:data-spec
     :name data-spec
     :label "Data Spec"
     :super-classes (license-doc)
     :data-fields
     ((:name name
	     :initarg :name
	     :accessor name
	     :label "Name"
	     :key t
	     :db-type string)
      (:name collection-type
	     :initarg :collection-type
	     :accessor collection-type
	     :initform nil
	    )
      (:name collection-name
	     :initarg :collection-name
	     :accessor collection-name
	     :initform nil)
      (:name script
	     :initarg :script
	     :accessor script
	     :initform nil
	     :db-type script))
     
     :after-persist #'(lambda (doc)	
			(when doc
			  `(read-sexp (script doc))		
			  ;;(create-context-definition doc (name doc))
			  ))
     :metaclass xdb2:storable-versioned-class
     :collection-name "data-specs"
     :collection-type :merge
     :default-initargs (:top-level t)))


