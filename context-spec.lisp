(in-package :cl-wfx)

(defclass context-spec-processor (monkey-lisp:processor)
  ())


(defmethod monkey-lisp:pre-process-monkey-sexp ((processor context-spec-processor) 
						sexp
						(tag (eql :context-spec)) 
						attributes body)
  )

(defmethod monkey-lisp:process-attribute ((processor context-spec-processor) 
					  (tag (eql :context-spec)) 
					  (attribute-tag (eql :name))
					  attribute-value)
  (setf (gethash 'name monkey-lisp::*sexp-cache*) 
	attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-spec-processor) 
					  (tag (eql :context-spec)) 
					  (attribute-tag (eql :url))
					  attribute-value)
  (setf (gethash 'url monkey-lisp::*sexp-cache*) 
	attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-spec-processor) 
					  (tag (eql :context-spec)) 
					  (attribute-tag (eql :permissions))
					  attribute-value)
  (setf (gethash 'permissions monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-spec-processor) 
					  (tag (eql :context-spec)) 
					  (attribute-tag (eql :for-everyone))
					  attribute-value)

  (setf (gethash 'for-everyone monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-spec-processor) 
					  (tag (eql :context-spec)) 
					  (attribute-tag (eql :module))
					  attribute-value)

  (setf (gethash 'module monkey-lisp::*sexp-cache*) attribute-value))


(defmethod  monkey-lisp:process-body ((processor context-spec-processor) tag body)
;;  (break "context body ~A" body)
  (setf (gethash 'body monkey-lisp::*sexp-cache*) body))


(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name context-spec
   :label "Context Spec"
   :super-classes (license-doc)
   :data-fields
   ((:name name 
	   :initarg :name
	   :accessor name
	   :initform nil
	   :db-type string
	   :key t
	   :display t
	   :editable t
	   )
    (:name script 
	   :initarg :script
	   :accessor script
	   :initform nil
	   :db-type script
	   :display t
	   :editable t
	   )
    (:name permissions 
	   :initarg :permissions
	   :accessor permissions
	   :initform nil
	   :db-type (list permission)
	   :display t
	   :editable t
	   )
    (:name url 
	   :initarg :url
	   :accessor url
	   :initform nil
	   :db-type string
	   :display t
	   :editable t
	   )
    (:name for-everyone 
	   :initarg :for-everyone
	   :accessor for-everyone
	   :initform nil
	   :db-type boolean)
    (:name args 
	   :initarg :args
	   :accessor args
	   :initform ()
	   :db-type (list string)
	   :display t
	   :editable t
	   ))
     
   
   :metaclass xdb2:storable-versioned-class
   :collection-name "context-specs"
   :collection-type :merge
   :default-initargs (:top-level t)))

(defgeneric context-spec-script (data-spec))
