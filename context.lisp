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


(defun myremove (tree)
  (if (atom tree)
      tree
      (mapcar (lambda (subtree) (myremove subtree))
              (remove-if 'not tree))))

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-spec-processor) 
						sexp
						(tag (eql :context-spec)) 
						attributes body)
  

  (let* ((name (gethash 'name monkey-lisp::*sexp-cache*))
	 (context-spec (fetch-item 'context-spec
				   :test (lambda (doc)
					   (equalp (name doc) name)))))
    
    
    (when context-spec
     
      (setf (script context-spec) body)
      
      (setf (permissions context-spec) 
	    (gethash 'permissions monkey-lisp::*sexp-cache*))
      (setf (for-everyone context-spec) 
	    (gethash 'for-everyone monkey-lisp::*sexp-cache*)))
    
    (unless context-spec
      (setf context-spec 
	    (make-instance 'context-spec 
			   :name name
			   :script body
			   :permissions 
			   (gethash 'permissions monkey-lisp::*sexp-cache*)
			   :for-everyone 
			   (gethash 'for-everyone monkey-lisp::*sexp-cache*))))
    
 
    
    (setf (permissions context-spec) (gethash 'permissions monkey-lisp::*sexp-cache*))
      
    (when *system*   
      (persist-data context-spec)

      )))




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
	   :key t)
    (:name script 
	   :initarg :script
	   :accessor script
	   :initform nil
	   :db-type script)
    (:name permissions 
	   :initarg :permissions
	   :accessor permissions
	   :initform nil
	   :db-type (list permission))
    (:name url 
	   :initarg :url
	   :accessor url
	   :initform nil
	   :db-type string)
    (:name for-everyone 
	   :initarg :for-everyone
	   :accessor for-everyone
	   :initform nil
	   :db-type boolean)
    (:name args 
	   :initarg :args
	   :accessor args
	   :initform ()
	   ;;:db-type (list string)
	   ))
     
   
   :metaclass xdb2:storable-versioned-class
   :collection-name "context-specs"
   :collection-type :merge
   :default-initargs (:top-level t)))




(defclass context ()
  ((context-id :initarg :context-id
	       :accessor context-id
	       :initform (random 10810))
   (module :initarg :module
	   :accessor module
	   :initform nil)
   (context-spec :initarg :context-spec
		       :accessor context-spec
		       :initform nil)
   (session :initarg :session
	    :accessor session
	    :initform nil)
   (url :initarg :url
	:accessor url
	:initform nil)
   (new-p :initarg :new-p
	  :accessor new-p
	  :initform t)
   (reuse-p :initarg :reuse-p
	    :accessor reuse-p
	    :initform nil)
   (widget-sequence :initarg :widget-sequence
		    :accessor widget-sequence
		    :initform nil)
   (cache :initarg :cache
	     :accessor cache
	     :initform (make-hash-table :test #'equalp)))
  (:documentation "An instance of a context within the current user session."))

(defgeneric start-context (module session context-name &key &allow-other-keys)
  (:documentation "Creates a context instance."))

(defgeneric setup-context (module context-spec system)
  (:documentation "To be used to setup a context instance.."))

(defgeneric init-context (context session )
  (:documentation "To be used to setup a context instance.."))

;;TODO: called by system (in init.lsip) to init permissions and ????
(defmethod init-context ((context context) session)
    ;;TODO: Go look in old insite code what needs to go here? Page setup stuff?
 ;; (setf  (permissions contexat) nil)
  
 ;;  (setf (request-page-permissions context)        (setup-page-permissions context))
  )

