(in-package :cl-wfx)


(defclass context-data-spec-processor (monkey-lisp:processor)
  ())


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :name))
					  attribute-value)
  (setf (gethash 'data-spec-name monkey-lisp::*sexp-cache*) 
	attribute-value))


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-name))
					  attribute-value)
  (setf (gethash 'collection-name monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :data-fields))
					  attribute-value)
  (setf (gethash 'fields monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						 attributes body)
  (render-grid :permissions '(:update :delete)))



(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-spec-processor) 
						sexp
						(tag (eql :context-spec)) 
						attributes body)
  

  (let* ((name (gethash 'name monkey-lisp::*sexp-cache*))
	 (context-spec (fetch-item 'context-spec
				   :test (lambda (doc)
					   (equalp (name doc) name)))))
    
    
    (when context-spec
     
      (setf (script context-spec) (if (consp (first body))
				      (first body)
				      body) )
      
      (setf (permissions context-spec) 
	    (gethash 'permissions monkey-lisp::*sexp-cache*))
      (setf (for-everyone context-spec) 
	    (gethash 'for-everyone monkey-lisp::*sexp-cache*)))
    
    (unless context-spec
      (setf context-spec 
	    (make-instance 'context-spec 
			   :name name
			   :script (if (consp (first body))
				      (first body)
				      body)
			   :permissions 
			   (gethash 'permissions monkey-lisp::*sexp-cache*)
			   :for-everyone 
			   (gethash 'for-everyone monkey-lisp::*sexp-cache*))))
    
     (setf (permissions context-spec) (gethash 'permissions monkey-lisp::*sexp-cache*))
      
    (when *system*   
      (persist-data context-spec))))

(defmethod context-spec-script ((spec context-spec))
  (if spec
      (script spec)))


(defmethod context-spec-data-script ((spec data-spec))
  (if spec
      (getf (cdr (script spec)) :data-spec)))
