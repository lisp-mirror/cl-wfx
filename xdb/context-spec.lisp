(in-package :cl-wfx)


(defclass context-data-spec-processor (monkey-lisp:processor)
  ())


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :name))
					  attribute-value)
  (setf (gethash :name monkey-lisp::*sexp-cache*) 
	attribute-value))


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-name))
					  attribute-value)
  (setf (gethash :collection-name monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-type))
					  attribute-value)
  (setf (gethash :collection-type monkey-lisp::*sexp-cache*) attribute-value))


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :super-classes))
					  attribute-value)
  (setf (gethash :super-classes monkey-lisp::*sexp-cache*) attribute-value))

(defun append-super-fields (classes)
  (let ((super-fields))
     (dolist (class classes)
      (let ((data-spec (get-data-spec class)))
	(when data-spec
	  (let ((fields (getf (cdr (data-spec-script 
				    data-spec))
			      :data-fields))
		(super-classes (getf (cdr (data-spec-script 
					   data-spec))
				     :super-classes)))
	    (when super-classes 
	      (setf super-fields (append super-fields
					 (append-super-fields super-classes))))
	    (setf super-fields (append super-fields fields))))))
     super-fields
     ))

(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :data-fields))
					  attribute-value)
  (let ((fields))
    (when (gethash :super-classes monkey-lisp::*sexp-cache*)
      (let ((super-fields (append-super-fields 
			   (gethash :super-classes monkey-lisp::*sexp-cache*))))
	(setf fields (append super-fields 
			     attribute-value))))
    
    (setf (gethash :data-fields monkey-lisp::*sexp-cache*) 
	  (or fields attribute-value))))

(defun context-data-spec (spec-name)
  (gethash spec-name (cache *context*)))

(defun set-context-data-spec-attribute (spec-name attribute value)
  (let ((spec (read-symbol-from-string spec-name)))
    (setf (gethash attribute (gethash (frmt "~S" spec) (cache *context*)) ) value)))

(defun get-context-data-spec-attribute (spec-name attribute)
  (let ((spec (read-symbol-from-string spec-name)))
    (when (gethash (frmt "~S" spec) (cache *context*))
	  (gethash attribute (gethash (frmt "~S" spec) (cache *context*))))))

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						 attributes body)
  (let* ((spec-name (gethash :name monkey-lisp::*sexp-cache*))
	 (data-spec-hash (or
			  (gethash (frmt "~S" spec-name) (cache *context*))
			  (make-hash-table :test #'equalp))))
    
    (setf (gethash :data-spec data-spec-hash ) 
	  (get-data-spec (gethash :name monkey-lisp::*sexp-cache*)))
    (setf (gethash :name data-spec-hash ) 
	  (gethash :name monkey-lisp::*sexp-cache*))
    (setf (gethash :collection-name data-spec-hash ) 
	  (gethash :collection-name monkey-lisp::*sexp-cache*))
    (setf (gethash :collection-type data-spec-hash ) 
	  (gethash :collection-type monkey-lisp::*sexp-cache*))
    (setf (gethash :data-fields data-spec-hash ) 
	  (gethash :data-fields monkey-lisp::*sexp-cache*))
  
    (setf (gethash (frmt "~S" spec-name) (cache *context*)) data-spec-hash)

    ))


(defun parse-data-spec-for-grid (spec-name)
  (monkey-lisp::monkey-lisp-immediate
	:processor-class 'cl-wfx::context-data-spec-processor
      :body (data-spec-script 
	     (get-data-spec 
	      spec-name))))

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-spec-processor) 
						sexp
						(tag (eql :context-spec)) 
						attributes body)
  

  (let* ((name (gethash 'name monkey-lisp::*sexp-cache*))
	 (context-spec (fetch-item 'context-spec
				   :test (lambda (doc)
					   (equalp (name doc) name)))))
    
    
    (when context-spec
     
      (setf (script context-spec) 
	    (if (consp (first body))
		(first body)
		body) )
      
      (setf (permissions context-spec) 
	    (gethash 'permissions monkey-lisp::*sexp-cache*))
      (setf (for-everyone context-spec) 
	    (gethash 'for-everyone monkey-lisp::*sexp-cache*))
      (setf (url context-spec) (gethash 'url monkey-lisp::*sexp-cache*))
      )
    
    
    (unless context-spec
      (setf context-spec 
	    (make-instance 'context-spec
			   :license-code (if (and (active-user) 
						  (license-codes (active-user)))
					     (first (license-codes (active-user)))
					     *sys-license-code*)
			   :name name
			   :script (if (consp (first body))
				      (first body)
				      body)
			   :url (gethash 'url monkey-lisp::*sexp-cache*)
			   :permissions 
			   (gethash 'permissions monkey-lisp::*sexp-cache*)
			   :for-everyone 
			   (gethash 'for-everyone monkey-lisp::*sexp-cache*))))
    
     (setf (permissions context-spec) (gethash 'permissions monkey-lisp::*sexp-cache*))
      
    (when *system*   
      (persist-data context-spec :collection-name "context-specs"))))

(defmethod context-spec-script ((spec context-spec))
  (if spec
      (script spec)))


(defmethod context-spec-data-script ((spec data-spec))
  (if spec
      (getf (cdr (script spec)) :data-spec)))
