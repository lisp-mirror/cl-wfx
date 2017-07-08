(in-package :cl-wfx)

(defclass xdb-data (data xdb2:dbs)
  ((data-folder :initarg :data-folder
		   :accessor data-folder
		   :initform "~~/xdb2/")))

(defmethod setup-data ((data xdb-data) &key &allow-other-keys)
  (maphash (lambda (key db)
	     (declare (ignore key))
	     (xdb2:load-collections db)
	     (xdb2:clear-db-cache db))
	   (xdb2::databases data)))

(defmethod is-initialized-system-data ((data xdb-data) &key &allow-other-keys)
 
  (xdb2:get-db (data *system*)
	       (list (frmt "~A" (id-string (system-name *system*)))
		     *sys-license-code*))
  )


(defmethod init-system-data ((data xdb-data) &key &allow-other-keys)
  (let ((store (xdb2:add-db (data *system*) 
	       (list (frmt "~A" (id-string (system-name *system*)))
		     *sys-license-code*))))
    (when store
      (dolist (data-spec (fetch-all "data-specs" :result-type 'list))
	(when (equalp (collection-type data-spec) :system)
	  (license-collection (collection-name data-spec))))
      (xdb2:load-collections store)
      (xdb2:clear-db-cache store))
    store))


(defmethod is-initialized-license-data ((data xdb-data) &key &allow-other-keys)
  (xdb2:get-db (data *system*)
	       (list (frmt "~A" (id-string (system-name *system*)))
		     (current-license-code))))


(defmethod init-license-data ((data xdb-data) &key &allow-other-keys)
  (let ((store (xdb2:add-db (data *system*) 
	       (list (frmt "~A" (id-string (system-name *system*)))
		     (current-license-code)))))
    (when store
      (dolist (data-spec (fetch-all "data-specs" :result-type 'list))
	(when (or
	       (equalp (collection-type data-spec) :merge)
	       (equalp (collection-type data-spec) :license))
	  (license-collection (collection-name data-spec))))      
      (xdb2:load-collections store)
      (xdb2:clear-db-cache store))
    store))

(defmethod tear-down-data ((data xdb-data) &key &allow-other-keys))

(defmethod persist-data ((doc xdb2:storable-object) &key &allow-other-keys)
  (xdb2:persist doc) )

(defmethod persist-data ((doc xdb2:storable-versioned-object) &key &allow-other-keys)
  (xdb2:persist doc) )

(defmethod delete-data ((doc xdb2:storable-object) &key &allow-other-keys)
  (xdb2:remove-doc doc))

(defmethod get-key-value ((item xdb2:storable-object) data-spec &key &allow-other-keys)
  (let ((keys))
    (dolist (field (data-spec-fields data-spec))
      (when (getf field :key)
	(pushnew (slot-value item (getf field :name)) keys)))
    keys))

(defun system-collection (collection)
  (let ((col
	 (xdb2:get-collection (system-data (data *system*)) collection)))
    (unless col
      (setf col (xdb2:add-collection (system-data (data *system*)) collection
				     :force-load t)))
    col))

(defun license-collection (collection)  
  (let ((col
	 (xdb2:get-collection (license-data (data *system*)) collection)))
    (unless col
      (setf col (xdb2:add-collection (license-data (data *system*)) collection
				     :force-load t)))
    col))

(defmethod system-data-items (collection)
  (when (system-collection collection)
    (xdb2:docs (system-collection collection))))

(defmethod license-data-items (collection)
  (when (license-collection collection)
    (xdb2:docs (license-collection collection))))


(defmethod fetch-all* ((data xdb-data) collection &key (result-type 'vector) 
						    &allow-other-keys)
  (let* ((data-spec (get-data-spec collection))	
	 (script (if data-spec (cdr (script data-spec))))
	 (entity-p (if script 
		       (if (find :super-classes script :test #'equalp)
			   (find 'entity-doc (getf script :super-classes) :test #'equalp)
			   nil)))
	 (collection-type (if data-spec
			      (collection-type data-spec)))
	 (items)) 
   
    (when data-spec
      (cond ((equalp collection-type :merge)
	     (let* ((sys-collection 
		    (system-collection (collection-name data-spec)))
		   (lic-collection 
		    (license-collection (collection-name data-spec)))
		   (docs (merge-items (and sys-collection (xdb2:docs sys-collection))
			    (and lic-collection (xdb2:docs lic-collection))
			    data-spec
			    result-type)))	       
	       (if entity-p
		   (setf items (remove-if-not #'match-context-entities docs))
		   (setf items docs)))
	     
	    
	     )
	    ((equalp collection-type :system)
	     
	     (let ((col
		    (system-collection (collection-name data-spec))))
	      
	       (if col
		   (setf items
			 (coerce (if entity-p
				     (remove-if-not #'match-context-entities 
						    (xdb2:docs col))
				     (xdb2:docs col))
				 result-type))))
	    
	     )
	    ((equalp collection-type :license)
	     
	     (let ((col 
		    (license-collection (collection-name data-spec))))
	      
	       (if col
		   (if (xdb2:docs col)
		       (setf items
			     (coerce 
			      (if entity-p
				  (remove-if-not #'match-context-entities 
						 (xdb2:docs col))
				  (xdb2:docs col))
			      result-type))))
	       
	     )
	     )
	    (t
	     (break "Collection type not set. --- data-spec ~%~A~A~A"
		    (name data-spec) (collection-name data-spec) collection-type)
	     (error "Collection type not set. --- data-from-class")))

      ;;TODO: Should sort be applied here?????????
     
      items)))

(defmethod fetch-items* ((data xdb-data) collection &key test result-type 
						      &allow-other-keys)
  (let ((docs (fetch-all* data collection :test test :result-type result-type)))
    (if test
	(remove-if 
	 #'not 
	 (map
	  (or result-type 'list)
	  (lambda (doc)
		      (funcall test doc))
	  docs)))))

(defmethod fetch-item* ((data xdb-data) collection &key test result-type &allow-other-keys)
  (let ((docs (fetch-all* data collection :test test 
			  :result-type (or result-type 'vector))))
    
    (when docs
      (if test
	  (map
	   nil
	   (lambda (doc)
	     (when (funcall test doc)
	       (return-from fetch-item* doc)))
	   docs)))
    ))

(defun fetch-data-merge (function collection-name 
			 &key (merge-eql-func #'get-key-value) 
			  (result-type 'vector))
  
  (let* ((system (system-collection collection-name))
	 (license (license-collection collection-name))
	 (sys-docs (if system 
		       (remove-if #'not (map result-type function (xdb2:docs system)))))
	 (lic-docs (if license 
		       (remove-if #'not (map result-type function (xdb2:docs license))))))
    
    (concatenate result-type 
		 (remove-if (lambda (doc)
			      (find doc lic-docs 
				    :test (lambda (doc tdoc)
					    
					    (equalp (funcall merge-eql-func doc)
						    (funcall merge-eql-func tdoc)))))
			    sys-docs) lic-docs)))

;;TODO: find all the dolist coerce and replace with find-docsx and loop?
(defmethod fetch-data* ((db xdb-data) &key class-name test (result-type 'vector)
				(merge-eql-func #'get-key-value))
  
  (let* ((class (find-class class-name))
	 (collection-name (car (collection-name class)))
	 (collection-type (car (collection-type class))))
    
    (cond ((equalp collection-type :merge)
	   (fetch-data-merge test collection-name
			    :merge-eql-func merge-eql-func
			    :result-type result-type))
	  ((equalp collection-type :system)
	  
	   (let* ((collection (system-collection collection-name))
		  (docs (if collection 
			    (xdb2:docs collection)) ))
	     (when docs
	       (remove-if #'not (map result-type test docs)))))
	  ((equalp collection-type :license)
	   
	   
	   (let* ((collection (license-collection collection-name))
		  (docs (if collection 
			    (xdb2:docs collection)) ))
	     
	     (when docs
	       (remove-if #'not (map result-type test docs)) )))
	  (t
	   (break "Collection type not set. --- find-docsx~%~A~A~A"
		  class collection-name collection-type)
	   (error "Col-type not set.")))))


(defun data-from-class (class-name)
  (let* ((class (find-class class-name))	
	 (collection-name (car (collection-name class)))
	 (collection-type (car (collection-type class)))) 
    
    
    (cond ((equalp collection-type :merge)
	   (fetch-merge-data-items collection-name))
	  ((equalp collection-type :system)	
	   (let ((collection 
		  (system-collection collection-name)))
	     (xdb2:docs collection)))
	  ((equalp collection-type :license)
	   (let ((collection 
		  (license-collection collection-name)))
	     (xdb2:docs collection)))
	  (t
	   (break "Collection type not set. --- data-from-class~%~A~A~A"
		  class collection-name collection-type)
	   (error "Collection type not set. --- data-from-class")))))


(defun collection-from-class (doc-class)
  (let* ((col-type (car (collection-type doc-class)))
	 (collection-name (car (collection-name doc-class))))
      
      (cond ((equalp col-type :system)
	     (system-collection collection-name))
	    ((equalp col-type :license)
	     (license-collection collection-name))
	    ((equalp col-type :merge)
	   
	     (if (current-user)
		 (if (and (license (current-user)) 
			  (string-equal (license-code (license (current-user)))
					*sys-license-code*))
		     (system-collection collection-name)
		     (license-collection collection-name))
		 (system-collection collection-name))))))


(defun collection-from-doc (doc)
  (let* ((data-spec (get-data-spec (class-name (class-of doc))))	 
	(col-type (collection-type data-spec))
	(collection-name (collection-name data-spec)))
    (cond  ((equalp col-type :system)
	    (system-collection collection-name))
	   ((or (equalp col-type :license) (equalp col-type :merge))
	    (license-collection collection-name)))))

(defun collection-from-data-spec (data-spec)
  (let* (
	 
	(col-type (collection-type data-spec))
	(collection-name (collection-name data-spec)))
    
    (cond  ((equalp col-type :system)	    
	    (system-collection collection-name))
	   ((equalp col-type :license)
	    (license-collection collection-name))
	   ((equalp col-type :merge)
	    (license-collection collection-name))
	   )))


(defmethod xdb2:doc-collection ((doc license))
  (if (xdb2:top-level doc)    
    (system-collection "licenses")))

(defmethod xdb2:doc-collection ((doc data-spec))
  (if (xdb2:top-level doc)    
    (system-collection "data-specs")))

(defmethod xdb2:doc-collection ((doc xdb2:storable-object))
  (if (xdb2:top-level doc)    
    (collection-from-doc doc)))

