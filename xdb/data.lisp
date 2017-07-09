(in-package :cl-wfx)

(defclass xdb-data (data xdb2:dbs)
  ((data-folder :initarg :data-folder
		   :accessor data-folder
		   :initform "~~/xdb2/")))

(defmethod tear-down-data ((data xdb-data) &key &allow-other-keys))


;;TODO: storable object version implementation code should be split into its own file
(defmethod xdb2:persist ((doc xdb2:storable-versioned-object) 
			 &key collection (set-time t))
  (unless (xdb2:top-level doc)
    (error "PERSIST can only be called on top-level objects."))
  (let ((time (get-universal-time)))
    (xdb2::with-collection-lock collection
      (when (xdb2:read-only-p collection)
        (warn "Collection ~s is read-only" collection)
        (return-from xdb2:persist))
      (setf (xdb2:collection doc) collection)
      (when set-time
        (setf (xdb2:stamp-date doc) time))
      (setf (xdb2:effective-date doc) time
            (xdb2:modified collection) time)
      (xdb2:before-persist collection doc)
      (xdb2::with-db-error-handling (collection)
        (xdb2::save-doc collection doc))))
  doc)

;;;

(defmethod xdb2:persist ((doc xdb2:storable-object) &key collection)
  (unless (xdb2:top-level doc)
    (error "PERSIST can only be called on top-level objects."))
  (xdb2::with-collection-lock collection 
      (when (xdb2:read-only-p collection)
        (warn "Collection ~s is read-only" collection)
        (return-from xdb2:persist))
      (setf (xdb2:collection doc) collection)
      (xdb2:before-persist collection doc)
      (xdb2::with-db-error-handling (collection)
        (xdb2::save-doc collection doc))
      (setf (xdb2:modified collection) (get-universal-time)))
  doc)

(defun license-collection (license-code collection-name)  
  (let ((db (xdb2:get-db (data *system*)
			 (list (frmt "~A" (id-string (system-name *system*)))
			       license-code))))
    (unless db
      (setf db (xdb2:add-db (data *system*) 
			    (list (frmt "~A" (id-string (system-name *system*)))
				  license-code)))
      
      ;;TODO: Figure out loading of db files
      #|
      (dolist (data-spec (fetch-all "data-specs" :result-type 'list))
	(when (or
	       (equalp (collection-type data-spec) :merge)
	       (equalp (collection-type data-spec) :license))
	  (license-collection (collection-name data-spec))))
      (xdb2:load-collections db)
      (xdb2:clear-db-cache db)
      |#
      )
    
    (let ((col
	   (xdb2:get-collection db collection-name)))
      (unless col
	(setf col (xdb2:add-collection db collection-name
				       :force-load t)))
      col)))

(defun persist (item license-code collection-name )
  (let ((license-code license-code))
    (unless license-code
      (setf license-code (license-code item)))
    
    (let ((collection (license-collection license-code collection-name)))
      (when (and (slot-exists-p item 'license-code)
		 (not (license-code item)))
	(setf (license-code item) license-code))
      (xdb2:persist item :collection collection)))
  )

(defmethod persist-data ((item xdb2:storable-object) 
			 &key license-code collection-name 
			   &allow-other-keys)
  
  (persist item license-code collection-name))

(defmethod persist-data ((item xdb2:storable-versioned-object) 
			 &key license-code collection-name 
			   &allow-other-keys)
  (persist item license-code collection-name))


(defmethod remove-doc ((object xdb2:storable-object) collection &key log)
  :documentation "Marks the document as deleted. The storable object implementation can
 be set to load deleted docs but they do not occur in the actual collection."
  (xdb2::with-collection-lock collection
      (setf (xdb2:docs collection)
            (xdb2::delete object
			 (alexandria:copy-array (xdb2:docs collection))))
      (xdb2::with-db-error-handling (collection)
        (xdb2::delete-doc collection object log))
      (setf (xdb2:modified collection) (get-universal-time))))


(defmethod delete-data ((doc xdb2:storable-object) &key license-code collection-name &allow-other-keys)
  (let ((collection (license-collection license-code collection-name)))
    (remove-doc doc collection)))

(defmethod get-key-value ((item xdb2:storable-object) data-spec &key &allow-other-keys)
  (let ((keys))
    (dolist (field (data-spec-fields data-spec))
      (when (getf field :key)
	(pushnew (slot-value item (getf field :name)) keys)))
    keys))


(defmethod data-items (license-code collection-name)
  (let ((collection (license-collection license-code collection-name)))
    (when collection
	(xdb2:docs collection))))


(defmethod license-items (collection-name result-type)
  
  (let ((license-codes 
	 (or (and (active-user) (license-codes (active-user))) 
	     (and (current-user) (list (first (license-codes (current-user)))))
	     (list *sys-license-code*)))
	(items))
    (dolist (license-code license-codes)
      (let ((collection (license-collection license-code collection-name)))
	(when (and collection (xdb2:docs collection))
	  (if items	      
	      (concatenate result-type items (xdb2:docs collection))
	      (setf items (coerce (xdb2:docs collection) result-type))))))
    items))




(defun merge-data-specs (sys-items lic-items)
  (concatenate 
   'list
   (if (and lic-items (> (length lic-items) 0))
       (remove-if (lambda (doc)
		    (find doc lic-items 
			  :test (lambda (doc tdoc)
				  (equalp 
				   (name doc)
				   (name tdoc)))))
		  sys-items)
       sys-items) 
   lic-items))

;;Not using other data fetch mechanisms because they rely on 
;;get-data-spec so goes into infinite loop
(defun get-data-spec* (name-or-col)    
  (map
   nil
   (lambda (spec)
     (when (or (string-equal name-or-col (name spec))
	       (string-equal name-or-col (collection-name spec)))
       (return-from get-data-spec* spec)))
   (merge-data-specs
    (data-items *sys-license-code* "data-specs")
    (license-items "data-specs" 'vector))))




(defmethod fetch-all* ((data xdb-data) collection-name 
		       &key (result-type 'vector) &allow-other-keys)
  (let* ((data-spec (get-data-spec* collection-name))	
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
	     
	   
	     
	     (let* ((system-items 
		     (data-items *sys-license-code*
					 (collection-name data-spec)))
		    (license-items 
		     (license-items (collection-name data-spec) result-type))
		    
		    
		    (docs (merge-items 
			   system-items
			   license-items
			   data-spec
			   result-type)))
	       
	       
	       (if entity-p
		   (setf items (remove-if-not #'match-context-entities docs))
		   (setf items docs))
	       
	       
	       
	       ))
	    
	    ((equalp collection-type :system)
	     
	     (let ((system-items 
		     (data-items *sys-license-code*
					 (collection-name data-spec))))
	      
	       (if system-items
		   (setf items
			 (coerce (if entity-p
				     (remove-if-not #'match-context-entities 
						    system-items)
				     system-items)
				 result-type)))))
	    
	    ((equalp collection-type :license)
	     
	     (let ((license-items (license-items (collection-name data-spec) result-type)))
	      
	       (when license-items
		 
		   (setf items
			 (if entity-p
			     (remove-if-not #'match-context-entities 
					    license-items)
			     license-items)))))
	    (t
	     (break "Collection type not set. --- data-spec ~%~A~A~A"
		    (name data-spec) (collection-name data-spec) collection-type)
	     (error "Collection type not set.")))

      ;;TODO: Should sort be applied here?????????
      
      items)))

(defmethod fetch-items* ((data xdb-data) collection 
			 &key test result-type &allow-other-keys)
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
	   docs)))))






