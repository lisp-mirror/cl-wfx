(in-package :cl-wfx)

(defclass wfx-universe (universe)
  ((store-class :initarg :store-class
		 :accessor store-class
		 :initform 'document-store
		 :allocation :class
		 :documentation "Overriding store class here to influence document-type-class and collection-class."))
  (:default-initargs
   :store-class 'document-store))

(defparameter *core-store-definitions* nil)

(defun add-core-definitions (definitions)  
  (setf *core-store-definitions* 
	(append *core-store-definitions* definitions)))

(defun add-system-definitions (system definitions)  
  (setf (data-definitions system)
	(append (data-definitions system) definitions)))

(defclass extended-collection (document-collection)
  ((destinations :initarg :destinations
		    :accessor destinations
		    :initform nil)))

(defclass extended-document-type (document-type)
  ((entity-accessor :initarg :entity-accessor
		    :accessor entity-accessor
		    :initform nil)
   (destinations :initarg :destinations
		    :accessor destinations
		    :initform nil)))

(defun init-document-types (universe destination store-name definitions)

  (dolist (definition definitions)
      (let ((destinations (or (getf definition :destinations)
			      (list destination))))
	(when (find destination destinations :test 'equalp)
	  (let ((document-type-def (getf definition :type-def)))
	    (when document-type-def
	      (let ((elements))
		(dolist (element (getf document-type-def :elements))
		  (setf
		   elements 
		   (append elements 
			   (list (make-instance 
				  'element
				  :name (getf element :name)
				  :key-p (getf element :key-p)
				  :type-def (getf element :db-type)
				  :attributes (getf element :attributes))))))

		(add-document-type
		 (get-store universe store-name)
		 (make-instance 
		  'extended-document-type
		  :name (getf document-type-def :name)
		  :label (getf document-type-def :label)
		  :elements elements
		  :destinations
		  (getf definition :destinations)
		  :entity-accessor
		  (getf definition :entity-accessor))))))))))

(defun init-col-definitions (universe destination store-name definitions)
  (when definitions
    (dolist (definition definitions)
      (let ((destinations (or (getf definition :destinations)
			      (list destination))))
	(when (find destination destinations :test 'equalp)
	  (let ((collection-def (getf definition :collection)))

	    
	    
	    (when collection-def
	      (let ((document-type (get-document-type (get-store universe store-name)
					      (getf collection-def :type-def))))
		
		(unless document-type
		  #|
		  (break "?~A~%~S" (getf collection-def :type-def) collection-def)
		  (break "??~A~%~A" (getf collection-def :type-def)
			 (get-document-type (get-store universe store-name)
					(getf collection-def :type-def)))
		  |#
		  (error (frmt "Collection document-type (~A) not found." (getf collection-def :type-def))))
		
		(add-collection 
		 (get-store universe store-name)
		 (make-instance 
		  'extended-collection
		  :name (getf collection-def :name)
		  :document-type document-type
		  :destinations (getf definition :destinations)))))

	  ))))))

(defun init-definitions (universe destination store-name definitions)
  ;;Data Types need to be done before collections.
  (init-document-types universe destination store-name definitions)
  (init-col-definitions universe destination store-name definitions))


(defmethod init-core-universe ((system system) &key &allow-other-keys)
  (unless *core-store-definitions*
    (warn (frmt "init-core ~A" *core-store-definitions*)))
  (add-store (universe system) 
	     (make-instance 'document-store
			    :name "core"))
 
  (dolist (def *core-store-definitions*)
    (pushnew def (data-definitions system)))
  (init-definitions (universe *system*) :core "core" *core-store-definitions*))

(defmethod init-system-universe ((system system) &key &allow-other-keys)
  (unless (data-definitions system)
    (warn (frmt "init-system ~A" (data-definitions system))))
  (add-store (universe system) 
	     (make-instance 'document-store
			    :name (name system)))
   (init-definitions (universe system)
		    :system (name system) (data-definitions system)))

(defmethod init-license-universe ((system system) license-code
				  &key &allow-other-keys)
  (add-store (universe system) 
	     (make-instance 'document-store
			    :name license-code))
  (init-definitions (universe system) :license license-code
		     *core-store-definitions*)
  (init-definitions (universe system) :license license-code
		    (data-definitions system)))

(defun core-store ()
  (naive-impl::get-store* (universe *system*) "core"))

(defun core-collection (name)
  (naive-impl::get-collection* (core-store) name))

(defun system-store ()
  (naive-impl::get-store* (universe *system*) (name *system*)))

(defun system-collection (name)
  (naive-impl::get-collection* (system-store) name))

(defun license-store (license-code)
  (naive-impl::get-store* (universe *system*) license-code))

(defun license-collection (license-code name)
  (naive-impl::get-collection* (license-store license-code) name))

(defun get-store-from-short-mod (mod)
  (cond ((equalp mod "cor")
	 (core-store))
	((equalp mod "sys")
	 (system-store))
	(t
	 (license-store mod))))

(defun find-collection-def (system name)
  (let ((definitions (data-definitions system)))
    (dolist (def definitions)
      (let ((col (digx def :collection)))  
	(when (and col (string-equal (digx col :name) name))
	  (return-from find-collection-def def))))))

(defun find-type-def (system name)
  (let ((definitions (data-definitions system)))
    
    (dolist (def definitions)
      (break "poes ~A" def)
      (let ((col (digx def :type-def)))  

	(when (and col (string-equal (digx col :name) name))
	  (return-from find-type-def def))))))

(defun find-type-defenition (system collection def-name)
  (let ((stores (collection-stores system collection))
	(store))

    (dolist (storex stores)
      (setf store storex))
    
    (cl-naive-document-types:get-document-type store def-name)))

(defun collection-stores (system collection)
  "To enable \"auto\" customization, stores adhere to a hieghrarchy
where license store documents overide, system which overrides
core documents. So stores are fetched in core,system,license order
so that when documents are merged when fetched, later
documents override earlier ones. See append-documents."
  (let ((core-store)
	(system-store)
	(license-store)
	(destinations
	 (if (not collection)
	     (list :core :system :license)
	     (if (stringp collection)
		 (digx  (find-collection-def system collection) :destinations)
		 (destinations collection)))))

    (dolist (dest (list :core :system :license))
      (when (find dest destinations :test #'equalp)
	
	(cond ((equalp dest :core)
	       (setf core-store (core-store)))
	      ((equalp dest :system)
	       (setf system-store (system-store)))
	      (t
	       (if (active-user)
		   ;;TODO: cannot select more than one license at this time
		   ;;change selected-lcisenses to selected-license or
		   ;;implement multiple license screens.
		   (dolist (lic (getx (active-user) :selected-licenses))
		     (setf license-store (license-store lic))))))))

      (remove-if #'not (list core-store system-store license-store))))

(defun collection-store (collection-name)
  "Selecting the last store from stores list ensure hierarchy of documents
that override others to the correct level."
  (first (last (collection-stores *system*
				  collection-name))))

(defun wfx-get-collection (collection-name)
  "Selects the collection from the correct store in the hierarchy of stores."
  (get-collection (collection-store collection-name)
		  collection-name))

(defun append-documents (documents more-documents)
  (let ((merged-documents documents))
    (dolist (document (remove-if #'not documents))
      (dolist (more-document (remove-if #'not more-documents))
	(when (equalp (document-hash document)
		      (document-hash more-document))
	  
	  (setf merged-documents (remove document merged-documents)))))
    (append merged-documents more-documents)))

(defun wfx-query-data (collection &key query)
  (let ((documents)
	(collection-name (if (stringp collection)
			     collection
			     (name collection))))
  
    (dolist (store (collection-stores *system* collection-name))      
      (when (get-collection store collection-name)
	(setf documents (append-documents
		     documents
		     (query-data (get-collection store collection-name)
				 :query query)))))
    documents))

(defun wfx-query-document (collection &key query)
  (first (last (wfx-query-data
		collection
		:query query))))

;;TODO: do something to force correct order of stores searched instead of relying on order of stores!!!

(defun wfx-query-context-data (collection &key query)
  (let* ((documents)
	 (document-count 0)
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name)))
	(stores (collection-stores *system* collection-name)))


    (dolist (store stores)
      (let ((collection (get-collection
				  store 
				  collection-name)))
	(when collection
	  (setf documents
		(append-documents documents
			(query-data
			 collection
			 :query (lambda (document)
				 (when document				  
				   (when (match-context-entities document)
				     (incf document-count)
				     (if query
					 (funcall query document)
					 document))))))))))
    
    (values documents document-count)))

(defun wfx-query-context-document (collection &key query)
  (let* ((documents)
	 (other-documents)
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name)))
	(stores (collection-stores *system* collection-name)))

    
    (dolist (store stores)
      (let ((collection (get-collection
				  store 
				  collection-name)))
	(when collection
	  (multiple-value-bind (object others)
	      (query-document
			collection
			:query (lambda (document)
				(when document
				  (when (match-context-entities document)
				    (if query
					(funcall query document)
					document)))))            
	    (setf documents
		  (append-documents
		   documents
		   (list object)))
	    (setf other-documents
		  (append-documents
		   other-documents
		   others))))))
    
    (values (first documents) (rest documents) other-documents)))

(defun in-place-subst (file refs)
  (let ((backup-file (format nil "~A.shash.bak" file)))
    (fad:copy-file file backup-file :overwrite t)
    (with-open-file (in-stream backup-file)
      (with-open-file (out-stream file :direction :output :if-exists :supersede)
        (loop for line = (read-line in-stream nil)
           while line do
	     (dolist (ref refs)
	       (setf line (cl-ppcre:regex-replace-all (fourth ref)  line (third ref))))
             (write-line line out-stream))))))


(defun read-shash-error-refs (file)
  (let ((refs))
    (with-open-file (in file :if-does-not-exist :create)
      (with-standard-io-syntax              
	(when in

	  (loop for line = (read in nil)
	   while line
	   do (push line refs))
	  (close in))))
    refs))

(defun replace-refs (dir refs)
  (let ((files (directory (format nil "~A**/*.log" dir))))
    (dolist (file files)
      (in-place-subst file refs)
      )
    ))



