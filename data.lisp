(in-package :cl-wfx)

(defparameter *core-store-definitions* nil)

(defun add-core-definitions (definitions)  
  (setf *core-store-definitions* 
	(append *core-store-definitions* definitions)))

(defun add-system-definitions (system definitions)  
  (setf (data-definitions system)
	(append (data-definitions system) definitions)))

(defclass extended-collection (item-collection)
  ((destinations :initarg :destinations
		    :accessor destinations
		    :initform nil)))

(defclass extended-data-type (data-type)
  ((entity-accessor :initarg :entity-accessor
		    :accessor entity-accessor
		    :initform nil)
   (destinations :initarg :destinations
		    :accessor destinations
		    :initform nil)))

(defun init-data-types (universe destination store-name definitions)

  (dolist (definition definitions)
      (let ((destinations (or (getf definition :destinations)
			      (list destination))))
	(when (find destination destinations :test 'equalp)
	  (let ((data-type-def (getf definition :data-type)))
	    (when data-type-def
	      (let ((fields))
		(dolist (field (getf data-type-def :fields))
		  (setf
		   fields 
		   (append fields 
			   (list (make-instance 
				  'field
				  :name (getf field :name)
				  :key-p (getf field :key-p)
				  :type-def (getf field :type-def)
				  :attributes (getf field :attributes))))))

		(add-data-type
		 (get-store universe store-name)
		 (make-instance 
		  'extended-data-type
		  :name (getf data-type-def :name)
		  :label (getf data-type-def :label)
		  :top-level-p
		  (getf data-type-def :top-level-p)
		  :fields fields
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
	      (let ((data-type (get-data-type (get-store universe store-name)
					      (getf collection-def :data-type))))
		
		(unless data-type
		  (break "?~A~%~S" (getf collection-def :data-type) collection-def)
		  (break "??~A~%~A" (getf collection-def :data-type)
			 (get-data-type (get-store universe store-name)
					(getf collection-def :data-type)))
		  (error "Collection data-type not found."))
		
		(add-collection 
		 (get-store universe store-name)
		 (make-instance 
		  'extended-collection
		  :name (getf collection-def :name)
		  :data-type data-type
		  :destinations (getf definition :destinations)))))

	  ))))))

(defun init-definitions (universe destination store-name definitions)
  ;;Data Types need to be done before collections.
  (init-data-types universe destination store-name definitions)
  (init-col-definitions universe destination store-name definitions))


(defmethod init-core-universe ((system system) &key &allow-other-keys)
  (unless *core-store-definitions*
    (warn (frmt "init-core ~A" *core-store-definitions*)))
  (add-store (universe system) 
	     (make-instance 'item-store
			    :name "core"))
 
  (dolist (def *core-store-definitions*)
    (pushnew def (data-definitions system)))
  (init-definitions (universe *system*) :core "core" *core-store-definitions*))

(defmethod init-system-universe ((system system) &key &allow-other-keys)
  (unless (data-definitions system)
    (warn (frmt "init-system ~A" (data-definitions system))))
  (add-store (universe system) 
	     (make-instance 'item-store
			    :name (name system)))
   (init-definitions (universe system)
		    :system (name system) (data-definitions system)))

(defmethod init-license-universe ((system system) license-code
				  &key &allow-other-keys)
  (add-store (universe system) 
	     (make-instance 'item-store
			    :name license-code))
  (init-definitions (universe system) :license license-code
		     *core-store-definitions*)
  (init-definitions (universe system) :license license-code
		    (data-definitions system)))

(defun core-store ()
  (cl-naive-store::get-store* (universe *system*) "core"))

(defun core-collection (name)
  (cl-naive-store::get-collection* (core-store) name))

(defun system-store ()
  (cl-naive-store::get-store* (universe *system*) (name *system*)))

(defun system-collection (name)
  (cl-naive-store::get-collection* (system-store) name))

(defun license-store (license-code)
  (cl-naive-store::get-store* (universe *system*) license-code))

(defun license-collection (license-code name)
  (cl-naive-store::get-collection* (license-store license-code) name))

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
      (let ((col (dig def :collection)))  
	(when (and col (string-equal (dig col :name) name))
	  (return-from find-collection-def def))))))

(defun find-type-def (system name)
  (let ((definitions (data-definitions system)))
    
    (dolist (def definitions)
      (let ((col (dig def :data-type)))  

	(when (and col (string-equal (dig col :name) name))
	  (return-from find-type-def def))))))

(defun collection-stores (system collection)
  "To enable \"auto\" customization, stores adhere to a hieghrarchy
where license store items overide, system which overrides
core items. So stores are fetched in core,system,license order
so that when items are merged when fetched, later
items override earlier ones. See append-items."
  (let ((core-store)
	(system-store)
	(license-store)
	(destinations
	 (if (stringp collection)
	     (digx  (find-collection-def system collection) :destinations)
	     (destinations collection))))

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
  "Selecting the last store from stores list ensure hierarchy of items
that override others to the correct level."
  (first (last (collection-stores *system*
				  collection-name))))

(defun wfx-get-collection (collection-name)
  "Selects the collection from the correct store in the hierarchy of stores."
  (get-collection (collection-store collection-name)
		  collection-name))

(defun append-items (items more-items)
  (let ((merged-items items))
    (dolist (item (remove-if #'not items))
      (dolist (more-item (remove-if #'not more-items))
	(when (equalp (item-hash item)
		      (item-hash more-item))
	  
	  (setf merged-items (remove item merged-items)))))
    (append merged-items more-items)))

(defun wfx-fetch-items (collection &key test (result-type 'list))
  
  (let ((items)
	(collection-name (if (stringp collection)
			     collection
			     (name collection))))
  
    (dolist (store (collection-stores *system* collection-name))      
      (when (get-collection store collection-name)
	
	(setf items (append-items
		     items
		     (fetch-items (get-collection store collection-name)
				  :test test
				  :result-type result-type)))))
    items))

(defun wfx-fetch-item (collection &key test (result-type 'list))
  (first (last (wfx-fetch-items
		collection
		:test test :result-type result-type))))



;;TODO: do something to force correct order of stores searched instead of relying on order of stores!!!

(defun wfx-fetch-context-items (collection &key test (result-type 'list))
  (let* ((items)
	 (item-count 0)
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name)))
	(stores (collection-stores *system* collection-name)))


    (dolist (store stores)
      (let ((collection (get-collection
				  store 
				  collection-name)))

	(when collection
	  (setf items
		(append-items items
			(fetch-items 
			 collection
			 :test (lambda (item)
				 (when item				  
				   (when (match-context-entities item)
				     (incf item-count)
				     (if test
					 (funcall test item)
					 item))))
			 :result-type result-type))))))
    
    (values items item-count)))

(defun wfx-fetch-context-item (collection &key test)
  (let* ((items)
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name)))
	(stores (collection-stores *system* collection-name)))

    (dolist (store stores)
      (let ((collection (get-collection
				  store 
				  collection-name)))
	(when collection  
	  (setf items
		(append-items
		 items
		 (list (fetch-item 
			collection
			:test (lambda (item)
				(when item
				  (when (match-context-entities item)
				    (if test
					(funcall test item)
					item)))))))))))
  
    (first (remove-if #'not items))))

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


(defun sanitize-data-file (store collection-name path)
    (let ((items (fetch-items 
		  (get-collection store  collection-name)
		  :test (lambda (item)
			 
			  item))))
      (when (probe-file
	       (frmt "~A/~A/~A.old" path collection-name collection-name))
	  (fad:copy-file (frmt "~A/~A/~A.old" path collection-name collection-name)
			 (frmt "~A/~A/~A.old.old" path collection-name collection-name)
			 :overwrite t))

      (fad:copy-file (frmt "~A/~A/~A.log" path collection-name collection-name)
		       (frmt "~A/~A/~A.old" path collection-name collection-name)
		       :overwrite t)
      (when items
	(dolist (item items)
	
	  (cl-naive-store::persist item
				   :file (frmt "~A~A/~A.new" path collection-name
					       collection-name)
				   :new-file-p t))
	(fad:copy-file (frmt "~A~A/~A.new" path collection-name collection-name)
		       (frmt "~A~A/~A.log" path collection-name collection-name)
		       :overwrite t))
      ))
