(in-package :cl-wfx)

(defparameter *core-store-definitions* nil)


(defun add-core-definitions (definitions)  
;;  (break "???? ~A" definitions)
  (setf *core-store-definitions* 
	(append *core-store-definitions* definitions)))

(defun add-system-definitions (system definitions)  
  (setf (data-definitions system)
	(append (data-definitions system) definitions)))

(defun init-definitions (universe destination store-name definitions)
  (when definitions
    (dolist (definition definitions)

      (let ((destinations (or (getf definition :destinations) (list destination))))
	(when (find destination destinations :test 'equalp)
	  
	  (let ((data-type-def (getf definition :data-type))
		(collection-def (getf definition :collection)))
	    (cond (collection-def			   
		   (add-collection 
		    (get-store universe store-name)
		    (make-instance 
		     'collection
		     :name (getf collection-def :name)
		     :data-type (getf collection-def :data-type)
		     :bucket-keys (getf collection-def :bucket-keys))))
		  
		  (data-type-def
		   (let ((fields))
		     (dolist (field (getf data-type-def :fields))
		       (setf fields 
			     (append fields 
				     (list (make-instance 
					    'field
					    :name (getf field :name)
					    :key-p (getf field :key-p)
					    :type-def (getf field :type-def)
					    :attributes (getf field :attributes))))))
		     
		     (add-data-type (get-store universe store-name)
				    (make-instance 
				     'data-type
				     :name (getf data-type-def :name)
				     :label (getf data-type-def :label)
				     :top-level-p (getf data-type-def :top-level-p)
				     :fields fields)))))))))))


(defmethod init-core-universe ((system system) &key &allow-other-keys)
  (unless *core-store-definitions*
    (warn (frmt "" "init-core ~A" *core-store-definitions*)))
  (add-store (universe system) 
	     (make-instance 'store
			    :name "core"))
  
  (init-definitions (universe *system*) :core "core" *core-store-definitions*))

(defmethod init-system-universe ((system system) &key &allow-other-keys)
  (unless (data-definitions system)
    (warn (frmt "init-system ~A" (data-definitions system))))
  (add-store (universe system) 
	     (make-instance 'store
			    :name (name system)))
  (init-definitions (universe system) :system (name system) (data-definitions system)))

(defmethod init-license-universe ((system system) license-code &key &allow-other-keys)
   (add-store (universe system) 
	      (make-instance 'store
			      :name license-code))
   (init-definitions (universe system) :license license-code *core-store-definitions*)
   (init-definitions (universe system) :license license-code (data-definitions system)))

(defun find-in-item-list (list test)
  (map nil
       (lambda (item)
	 (when (apply test (list item))
	       (return-from find-in-item-list item)))
       list))

(defun core-store ()
  (get-store (universe *system*) "core"))

(defun core-collection (name)
  (get-collection (core-store) name))

(defun system-store ()
  (get-store (universe *system*) "app"))

(defun system-collection (name)
  (get-collection (system-store) name))

(defun license-store (license-code)
  (get-store (universe *system*) license-code))

(defun license-collection (license-code name)
  (get-collection (license-store license-code) name))

(defun get-store-from-short-mod (mod)
  (cond ((equalp mod "cor")
	 (core-store))
	((equalp mod "sys")
	 (system-store))
	(t
	 (license-store mod))))


(defun find-collection-def (system name)
  (dolist (def (append *core-store-definitions* (data-definitions system)))

    (let ((col (dig def :collection)))     
 ;;     (break "~A ~A" col name)
      (when (and col (string-equal (getf col :name) name))
	(return-from find-collection-def col)))))

(defun find-type-def (system name)
  (dolist (def (append *core-store-definitions* (data-definitions system)))
    (let ((col (dig def :data-type)))     
      (when (and col (string-equal (getf col :name) name))
	(return-from find-type-def col)))))

(defun collection-stores (system collection)
  (let ((stores))
    (dolist (dest (digx (if (stringp collection)
			    (find-collection-def system collection)
			    collection) 
			:destinations))
      (cond ((equalp dest "cor")
	     (push (core-store) stores))
	    ((equalp dest "sys")
	     (push (system-store) stores))
	    (t
	     (dolist (lic (getx (active-user) :selected-license))
	       (push (license-store lic) stores)))))
    stores))
