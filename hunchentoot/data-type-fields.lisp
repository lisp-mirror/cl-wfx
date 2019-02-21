(in-package :cl-wfx)

(defun print-item-val-s* (field item &key default-value)
  (let ((*print-case* :downcase)
	(val (if item (getfx item field)))
	(accessors (if (listp (getf field :db-type))
		       (dig field :db-type :accessor))))

    (when (and (listp val)
	       (listp (first val)))
     ;; (break "~A" (first val))
	(setf val (first val))
	)
    
    (if (if accessors
	    (accessor-value val accessors)
	    val)
	(frmt "~S" val)
	(if default-value
	    default-value
	    ""))))

(defun print-item-val-a* (field item &key default-value)
  (let ((*print-case* :downcase)
	(val (if item (getfx item field)))
	(accessors (if (listp (getf field :db-type))
		       (dig field :db-type :accessor))))

    (if val
	(frmt "~A" (if accessors
	    (accessor-value val accessors)
	    val))
	(if default-value
	    default-value
	    ""))))

(defmethod print-item-val ((type (eql :symbol)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :keyword)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :script)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :string)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :link)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :text)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defun file-server-path (collection field item)
  (string-downcase
   (frmt "~A/files/~A/~A/~A"
	 (if (and (item-collection item)
		  (location (item-collection item)))
	     (location (item-collection item))
	     (string-downcase
	      (frmt "~A~A"
		    (location
		     (store
		      collection))
		    (name collection))))
	 (item-data-type item)
	 (getf field :name)
	 ;; (item-hash item)
	 (if (not (empty-p (getx item (getf field :name))))	     		
	     (sanitize-file-name (getx item (getf field :name)))))))

(defun file-url (collection field item)
  (string-downcase
     (frmt "~A~A/~A/files/~A/~A/~A"
	   (site-url *system*)
	   (first (getx (active-user) :selected-licenses))
	   (name collection)
	   (item-data-type item)
	   (getf field :name)
	   ;; (item-hash item)
	   (if (not (empty-p (getx item (getf field :name))))
	       (sanitize-file-name (getx item (getf field :name)))))))

(defmethod print-item-val ((type (eql :image)) field item
			   &key default-value &allow-other-keys)
  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (server-path (file-server-path collection field item)))
    (with-html-string
      
      (if (getx item (getf field :name))
	  (let ((image-url (file-url collection field item)))
	    (push (hunchentoot::create-static-file-dispatcher-and-handler
		   image-url
		   server-path)
		  hunchentoot::*dispatch-table*)
	    (cl-who:htm
	     (:a :data-toggle "modal"
		 :data-target (string-downcase
			       (frmt "#~A-modal" (getf field :name)))
		 (:img
		  :style "width:128px;height:128px;"
		  :src image-url))
	     (:div :class "modal fade"
		   :tab-index -1
		   :id (string-downcase (frmt "~A-modal" (getf field :name)))
		   (:div :class "modal-dialog modal-dialog-centered "
			 (:img
			  :style "max-width:120%;"
			  :src image-url)))))
	  (cl-who:htm (:img :src "/umage/cor/web/images/logo-small.png"))))))

(defmethod print-item-val ((type (eql :email)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :file)) field item
			   &key default-value &allow-other-keys)
  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (server-path (file-server-path collection field item)))
    (with-html-string
      
      (if (getx item (getf field :name))
	  (let ((file-url (file-url collection field item)))
	  
	    (push (hunchentoot::create-static-file-dispatcher-and-handler
		   file-url
		   server-path)
		  hunchentoot::*dispatch-table*)
	    (cl-who:htm
	     (:a :href file-url
		(cl-who:str file-url))
	    ))
	  ))))

(defmethod print-item-val ((type (eql :number)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :integer)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :date)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :time)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-a* field item :default-value default-value))

(defmethod print-item-val ((type (eql :boolean)) field item
			   &key default-value &allow-other-keys)
  (if (getfx item field)
      "checked"
      (if default-value
	  default-value
	  "")))

(defmethod print-item-val ((type (eql :key-value)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :item)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))


(defmethod print-item-val ((type (eql :value-list)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :key-value-list)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :value-string-list)) 
			   field item &key default-value
					&allow-other-keys)
  (let* ((delimiter (dig field :db-type :delimiter))
	 (val (getsfx (dig field :db-type :type) field item))
	 (final-val))
  
    (when (and val (listp val))
      
      (dolist (x val)
	(setf final-val 
	      (if final-val
		  (concatenate 'string final-val (if (stringp delimiter)
						     delimiter
						     (eval delimiter)) 
			       (if (equalp (dig field :db-type :type)
					   :keyword)
				   (string-downcase (frmt "~S" x))
				   x))
		  (if (equalp (dig field :db-type :type)
			      :keyword)
		      (string-downcase (frmt "~S" x))
		      x)))))

    final-val))

(defmethod print-item-val ((type (eql :list-items)) field item
			   &key default-value &allow-other-keys)
  (print-item-val-s* field item :default-value default-value))

(defmethod print-item-val ((type (eql :collection-items))
			   field item &key default-value
					&allow-other-keys)
 
  (let ((item-val (getfx item field))
	(accessor (dig field :db-type :accessor)))
    
    (when (listp item-val)
      (when item-val
	
	(frmt "~A"
	      (accessor-value (first item-val) (dig field :db-type :accessor))
	      ))))
  ;;(print-item-val-s* field item)
  )

(defmethod print-item-val ((type (eql :contained-item)) field item
			   &key default-value
			     &allow-other-keys)
  (let ((item-val (getfx item field))
	(accessor (dig field :db-type :accessor)))
    
    (when (or (listp item-val) (equalp (type-of item-val) 'item))
      (when item-val
	
	(frmt "~A"
	      (accessor-value item-val (dig field :db-type :accessor))
	      )))))

(defmethod print-item-val ((type (eql :collection-contained-item)) 
			   field item &key default-value
					&allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection)) field item
			   &key &allow-other-keys)
  
  (let ((item-val (getfx item field))
	(accessor (dig field :db-type :accessor)))

    
    (when (or (listp item-val) (equalp (type-of item-val) 'item))
       (when item-val
	 (frmt "~A"
	       (accessor-value item-val accessor)
	     )))))

(defmethod print-item-val ((type (eql :hierarchical)) field item 
			   &key default-value &allow-other-keys)
  ;;TODO: Sort this shit out need to loop tree
  (let ((item-val (getfx item field))
	(final))    
    (when item-val
      (dolist (x item-val)
	(if final
	    (setf final (frmt "~A ~A"
			      final
			      (accessor-value x (dig field :db-type :accessor))
			     ))
	    (frmt "~A" (accessor-value x (dig field :db-type :accessor))))))))


(defgeneric render-input-val (type field item &key &allow-other-keys))

(defun item-default-val* (field item)
  (if (getf field :default-value)
      (apply
       (eval
	(getf field :default-value))
       (list item))))

(defun render-input-val* (type field item)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"		  
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"
		  :required (if (getf field :key-p)
				"required")
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item)))))))

(defmethod render-input-val ((type (eql :symbol)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :keyword)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :string)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :link)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :text)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 50 :rows 3
	   :disabled "disabled"
	   (cl-who:str (print-item-val 
			type
			field 
			item
			:default-value
			       (item-default-val* field item)))))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 50 :rows 3
	   :required (if (getf field :key-p)
			 "required")
	   (cl-who:str
	    (print-item-val 
	     type
	     field 
	     item
	     :default-value
	     (item-default-val* field item))))))))

(defun render-image (field item)
  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (image-path (file-url collection field item)))
    (with-html-string
      (:input :type "hidden"
	      :id (string-downcase
		   (frmt "args-~A-~A"
			 (getf field :name)
			 (item-hash item)))
	      :value (string-downcase
		      (frmt "{\"license\":\"~A\", \"collection\":\"~A\", \"datatype\":\"~A\", \"field\":\"~A\"}"
			    (first (getx (active-user) :selected-licenses))
			    (name collection)
			    (item-data-type item)
			    (getf field :name))))
      
      (:input :type "hidden"
	      :id (string-downcase
		   (frmt "init-~A-~A" (getf field :name)
			 (item-hash item)))
	      :value image-path
	      :name (string-downcase
		     (frmt "~A" (getf field :name))))
      
      (:input :type "file"
	      :class "file-upload"
	      :multiple t
	      :id (string-downcase
		   (frmt "~A-~A" (getf field :name)
			 (item-hash item)))
	      :required (if (getf field :key-p)
			    "required")))))

(defun ajax-render-file-upload (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  
  (let* ((data-type (parameter "data-type"))
	(item (getcx data-type :edit-item))
	(fields (getcx data-type :fields)))

    (dolist (field fields)
      (when (equalp (getf field :name) (parameter "field-name"))
	(return-from ajax-render-file-upload
	  (render-image field item))))
    )
  )

(defmethod render-input-val ((type (eql :image)) field item
			     &key  &allow-other-keys)

  (with-html-string
    (:div :class "col"
	  (:div :class "row"	  
		:id (string-downcase
		     (frmt "file-upload-row-~A" (item-data-type item)))
		:name (string-downcase
		       (frmt "file-upload-row-~A" (item-data-type item)))
		(cl-who:str (render-image field item))
		))))

(defmethod render-input-val ((type (eql :file)) field item
			     &key &allow-other-keys)

  (with-html-string
    (:div :class "col"
	  (:div :class "row"	  
		:id (string-downcase
		     (frmt "file-upload-row-~A" (item-data-type item)))
		:name (string-downcase
		       (frmt "file-upload-row-~A" (item-data-type item)))
		(cl-who:str (render-image field item))
		)))
  #|
(with-html-string
    (:div :class "row"
	  (:div :class "col"
		(:input
		 :class "file-upload"
		 :type "file"
			:onchange (frmt "$(\"#file-~A\").val($(this).val());"
					(getf field :name)))
		
		(:input :type "text" :class "form-control "
			:name (getf field :name)
			:id (frmt "file-~A" (getf field :name))
			:value (getx item (getf field :name))
			:readonly t))))
  |#
  )

(defmethod render-input-val ((type (eql :email)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :number)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
		      
		      (:input :class "form-control"
			      :id name
			      :name name 
			      :type "number"
			      :step "any"		  
			      :value
			      (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))
			      :disabled "disabled"))
	
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name		  
		  :type "number"
		  :step "any"
		  :required (if (getf field :key-p)
				"required")
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item)))))))

(defmethod render-input-val ((type (eql :integer)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "number"
		  :step "any"		  
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name		  
		  :type "number"
		  :step "1"
		  :required (if (getf field :key-p)
				"required")
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))))))))

(defmethod render-input-val ((type (eql :date)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control "
		  :id name
		  :name name 
		  :type "date"
		 
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control date"
		  :id name
		  :name name 
		  :type "date"
		  :required (if (getf field :key-p)
				"required")
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item)))))))

(defmethod render-input-val ((type (eql :time)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "time"	     
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "time"
		  :required (if (getf field :key-p)
				"required")
		  :value
		  (print-item-val 
			       type
			       field 
			       item
			       :default-value
			       (item-default-val* field item)))))))

(defmethod render-input-val ((type (eql :boolean)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name))
	(print-val (getsfx type field item)))
    
    (with-html-string
      (:div :class "form-check" 
	    (if (not (digx field :attributes :editable))
		(cl-who:htm (:div :class "form-check-label"
				  (:input
				   :class "form-check-input"
				   :type "checkbox"
				   :id name
				   :name name
				   :value (getsfx
					   type
					   field 
					   item
					   :default-value
					   (item-default-val* field item))
				   :checked print-val
				   :aria-label "..."
				   :disabled "disabled")))
		(cl-who:htm (:div :class "form-check-label"
				  (:input
				   :class "form-check-input"
				   :type "checkbox"
				   :id name
				   :name name
				   :required (if (getf field :key-p)
						 "required")
				   :value (getsfx
					   type
					   field 
					   item
					   :default-value
					   (item-default-val* field item))
				   :checked print-val
				   :aria-label "..."))))))))

(defmethod render-input-val ((type (eql :script)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:textarea 
	   :class "form-control wfx-script"
	   :id name
	   :name name :cols 50 :rows 10
	   :disabled "disabled"
	   (cl-who:str (print-item-val 
			type
			field 
			item))))
	(with-html-string
	  (:textarea 
	   :class "form-control wfx-script"
	   :id name
	   :name name :cols 50 :rows 10
	   :required (if (getf field :key-p)
			 "required")
	   (cl-who:str
	    (print-item-val 
	     type
	     field 
	     item)))))))

(defmethod render-input-val ((type (eql :value-string-list)) 
			     field item &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (delimiter (dig field :db-type :delimiter)))

    (if (stringp delimiter)
	delimiter
	(setf delimiter (eval delimiter)))

    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 20 :rows 3
	   :disabled "disabled"
	   (cl-who:str (print-item-val 
			:value-string-list
			field 
			item)))
	  (:span 
	   (cl-who:str (frmt "Delimit by ~A" (cond ((string-equal delimiter " ")
						    "#\Space")
						   ((string-equal delimiter (frmt "~%"))
						    "#\Return")
						   (t
						    (frmt "~S" delimiter)))))))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 20 :rows 3
	   :required (if (getf field :key-p)
			 "required")
	   (cl-who:str (print-item-val 
			:value-string-list
			field 
			item)))
	  (:span (cl-who:str
		  (frmt "Delimit by ~A" (cond ((string-equal delimiter " ")
						    "#\Space")
						   ((string-equal delimiter (frmt "~%"))
						    "#\Return")
						   (t
						    (frmt "~S" delimiter))))))))))

