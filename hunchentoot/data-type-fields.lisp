(in-package :cl-wfx)

(defgeneric render-input-val (type field item &key &allow-other-keys))

(defun render-input-val* (type field item)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"	     
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"	     
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item)))))))

(defmethod render-input-val ((type (eql :symbol)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :keyword)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :string)) field item
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
	   :name name :cols 50 :rows 10
	   :disabled "disabled"
	   (cl-who:str (print-item-val 
			type
			field 
			item))))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 50 :rows 10
	   (cl-who:str
	    (print-item-val 
	     type
	     field 
	     item)))))))

(defun render-image (field item)
  (let* ((collection (get-perist-collection
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
			 (item-hash item)))))))

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
    (:div :class "row"	  
	  :id (string-downcase
	       (frmt "file-upload-row-~A" (item-data-type item)))
	  :name (string-downcase
		 (frmt "file-upload-row-~A" (item-data-type item)))
	  (cl-who:str (render-image field item))
	  )))

(defmethod render-input-val ((type (eql :file)) field item
			     &key &allow-other-keys)
  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(:input :type "file"
			:onchange (frmt "$(\"#file-~A\").val($(this).val());"
					(getf field :name)))
		
		(:input :type "text" :class "form-control "
			:name (frmt "file-~A" (getf field :name))
			:id (frmt "file-~A" (getf field :name))
			:value (getx item (getf field :name))
			:readonly t)))))

(defmethod render-input-val ((type (eql :email)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :number)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :integer)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :date)) field item
			     &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (digx field :attributes :editable))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "date"	     
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "date"	     
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item)))))))

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
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item))
		  :disabled "disabled"))
	(with-html-string
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "time"	     
		  :value
		  (cl-who:str (print-item-val 
			       type
			       field 
			       item)))))))

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
					   item)
				   :checked print-val
				   :aria-label "..."
				   :disabled "disabled")))
		(cl-who:htm (:div :class "form-check-label"
				  (:input
				   :class "form-check-input"
				   :type "checkbox"
				   :id name
				   :name name
				   :value (getsfx
					   type
					   field 
					   item)
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
	   (cl-who:str
	    (print-item-val 
	     type
	     field 
	     item)))))))

(defmethod render-input-val ((type (eql :value-string-list)) 
			     field item &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (delimiter (dig field :db-type :delimiter)))

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
	   (cl-who:str (frmt "Delimit by ~A" (if (string-equal delimiter " ")
						 "#\Space"
						 delimiter)))))
	(with-html-string
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 20 :rows 3
	   (cl-who:str (print-item-val 
			:value-string-list
			field 
			item)))
	  (:span (cl-who:str
		  (frmt "Delimit by ~A" (if (string-equal delimiter " ")
					    "#\Space"
					    delimiter))))))))

