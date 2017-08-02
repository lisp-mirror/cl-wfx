(in-package :cl-wfx)

(defun complex-type (field)
  (if (listp (getf field :db-type))
      (or (dig field :db-type :complex-type)
	  (dig field :db-type :type))
      (getf field :db-type)))

(defun simple-type (field)
  (if (listp (getf field :db-type))
      (dig field :db-type :type)
      (getf field :db-type)))



(defun grid-fetch-items (data-type collection &key test (result-type 'list))
  (let ((items)
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name))))
    

    (unless (getcx data-type :stores)
      (setf (getcx data-type :stores)
	    (collection-stores *system* collection-name)))
    
    (dolist (store (getcx data-type :stores))
      (setf items (append items (fetch-items 
				 (get-collection
				  store 
				  collection-name)
				 :test test
				 :result-type result-type))))
    items))


(defun print-item-val-s* (field item)
  (let ((*print-case* :downcase))
    
    (frmt "~S"
	  (getfx item field))))

(defun print-item-val-a* (field item)
  (let ((*print-case* :downcase))
    (frmt "~A"
	  (getfx item field))))

(defmethod print-item-val ((type (eql :symbol)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :keyword)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :script)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :string)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :email)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :number)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :integer)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :date)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :boolean)) field item
			   &key &allow-other-keys)
  (if (getfx item field)
      "checked"
      ""))

(defmethod print-item-val ((type (eql :key-value)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :item)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))


(defmethod print-item-val ((type (eql :value-list)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :key-value-list)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :value-string-list)) 
			   field item &key &allow-other-keys)
  (let* ((delimiter (dig field :db-type :delimiter))
	 (val))
    (dolist (x (getsfx (dig field :db-type :type) field item))
      (setf val 
	    (if val
		(concatenate 'string val delimiter 
			     (if (equalp (dig field :db-type :type)
					 :keyword)
				 (string-downcase (frmt "~S" x))
				 x))
		(if (equalp (dig field :db-type :type)
			    :keyword)
		    (string-downcase (frmt "~S" x))
		    x))))
    val))

(defmethod print-item-val ((type (eql :list-items)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection-items))
			   field item &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :contained-item)) field item
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection-contained-item)) 
			   field item &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection)) field item
			   &key &allow-other-keys)

  (let ((item-val (getfx item
			 field 
			 )))    
    (when item-val
      (frmt "~A" (getx 
		  item-val
		  (dig field :db-type :accessor))))))

(defmethod print-item-val ((type (eql :hierarchical)) field item 
			   &key &allow-other-keys)
  ;;TODO: Sort this shit out need to loop tree
  (let ((item-val (getfx field item)))    
    (when item-val
      (frmt "~A" (getx 
		  item-val
		  (dig :db-type :accessor))))))

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

(defmethod render-input-val ((type (eql :symbol)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :keyword)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :string)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :email)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :number)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :integer)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :date)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :boolean)) field item &key &allow-other-keys)
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
			(dig field :db-type :type)
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
			(dig field :db-type :type)
			field 
			item)))
	  (:span (cl-who:str
		  (frmt "Delimit by ~A" (if (string-equal delimiter " ")
					    "#\Space"
					    delimiter))))))))

(defmethod render-input-val ((type (eql :value-list)) field item
			     &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (dig :db-type :values))
	 (selected (find (getfx field item) list :test #'equalp)))

    (with-html-string
      (:div :class "dropdown"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" name) :value "")
	    (:button :class "btn btn-secondary dropdown-toggle"
		     :type "button"
		     :data-toggle "dropdown"
		     :aria-haspopup "true" :aria-expanded "false"
		     (if selected
			 (cl-who:str (frmt "~S" selected))))
	    
	    (:div :class "dropdown-menu" :aria-labelledby (frmt "wtf-~A" name)
		  (dolist (option list)
		    (cl-who:htm
		     (:span :class "dropdown-item" 
			    (:input :type "hidden"
				    :value (frmt "~S" option))
			    (cl-who:str (frmt "~S" option))))))))))

(defmethod render-input-val ((type (eql :collection-items)) 
			     field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :list-items)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))





(defmethod render-input-val ((type (eql :hierarchical)) field item 
			     &key &allow-other-keys)

  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :collection-items))
			     field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :list-items)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))




(defmethod render-input-val ((type (eql :collection)) field item 
			     &key &allow-other-keys)

  (let* ((name (getf field :name))
	 (list (grid-fetch-items (dig field :db-type :data-type)
				 (dig field :db-type :collection)))
	 (selected (find (getx item name)  
			 list :test #'equalp))
	 (accessors (dig field :db-type :accessor)))
    
    (with-html-string
      (:div :class "dropdown"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" name) :value "")
	    (:button :class "btn btn-secondary dropdown-toggle"
		     :type "button"
		     :data-toggle "dropdown"
		     :aria-haspopup "true" :aria-expanded "false"
		     (if selected
			 (cl-who:str (apply #'digx
					    selected
					    (if (listp accessors)
						accessors
						(list accessors))))))
	    
	    (:div :class "dropdown-menu" :aria-labelledby (frmt "wtf-~A" name)
		  (dolist (option list)
		    (cl-who:htm
		     (:span :class "dropdown-item" 			      
			    (:input :type "hidden"
				    :value (frmt "~A" (item-hash option)))
			    (cl-who:str
			     (apply #'digx option
				    (if (listp accessors)
					accessors
					(list accessors))))))))))))

(defun grid-js-render-form-values (data-type form-id 
				   &key action item-id )
  (let ((active-page (getcx 
		      data-type :active-page)))
    (js-render-form-values 
     "cl-wfx:ajax-grid"
     (gethash :collection-name (cache *context*))
     form-id
     (js-pair "data-type"
	      (frmt "~A" data-type))
     
     (js-pair "action" (or action ""))
     
     (js-pair "item-id" (frmt "~A" (or item-id
				       (getcx 
					data-type :item-id)
				       "")))
     (js-pair "pages"
	      (or (parameter "pages") 10))
     (js-pair "page"
	      (or active-page 1)))))

(defun grid-js-render (data-type &key action item-id)
  (let ((active-page (getcx data-type :active-page)))

    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "action" (or action ""))
	       
	       (js-pair "item-id" (frmt "~A" (or item-id "")))

	       (js-pair "pages"
			(or (parameter "pages") 10))
	       (js-pair "page"
			(or active-page 1)))))



(defun render-expand-buttons (subs data-type item)
  (if subs
      (if (equalp (ensure-parse-integer 
		   (getcx data-type :expand-id)) 
		  (item-hash item))
	  (with-html-string
	    (:button
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm active "
	     :aria-pressed "true"
	     :onclick (grid-js-render data-type
				      :action "unexpand")
	     "-"))
	  (with-html-string
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm border-0"
	     :aria-pressed "false"
	     :onclick (grid-js-render data-type
				      :action "expand"
				      :item-id (item-hash item))
	     (cl-who:str "+"))))))


(defun render-grid-buttons (data-type item)
  (let ((permissions (getx (context-spec *context*) :permissions)))
    (with-html-string
      (dolist (permission permissions)
	(cond ((equalp permission :update)
	       (cl-who:htm
		
		(:button ;;:tabindex -1 ;;when disabled
		 :name "edit" :type "submit" 
		 :class
		 (if (and (parameter "item-id")
			  (string-equal 
			   (parameter "item-id") 
			   (frmt "~A" (item-hash item))))
		     "btn btn-outline-primary btn-sm active"
		     "btn btn-outline-primary btn-sm")
		 :aria-pressed 
		 (if (and (parameter "item-id")
			  (string-equal 
			   (parameter "item-id") 
			   (frmt "~A" (item-hash item))))
		     "true"
		     "false")
		 :onclick 
		 (grid-js-render data-type
				 :action "edit"
				 :item-id (item-hash item))
		 "Edit")))
	      ((equalp permission :delete)
	       (cl-who:htm
		(:button ;;:tabindex -1 ;;when disabled
		 :name "edit" :type "submit" 
		 :class (if (and (parameter "item-id")
				 (string-equal 
				  (parameter "item-id") 
				  (frmt "~A" (item-hash item))))
			    "btn btn-outline-primary btn-sm active"
			    "btn btn-outline-primary btn-sm")
		 :aria-pressed (if (and (parameter "item-id")
					(string-equal 
					 (parameter "item-id") 
					 (frmt "~A" (item-hash item))))
				   "true"
				   "false")
		 :onclick 
		 (grid-js-render data-type
				 :action "delete"
				 :item-id (item-hash item))
		 "Delete"))))))))


(defun render-grid-edit (data-type fields item parent-item
			 parent-spec)
  

  (setf (getcx data-type :edit-item) item)
  (setf (getcx data-type :parent-spec) parent-item)
  (setf (getcx data-type :parent-spec) parent-spec)
  (setf (getcx (parameter "data-type") :item-id) (item-hash item))
  
  
  (with-html-string
    (:div :class "card" :id (string-downcase (frmt "grid-edit-~A"  data-type))
	  (:div :class "card-header"
		(cl-who:str (frmt "Editing... ~A" (string-capitalize data-type))))
	  (:div
	   :class "card-block"
	   (:div :class "row" 
		 :id (frmt "~A" data-type)
		 (:div :class "col" 
		       
		       (dolist (field fields)
			 (let* ((name (getf field :name))
				(label (getf field :label)))
			   
			   (when (and (digx field :attributes :display) 
				      (not (find (complex-type field)
						 (list :collection-items
						       :list-items))))
			     
			     (cl-who:htm
			      (:div :class (if (digx field :attributes :editable)
					       "form-group row"
					       "form-group row disabled")
				    (:label :for name 
					    :class "col-sm-2 col-form-label" 
					    (cl-who:str
					     (if label
						 label
						 (string-capitalize 
						  (substitute 
						   #\Space  
						   (character "-")  
						   (format nil "~A" name) 
						   :test #'equalp)))))
				   ;; (break "~A ~S" label (complex-type field))
				    (:div :class "col"
					  (cl-who:str
					   (render-input-val 
					    (complex-type field) 
					    field item
					    :parent-item
					    parent-item)))))))))))
	  (:div :class "card-footer"
		(when (gethash :validation-errors (cache *context*))
		  (let ((errors (gethash :validation-errors (cache *context*))))
		    
		    (setf (gethash :validation-errors (cache *context*)) nil)
		    (setf (gethash :validation-error-item-id
				   (cache *context*))
			  nil)
		    
		    (cl-who:htm
		     (:div :class "row"
			   (:div :clas "col"
				 (cl-who:str
				  (frmt "Errors ~S"
					errors)))))))
		
		(:div :class "row"
		      (:div :class "col"
			    (:button
			     :name "save" 				   
			     :type "submit" 
			     :class "btn btn-outline-primary btn-sm"
			     :onclick 
			     (grid-js-render-form-values			 
			      data-type
			      (string-downcase
			       (frmt "grid-edit-~A"  data-type))
			      :action "save")
			     "Save")
			    (:button
			     :name "cancel" 				   
			     :type "submit" 
			     :class "btn btn-outline-primary btn-sm float-right"
			     :onclick 
			     (grid-js-render data-type
					     :action "cancel")
			     "Cancel")))))))

(defun render-grid-col-filter (data-type col-name)
  (with-html-string
    (:input :class "w-100"
	    :type "text" 
	    :name (frmt "~A-filter" col-name) 

	    :id (frmt "~A-filter" col-name)
	    :value (or (parameter (frmt "~A-filter" col-name))
		       (getcx 
			data-type (frmt "~A-filter" col-name))
		       "")
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     (frmt "~A-filter" col-name)
	     13
	     "cl-wfx:ajax-grid"
	     (gethash :collection-name (cache *context*))
	     (js-pair "data-type"
		      (frmt "~A" data-type))
	     (js-pair "action" "grid-col-filter")))))

(defun rough-half-list (list &optional (half-if-length 2))
  (when list
    (let ((length (length list)))
      (if (and (> length 1)
	       (> length half-if-length))
	  (multiple-value-bind (half)
	      (floor length 2)
	    (list
	     (subseq list 0 half)
	     (subseq list half)))
	  (list list)))))

(defun get-header-fields (fields)
  (let ((header-fields))
    (dolist (field fields)
      (when (and (digx field :attributes :display)
		 (not (find (complex-type field)
			    (list :collection-items :list-items))))
	(pushnew field header-fields)))
    (reverse header-fields)))

(defun render-filter-row (data-type fields sub-p subs)
  (with-html-string
    (:div :class "collapse" :id "collapseFilter"
	  (:div :class "row"	
		(when subs
		  (cl-who:htm
		   (:div :class "col-sm-1"
			 " ")))
		
		(dolist (half (rough-half-list (get-header-fields fields) 7))
		  (cl-who:htm
		   (:div :class "col"
			 (:div :class "row no-gutters"
			       (dolist (field half)
				 (let* ((name (getf field :name)))
				   
				   (cl-who:htm
				    (:div :class "col"
					  (cl-who:str
					   (render-grid-col-filter 
					    data-type name))))))))))
		(if sub-p
		    (cl-who:htm	      
		     (:div :class "col-sm-2"))
		    (cl-who:htm	      
		     (:div :class "col-sm-2"
			   (cl-who:str (render-grid-search data-type)))))))))

(defun render-header-row (data-type fields sub-p subs)
  (with-html-string
    (:div :class "row"	
	  (when subs
	    (cl-who:htm
	     (:div :class "col-sm-1"
		   " ")))
	  
	  (dolist (half (rough-half-list (get-header-fields fields) 7))
	    (cl-who:htm
	     (:div
	      :class "col"
	      (:div :class "row no-gutters"
		    (dolist (field half)
		      
		      (let* ((name (getf field :name))
			     (label (getf field :label)))
			
			(cl-who:htm
			 (:div :class "col text-left"
			       (:h6 (cl-who:str
				     (if label
					 label
					 (string-capitalize 
					  (substitute #\Space  (character "-")  
						      (format nil "~A" name) 
						      :test #'equalp)))))))))))))
	  
	  (if sub-p
	      (cl-who:htm	      
	       (:div :class "col-sm-2"))
	      (cl-who:htm	      
	       (:div :class "col-sm-2"
		     (cl-who:str
		      (render-grid-sizing data-type))))))))

(defun render-grid-header (data-type sub-p)
  (let ((fields (getcx	data-type :fields))
	(subs))
    (dolist (field fields)
      (when (find (complex-type field)
		  (list :collection-items :list-items))
	(pushnew field subs)))
    (with-html-string
      (unless sub-p	  
	(cl-who:str (render-filter-row data-type fields sub-p subs)))
      
      (cl-who:str (render-header-row data-type fields sub-p subs)))))

(defun get-data-fields (fields)
  (let ((data-fields))
    (dolist (field fields)
      (when (and (digx field :attributes :display)
		 (not (find (complex-type field)
			    (list :collection-items :list-items))))
	(pushnew field data-fields)))
    (reverse data-fields)))

(defun render-grid-data (data-type page-items sub-level
			 parent-item parent-spec)

  (let ((sub-level-p (not (equalp data-type 
				   (gethash :data-type (cache *context*))))))
    
    (when sub-level-p
      ;; (parse-data-spec-for-grid data-type)
      )  
    
    (with-html-string
      (let ((fields (getcx data-type :fields))
	    (subs))

	
	
	(dolist (field fields)
	  (when (find (complex-type field) (list :collection-items :list-items))
	    (pushnew field subs)))

	(dolist (item page-items)
	  (cl-who:htm
	   (:div :class "row "
		 
		 (if subs
		     (cl-who:htm
		      (:div :class "col-sm-1"
			    (cl-who:str
			     (render-expand-buttons subs data-type item)))))
		 (dolist (half (rough-half-list (get-data-fields fields) 7))
		   (cl-who:htm
		    (:div :class "col"
			  (:div :class "row no-gutters"
				(dolist (field half)
				  (cl-who:htm
				   (:div 
				    :class "col text-left text-truncate"
				    
				    (let ((val (print-item-val 
						(complex-type field)
						field item)))

				      (if (> (length val) 100)
					  (cl-who:str 
					   (subseq val 0 100))
					  (cl-who:str 
					   val))))))))))

		 (:div :class "col-sm-2"
		       (:div :class "btn-group float-right"
			     (cl-who:str
			      (render-grid-buttons data-type item ))))))
	  
	  
	  (if (and (or (and (equalp (parameter "action") "edit")
			    (parameter "item-id")) 
		       (gethash :validation-error-item-id
				(cache *context*)))
		   (string-equal (parameter "data-type")
				 (frmt "~A" data-type))
		   (string-equal (or (parameter "item-id") 
				     (gethash :validation-error-item-id
					      (cache *context*)))
				 (frmt "~A" (item-hash item))))
	      
	      (cl-who:str
	       (render-grid-edit data-type fields item
				 parent-item parent-spec)))
	  

	 
	 	  (when (equalp (ensure-parse-integer
			 (getcx data-type :expand-id)) 
			(item-hash item))
	    
	    (unless sub-level-p
	      (setf (gethash :root-item (cache *context*)) item))
	    
	    (dolist (sub subs)
	      (let* ((sub-data-spec (dig sub :db-type :data-type)))

		(when (equalp (ensure-parse-integer
			       (getcx sub-data-spec :expand-id)) 
			      (item-hash item)))
		
		;; (set-grid-expand sub-data-spec)
		

		(unless (getcx sub-data-spec :data-type)
		  (setf (getcx sub-data-spec :data-type)
			(find-type-def *system* 
				       sub-data-spec))
		  
		  (setf (getcx sub-data-spec :fields) 
			(dig (getcx sub-data-spec :data-type)
			     :data-type :fields)))
		
		;;	(parse-data-spec-for-grid sub-data-spec)
		(setf (getcx sub-data-spec :active-item) item)

		
		(cl-who:htm
		 (:div
		  :class "row"
		  (:div :class "col"
			(:div :class "card"
			      (:h4 :class "card-title"
				   (cl-who:str
				    (frmt "~A" (string-capitalize
						(getf sub :name)))))
			      (:div :class "card-header"
				    (cl-who:str
				     (render-grid-header
				      sub-data-spec
				      t)))
			      (:div :class "card-block"
				    (cl-who:str
				     (render-grid-data
				      sub-data-spec
				      (getfx item sub) 
				      (+ sub-level 1)
				     
				      item
				      data-type)))
			      (:div
			       :class "card-footer"
			       (:div :class "row"	  
				     (:div :class "col"
					   (:button
					    :name "new" :type "submit" 
					    :class "btn btn-outline-success"
					    :aria-pressed "false"
					    :onclick 
					    (grid-js-render
					     sub-data-spec
					     :action "new" )
					    (cl-who:str "+")))))))))))))
	
	(when (and (equalp (parameter "action") "new")
		   (string-equal (parameter "data-type") (frmt "~A" data-type)))
	  (let ((item (make-item :data-type data-type)))

	    (unless sub-level-p
	      (setf (getcx data-type :root-item) item))
	    
	    (cl-who:str
	     (render-grid-edit data-type fields 
			       item 
			       parent-item
			       parent-spec))))))))

(defun render-grid-sizing (data-type)
  (with-html-string
    (:input :type "text" :name "pages" 
	    :size 2
	    :id "pages"
	    :value (or  (parameter "pages")
			(getcx 
			 data-type :show-pages-count)
			10)
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "pages"
	     13
	     "cl-wfx:ajax-grid"
	     (gethash :collection-name (cache *context*))
	     (js-pair "data-type"
		      (frmt "~A" data-type))	     
	     (js-pair "action" "grid-sizing")))))

(defun render-grid-search (data-type)
  (with-html-string
    (:input :type "text" 
	    :name "search" 	   
	    :id "search"
	    :value (or (parameter "search")
		       (getcx 
			data-type :search)
		       "")
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "search"
	     13
	     "cl-wfx:ajax-grid"
	     (gethash :collection-name (cache *context*))
	     (js-pair "data-type"
		      (frmt "~A" data-type))
	     (js-pair "action" "grid-search")))))



(defun fetch-grid-page-data (data-type items)  
  (setf (getcx data-type :data-count) (length items))
  
  (setf (getcx data-type :show-pages-count) 
	(if (not (empty-p (parameter "pages")))
	    (parse-integer (parameter "pages"))
	    (or (getcx 
		 data-type :show-pages-count)
		10)))
  
  (setf (getcx data-type :active-page)
	(if (not (empty-p (parameter "page")))
	    (parse-integer (parameter "page"))
	    (or (getcx 
		 data-type :active-page)
		1)))


  (multiple-value-bind (page-count rem)
      (floor (getcx data-type :data-count) 
	     (getcx data-type :show-pages-count))
    
    (setf (getcx data-type :page-count) page-count)
    (setf (getcx data-type :page-count-remaining) rem))
  
  (setf (getcx data-type :start-page-count) 
	(- (* (getcx 
	       data-type :active-page) 
	      (getcx 
	       data-type :show-pages-count)) 
	   (getcx 
	    data-type :show-pages-count)))
  
  (setf (getcx data-type :end-page-count) 
	(if (< (* (getcx 
		   data-type :active-page)
		  (getcx 
		   data-type :show-pages-count)) 
	       (getcx 
		data-type :data-count))
	    (* (getcx 
		data-type :active-page)
	       (getcx 
		data-type :show-pages-count))))
  
  (when items
    (subseq items 
	    (getcx data-type :start-page-count) 
	    (getcx data-type :end-page-count))))

(defun found-nil-p (list)
  (dolist (item list)
    (unless item
      (return-from found-nil-p t))))

(defun filter-found-p (filter-term val)
  (let ((terms (split-sequence:split-sequence #\| filter-term))
	(found))
    
    (dolist (term terms)
      (push (search term 
		    val
		    :test #'string-equal)
	    found))
    
    (remove-if #'not found)))

(defun filter-function (data-type)
  (lambda (item)
    
    (let ((found nil))
      (dolist (field (getcx 
		      data-type 
		      :filter-fields))
	(let ((filter-term 
	       (or
		(parameter 
		 (frmt "~A-filter" (getf field :name)))
		(getcx 
		 data-type (frmt "~A-filter" 
				 (getf field :name))))))
	  (when filter-term			       
	    (when (getf field :db-type)
	      (when (find (complex-type field)
			  (list :collection-items :list-items))
		(dolist (sub-val (getfx field item))
		  (when sub-val
		    (let* ((val (apply #'digx (dig field :db-type :accessor))))
		      
		      (if (filter-found-p filter-term val)
			  (push t found)
			  (push nil found))))))
	      
	      (let ((val (print-item-val 
			  (complex-type field) field item)))
		(if (filter-found-p filter-term val)
		    (push t found)
		    (push nil found)))))))
      
      
      (unless (found-nil-p found)
	item))))

(defun search-function (data-type search-term)
  (lambda (item)
    
    (let ((found nil))
      (dolist (field (getcx 
		      data-type 
		      :fields))
	(when (getf field :db-type)
	  (when (find (complex-type field) (list :collection-items :list-items))
	    (dolist (sub-val (getfx field item))
	      
	      (when sub-val
		
		(let* ((val (apply #'digx (dig field :db-type :accessor))))
		  (when val
		    (when (search search-term 
				  val
				  :test #'string-equal)
		      (unless found		       
			
			(setf found t))))))))
	  (let ((val (print-item-val 
		      (complex-type field) field item)))
	    (when val
	      (when (search search-term 
			    val
			    :test #'string-equal)
		(unless found		       				     
		  (setf found t)))))))
      (when found
	item))))

(defun fetch-grid-data (data-type)
  (let* ((items )
	 (collection-name (gethash :collection-name (cache *context*)))
	 (search-term (or (parameter "search") 
			  (getcx 
			   data-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (getcx 
		    data-type :filter)))
    
    
    
    (unless (or search-p filter-p)
      (setf items (grid-fetch-items data-type collection-name)))
    
    
    (when (or search-p filter-p)
      
      (if (getcx 
	   data-type 
	   :filter-fields)
	  
	  (setf items
		(grid-fetch-items data-type collection-name
				  :test (filter-function data-type)))
	  (setf items
		(grid-fetch-items data-type collection-name
				  :test (search-function
					 data-type search-term))))
      
      (when items
	(setf items
	      (find-in-item-list
	       items
	       (search-function data-type search-term)))))
    
    (fetch-grid-page-data data-type items)))

(defun render-grid-paging (data-type)  
  (let ((active-page (getcx 
		      data-type :active-page))
	(how-many-rem (getcx 
		       data-type :page-count-remaining))
	(how-many-pages (getcx 
			 data-type :page-count)))
    
    (with-html
      (:nav
       (:ul :class "pagination justify-content-end"
	    
	    (:li :class "page-item"
		 (:button ;;:tabindex -1 ;;when disabled
		  :name "page" :type "submit" 
		  :class (if (> active-page 1)
			     
			     "btn page-link"
			     "btn page-link disabled")		  
		  :onclick 
		  (js-render "cl-wfx:ajax-grid"
			     (gethash :collection-name (cache *context*))
			     (js-pair "data-type"
				      (frmt "~A" data-type))
			     (js-pair "pages"
				      (or (parameter "pages") 10))
			     (js-pair "page"
				      (if (> active-page 1)
					  (- active-page 1)))
			     (js-pair "action" "page-previous"))
		  "Previous"))
	    
	    (let ((real-page-count (if (>  how-many-rem 0)
				       (+ how-many-pages 1)
				       how-many-pages
				       )))
	      (dotimes (i real-page-count)		
		(cl-who:htm
		 (:li :class (if (equalp active-page (+ i 1))
				 "page-item active"
				 "page-item")
		      (:button 
		       :name "page" :type "submit" 
		       :class "btn page-link"
		       
		       :onclick 
		       (js-render "cl-wfx:ajax-grid"
				  (gethash :collection-name (cache *context*))
				  (js-pair "data-type"
					   (frmt "~A" data-type))
				  (js-pair "pages"
					   (or (parameter "pages") 10))
				  (js-pair "page"
					   (+ i 1))				 
				  (js-pair "action" "page"))
		       (cl-who:str (+ i 1))))))
	      
	      (cl-who:htm
	       (:li :class "page-item"
		    (:button ;;:tabindex -1
		     :name "page" :type "submit" 
		     :class (if (< active-page real-page-count)
				
				"btn page-link"
				"btn page-link disabled")
		     
		     :onclick 
		     (js-render "cl-wfx:ajax-grid"
				(gethash :collection-name (cache *context*))
				(js-pair "data-type"
					 (frmt "~A" data-type))
				(js-pair "pages"
					 (or (parameter "pages") 10))
				(js-pair "page"
					 (if (< active-page real-page-count)
					     (+ active-page 1)))
				(js-pair "action" "page-next"))
		     "Next")))))))))



(defmethod action-handler ((action (eql :assign-campaign)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (gethash :assign-campaign (cache *context*)) t))


(defun getcx (&rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))
    
    (if indicators
	(naive-dig place indicators)
	place)))

(defun (setf getcx) (value &rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))
    
    (if indicators
	(setf (gethash indicator (cache *context*)) 
	      (set-naive-dig place indicators value))
	(setf (gethash indicator (cache *context*)) value))))


(defun set-grid-filter (data-type)
  (when (equalp (parameter "action") "filter")
    (setf (getcx data-type :filter) t))
  
  (when (equalp (parameter "action") "un-filter")
    (setf (getcx data-type :filter) nil))

  
  (when (equalp (parameter "action") "grid-col-filter")
    (let ((fields (getcx data-type :filter-fields)))
      (dolist (field (getcx data-type :fields))
	(when (parameter (frmt "~A-filter" (getf data-type :name)))
	  
	  (pushnew field fields)
	  
	  (setf (getcx data-type
		       (intern (string-upcase
				(frmt "~A-filter" (getf field :name)))))  
		(parameter (frmt "~A-filter" (getf field :name))))))
      
      (setf (getcx data-type :filter-fields)fields))))


(defun set-type-context (data-type)
  (unless (getcx data-type :data-type)
    (setf (getcx data-type :data-type)
	  (find-type-def *system* data-type))

    (setf (getcx data-type :fields) 
	  (dig (getcx data-type :data-type) :data-type :fields))))

(defun set-grid-context (collection-name data-type)
  (unless (gethash :collection-name (cache *context*))

    (setf  (gethash :collection-name (cache *context*)) collection-name)
    (setf  (gethash :data-type (cache *context*)) data-type)
    
    (setf (getcx data-type :stores)
	  (collection-stores *system* collection-name))
    
    (unless (equalp (parameter "action") "save")
      (setf (getcx data-type :root-item) nil))
    
    (setf (getcx data-type :search)
	  (or (parameter "search")
	      (getcx data-type :search)))))

(defun set-grid-expand ()
  
  (when (equalp (parameter "action") "expand")
    (setf (getcx (parameter "data-type") :expand-id) (parameter "item-id")))
  
  (when (equalp (parameter "action") "unexpand")    
    (setf (getcx (parameter "data-type") :expand-id) nil)))


(defun render-grid (collection-name)
  (let* ((collection (if (not (gethash :collection-name (cache *context*)))
			 (find-collection-def *system*   
					      collection-name)))
	 (data-type (or (gethash :data-type (cache *context*))
			(and collection
			     (dig collection :collection :data-type)))))   
    
    (set-grid-context collection-name data-type)

    (set-type-context data-type)

    (set-type-context (parameter "data-type"))
    
    (set-grid-filter data-type)
    
    (set-grid-expand)
    
    (let ((page-items (fetch-grid-data data-type)))
      
      ;;(campaign-shit page-items)
      
      (with-html-string  
	(:div :id (gethash :collection-name (cache *context*))
	      :class "card"
	      (:h4 :class "card-title"
		   (cl-who:str collection-name)
		   (:button
		    :class "btn btn-small btn-outline-secondary float-right"
		    :name "filter-grid" 
		    
		    :data-toggle "collapse" :href "#collapseFilter" 
		    :aria-expanded "false" :aria-controls="collapseFilter"
		    :aria-pressed "false"
		    (:i :class "fa fa-filter "))
		   

		   (:button
		    :class "btn btn-small btn-outline-secondary float-right"
		    :name "export" 
		    :type "submit" 
		    
		    :aria-pressed "false"
		    :onclick 
		    (grid-js-render data-type
				    :action "export")
		    (:i :class "fa fa-download")))
	      (:div :class "card-header"
		    (cl-who:str
		     (render-grid-header data-type nil)))
	      (:div :class "card-block"
		    (cl-who:str
		     (render-grid-data data-type page-items 0 nil nil)))
	      
	      (:div :class "card-footer"
		    (:div :class "row"	  
			  (:div :class "col"
				(:button 
				 :name "expand" :type "submit" 
				 :class "btn btn-outline-success"
				 :aria-pressed "false"
				 :onclick 
				 (grid-js-render data-type :action "new")
				 (cl-who:str "+"))
				(cl-who:str
				 (render-grid-paging data-type))))))))))



(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  
  (render-grid (getx (context-spec *context*) :name)))


(defmethod action-handler ((action (eql :save)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (let* ((data-type (parameter "data-type"))
	 (fields (getcx data-type :fields))
	 (parent-slot nil ;;(getcx data-type :list-field-name)
	   ))
    
    (setf (gethash :validation-errors (cache *context*)) nil)
    (setf (gethash :validation-error-item-id (cache *context*)) nil)

  
    
    (when fields
      (let ((item (getcx data-type :edit-item))
	    (parent-item (getcx data-type :parent-item)))
	
	(unless item
	  (setf item (make-item :data-type data-type)))
	
	(dolist (field fields)
	  
	  (when (and (digx field :attributes :editable)
		     (getf field :db-type)
		     (not (find (complex-type field)
				(list :collection-items :list-items))))
	   
	    (let* ((field-name (frmt "~A" (getf field :name)))
		  (valid (if (equalp (complex-type field) :item)
			     (validate-sfx (complex-type field)
					   field 
					   item 
					   (parameter field-name))
			     (list t nil))))
	      
	      (unless (first valid)
		(pushnew 
		 (list field-name (second valid))
		 (gethash :validation-errors (cache *context*))))
	      
	      (when (first valid)
		
		(if (equalp (complex-type field) :hierarchical)
		    (setf (getfx item field				 
				 :source (getcx 
					  (dig field :db-type :data-type) 
					  :active-item))
			  (parameter field-name))
		    (setf (getfx item field ) 
			  (parameter field-name)))
	
		))))
	
	
	;;TODO: is this still needed????
	;;Doing this to keep edit window open.
	(when (gethash :validation-errors (cache *context*))
	  (setf (gethash :validation-error-item-id
			 (cache *context*))
		(parameter "data-id")))

	(unless (gethash :validation-errors (cache *context*))
	  
	  ;;Append parent-slot only if new
	  (when (and parent-slot (not (item-hash item)))
	    
	    (setf (getx parent-item parent-slot)
		  (append (getx parent-item parent-slot)
			  (list item))))

;;	  (break "??? ~A ~A" item		 (getcx data-type :root-item))
	  
	  (persist-item
	   (get-collection
	    (first (collection-stores *system*
		    (gethash :collection-name (cache *context*))))
	    (gethash :collection-name (cache *context*)))
	   (getcx data-type :root-item))
	  
	  (setf (getcx data-type :parent-item) nil)
	  (setf (getcx data-type :edit-item) nil)
	  (setf (getcx data-type :item-id) nil))))))


(defun store-from-stash (store-name)
 
  (dolist (store (getcx (gethash :data-type (cache *context*)) :stores))
    (break "~S ~S" store-name (name store))
    (when (equalp store-name (name store))
      
      (return-from store-from-stash store))))


(defun campaign-shit (page-items)
  (when (gethash :assign-campaign (cache *context*))
    (setf (gethash :assign-campaign (cache *context*)) nil)
    (when (not (empty-p (parameter "campaign")))
      (let ((campaign 
	     (fetch-item "campaigns" 
			 :test (lambda (doc)
				 (equalp (item-hash doc) 
					 (parse-integer
					  (parameter "campaign")))))))
	(when campaign
	  (dolist (company page-items)
	    (let ((exists-p)
		  (campaigns-slot (intern "CAMPAIGNS" 'cl-bizhub))
		  (campaign-slot (intern "CAMPAIGN" 'cl-bizhub))
		  (company-campaign (intern "COMPANY-CAMPAIGN" 'cl-bizhub)))
	      
	      (dolist (campaignx (slot-value company campaigns-slot))
		(when (equalp (item-hash campaign)
			      (item-hash
			       (slot-value campaignx campaign-slot)))
		  (setf exists-p t)))
	      
	      (unless exists-p
		(setf (slot-value company campaigns-slot)
		      (append (slot-value company campaigns-slot) 
			      (list (make-instance
				     company-campaign
				     :campaign campaign))))))))))))

(defun more-campaign-shit (name)
  (when (string-equal (frmt "~A" name) "company")
    (with-html-string
      (let ((items))
	(setf items 
	      (append items (wfx-fetch-items 
			     "campaigns"
			     :result-type 'list)))
	(let ((campaigns items))
	  (cl-who:htm
	   (:form :method "post" :id "assign-campaign-id"
		  (:button
		   :class "btn btn-small btn-outline-success float-right"
		   :name "assign-campaign" 
		   :type "submit" 
		   
		   :aria-pressed "false"
		   :onclick 
		   (grid-js-render-form-values
		    name
		    "assign-campaign-id"
		    :action "assign-campaign")
		   "<>")
		  
		  (:div
		   :class "dropdown float-right"
		   (:input :type "hidden" :class "selected-value" 
			   :name "campaign" :value "")
		   (:button :class "btn btn-secondary dropdown-toggle"
			    :type "button"
			    :data-toggle "dropdown"
			    :aria-haspopup "true" :aria-expanded "false"
			    (if (parameter "campaign")
				(parameter "campaign")
				"Assign Campaign"))
		   
		   (:div
		    :class "dropdown-menu" 
		    ;; :aria-labelledby "campaign"
		    (dolist (option campaigns)
		      (cl-who:htm
		       (:span :class "dropdown-item" 			     
			      (:input :type "hidden"
				      :value (frmt "~A"
						   (item-hash option)))
			      (frmt "~A" 
				    (slot-value 
				     option 
				     (intern "NAME" 'cl-bizhub)))))))))))))))
