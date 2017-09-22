(in-package :cl-wfx)

(defparameter *item-hierarchy* nil)

(defun complex-type (field)
  (if (listp (getf field :db-type))
      (or (dig field :db-type :complex-type)
	  (dig field :db-type :type))
      (getf field :db-type)))

(defun simple-type (field)
  (if (listp (getf field :db-type))
      (dig field :db-type :type)
      (getf field :db-type)))

(defun entity-type-p (fields)
  (dolist (field fields)
    (when (equalp (getf field :name) :entity)
      (return-from entity-type-p t))))

(defun grid-fetch-items (data-type collection &key test (result-type 'list))
  (let ((items)
	(entity-type-p (entity-type-p (getcx data-type :fields)))
	(entity-p (equalp data-type "entity"))
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name))))
    
    
    (unless (getcx data-type :stores)
      (setf (getcx data-type :stores)
	    (collection-stores *system* collection-name)))

    
    (dolist (store (getcx data-type :stores))
      (let ((collection (get-collection
				  store 
				  collection-name)))
	(when collection
	  (setf items
		(append items
			(fetch-items 
			 collection
			 :test (lambda (item)
				 (when item				  
				   (when (or
					  (and (not entity-type-p)
					       (not entity-p))
					  (and entity-type-p
					       (digx item :entity)
					       (find (item-hash
						      (digx item :entity))
						     (getx (active-user)
							   :selected-entities)
						     :test #'equalp))
					  (and entity-p
					       (find (item-hash
						      item)
						     (getx (active-user)
							   :selected-entities)
						     :test #'equalp)))
				     
				     (if test
					 (funcall test item)
					 item))))
				     :result-type result-type))))))
    
    items))

(defun grid-fetch-item (data-type collection &key test)
  (let ((items)
	(entity-type-p (entity-type-p (getcx data-type :fields)))
	(entity-p (equalp data-type "entity"))
	(collection-name (if (stringp collection)
			     collection
			     (digx collection :collection :name))))

    
    
    (unless (getcx data-type :stores)
      (setf (getcx data-type :stores)
	    (collection-stores *system* collection-name)))
    
    (dolist (store (getcx data-type :stores))
      (let ((collection (get-collection
				  store 
				  collection-name)))

	(when collection
	  (setf items
		(append
		 items
		 (list (fetch-item 
			collection
			:test (lambda (item)
				
				(when (or
				       (and (not entity-type-p)
					    (not entity-p))
				     
				       (and entity-type-p
					    (digx item
						  :entity)
					    (find (item-hash
						   (digx item
							 :entity))
						  (getx (active-user)
							:selected-entities)
						  :test #'equalp))
				     
				       (and entity-p
					    (find (item-hash
						   item)
						  (getx (active-user)
							:selected-entities)
						  :test #'equalp)))
				 
				  
				  (if test
				      (funcall test item)
				      item))))))))))
  
    (first (remove-if #'not items))))


(defun print-item-val-s* (field item)
  (let ((*print-case* :downcase)
	(val (getfx item field)))
    
    (if val
	(frmt "~S" val)
	"")))

(defun print-item-val-a* (field item)
  (let ((*print-case* :downcase)
	(val (getfx item field)))
    (if val
	(frmt "~A" val)
	"")))

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

(defmethod print-item-val ((type (eql :text)) field item
			   &key &allow-other-keys)
  (print-item-val-a* field item))

(defmethod print-item-val ((type (eql :image)) field item
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

(defmethod print-item-val ((type (eql :time)) field item
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
	 (val (getsfx (dig field :db-type :type) field item))
	 (final-val))
  
    (when (and val (listp val))
      
      (dolist (x val)
	(setf final-val 
	      (if final-val
		  (concatenate 'string final-val delimiter 
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
			   &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection-items))
			   field item &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :contained-item)) field item
			   &key &allow-other-keys)
  (let ((item-val (getfx item field))
	(accessor (dig field :db-type :accessor)))
    
    (when (or (listp item-val) (equalp (type-of item-val) 'item))
       (when item-val
	(frmt "~A"
	      (if (listp accessor)
		  (apply #'digx item-val accessor)
		  (getx
		    item-val		    
		    (dig field :db-type :accessor))))))))

(defmethod print-item-val ((type (eql :collection-contained-item)) 
			   field item &key &allow-other-keys)
  (print-item-val-s* field item))

(defmethod print-item-val ((type (eql :collection)) field item
			   &key &allow-other-keys)
  
  (let ((item-val (getfx item field))
	(accessor (dig field :db-type :accessor)))
    
    (when (or (listp item-val) (equalp (type-of item-val) 'item))
       (when item-val
	(frmt "~A"
	      (if (listp accessor)
		  (apply #'digx item-val accessor)
		  (getx
		    item-val		    
		    (dig field :db-type :accessor))))))))

(defmethod print-item-val ((type (eql :hierarchical)) field item 
			   &key &allow-other-keys)
  ;;TODO: Sort this shit out need to loop tree
  (let ((item-val (getfx item field))
	(final))    
    (when item-val
      (dolist (x item-val)
	(if final
	    (setf final (frmt "~A ~A"
			      final
			      (getx 
			       x
			       (dig field :db-type :accessor))))
	    (frmt "~A" (getx 
			x
			(dig field :db-type :accessor))))))))

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

(defmethod render-input-val ((type (eql :image)) field item
			     &key &allow-other-keys)

  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(if (getx item (getf field :name))
		    (cl-who:htm
		     (:img
		      :style "width:128px;height:128px;"
		      :src (frmt "/umage/cor/web/images/~A"
				      (getx item (getf field :name)))))
		    (cl-who:htm (:img :src "/umage/cor/web/images/logo-small.png"))))
	  (:div :class "col"
		(cl-who:str (getx item (getf field :name)))))))

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

(defmethod render-input-val ((type (eql :value-list)) field item
			     &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (dig field :db-type :values))
	 (selected (find (getfx item field) list :test #'equalp)))

    (with-html-string
      (:div :class "dropdown"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" name) :value "")
	    (:button :class "btn btn-secondary dropdown-toggle"
		     :type "button"
		     :data-toggle "dropdown"
		     :aria-haspopup "true" :aria-expanded "false"
		     (if selected
			 (cl-who:str (frmt "~S" selected))
			 (cl-who:str "Select a value")))
	    
	    (:div :class "dropdown-menu" :aria-labelledby (frmt "wtf-~A" name)
		  (dolist (option list)
		    (cl-who:htm
		     (:span :class "dropdown-item" 
			    (:input :type "hidden"
				    :value (frmt "~S" option))
			    (cl-who:str (frmt "~S" option))))))))))



(defmethod render-input-val ((type (eql :hierarchical)) field item 
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :collection-items))
			     field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :list-items)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :item)) field item
			     &key &allow-other-keys)
  (render-input-val* type field item))




(defmethod render-input-val ((type (eql :collection??)) field item 
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
		    :name (frmt "~A" name) :value
		    "")
	  
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
	    
	    (:div :class "dropdown-menu"
		  :style "overflow: scroll;"
		  :aria-labelledby (frmt "wtf-~A" name)
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

(defmethod render-input-val ((type (eql :collection)) field item 
			     &key data-type &allow-other-keys)

  (let* ((name (getf field :name))
	 (list (grid-fetch-items (dig field :db-type :data-type)
				 (dig field :db-type :collection)))
	 (selected (find (getx item name)  
			 list :test #'equalp))
	 (accessors (dig field :db-type :accessor)))

    (with-html-string
      (:div :class "auto-complete"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" name) :value
		    "")
	  
	    (:input :class "form-control auto-complete-text"
		    :type "text"
		    :placeholder "Press enter for list or start typing and then press enter for list..."
		    :name (frmt "~A-drop" name) 
		    :id (frmt "~A-drop" name)
		    :value (if selected
			       (cl-who:str (apply #'digx
						  selected
						  (if (listp accessors)
						      accessors
						      (list accessors))))
			       (or (parameter (frmt "~A-drop" name))
				   (getcx 
				    (dig field :db-type :data-type)
				    (frmt "~A-drop" name))
				   ""))
		    :onkeydown
		    ;;fires ajax call on enter (13)
		    (js-render-event-key 
		     (frmt "~A-drop" name)
		     13
		     "cl-wfx:ajax-auto-complete"
		     (frmt "~A-drop-div" name)
		     (js-pair "data-type"
			      data-type)
		     (js-pair "field-name"
			      (frmt "~A" name))
		     
		     
		     (js-pair "action" "grid-auto-complete")))
	    (:div :id (frmt "~A-drop-div" name) :class "auto-list"
		  )))))

(defmethod render-input-val ((type (eql :contained-item)) field item 
			     &key parent-item &allow-other-keys)

  (let* ((name (getf field :name))
	 (list (apply #'digx parent-item
		      (digx field :db-type :container-accessor)))
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
				   &key action action-script
				     action-data item-id )
  (let ((active-page (getcx 
		      data-type :active-page)))
    (js-render-form-values 
     "cl-wfx:ajax-grid"
     (gethash :collection-name (cache *context*))
     form-id
     (js-pair "data-type"
	      (frmt "~A" data-type))
     
     (js-pair "action" (or action ""))

     (js-pair "action-script" (or action-script ""))
     (js-pair "action-data" (or action-data ""))
     
     (js-pair "item-id" (frmt "~A" (or item-id (getcx data-type :item-id)
				       "")))
     (js-pair "pages"
	      (or (parameter "pages") 50))
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
			(or (parameter "pages") 50))
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
	     (cl-who:str "+"))
	    ))))


(defun render-select-button (item)
  (with-html-string
      (:div :class "form-check-label"
	    (:input	    
	     :type "checkbox"
	     :id "grid-selection"
	     :name "grid-selection"
	     :value (frmt "~A" (item-hash item))	    
	     :aria-label "..."))))


(defun grid-js-render-delete (data-type &key item)
  (let ((active-page (getcx data-type :active-page))
	(items))

    (dolist (hierarchy-item *item-hierarchy*)
      (setf items (pushnew 
		   (list (getf hierarchy-item :data-type)
			 (item-hash (getf hierarchy-item :item)))
		   items)))

    (setf items (reverse items))
    (setf items (pushnew (list data-type (item-hash item))
		      items))
    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "action" "delete")
	       
	       (js-pair "item-id" (frmt "~A" (or (item-hash item) "")))
	       (js-pair "item-hierarchy"
			(hierarchy-string (reverse items)))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))


(defun hierarchy-string (items)
  (setf items (remove-duplicates items :test #'equalp))
  (let ((hierarchy ""))
    (dolist (item items)
      (setf hierarchy  (concatenate 'string hierarchy " " (frmt "~A" item))))
    (frmt "(~A)" hierarchy)))

(defun grid-js-render-edit (data-type &key action item)
  (let ((active-page (getcx data-type :active-page))
	(items))

    (dolist (hierarchy-item *item-hierarchy*)
      (setf items (pushnew 
		   (list (getf hierarchy-item :data-type)
			 (item-hash (getf hierarchy-item :item)))
		   items)))

    (setf items (reverse items))
    (setf items (pushnew (list data-type (item-hash item))
			 items))
    (setf items (remove-duplicates items :test #'equalp))
    
    (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-edit-~A" (item-hash item))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "action" (or action ""))
	       
	       (js-pair "item-id" (frmt "~A" (or (item-hash item) "")))
	       (js-pair "item-hierarchy"
			(hierarchy-string (reverse items)))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

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
		 (grid-js-render-edit data-type
				 :action "edit"
			
				 :item item)
		 
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
		 (grid-js-render-delete data-type

				 :item item)
		 "Delete"))))))))

(defun render-grid-edit (data-type fields item parent-item
			 parent-spec)

;;  (break "~A" item)
  (setf (getcx data-type :edit-item) item)
  (setf (getcx data-type :parent-spec) parent-item)
  (setf (getcx data-type :parent-spec) parent-spec)
  (setf (getcx (parameter "data-type") :item-id) (item-hash item))


  (with-html-string
    (:div :class "card" :id (string-downcase (frmt "grid-edit-~A"  data-type))
	  (:div :class "card-header"
		(cl-who:str (frmt "Editing... ~A"
				  (string-capitalize data-type))))
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
						       :list-items
						       :hierarchical))))
			     
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
				    (:div :class "col"
					  (or
					  ;; (cl-who:str (parameter name))
					   (cl-who:str
					    (render-input-val 
					     (complex-type field) 
					     field item
					     :parent-item
					     parent-item
					     :data-type data-type))))))))))))
	  (:div :class "card-footer"
		(when (getcx data-type :validation-errors)
		  (let ((errors (getcx data-type :validation-errors)))
		    
		    (setf (getcx data-type :validation-errors) nil)
		    (setf (getcx data-type :validation-errors-id) nil)
		    
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
			    (list :collection-items :list-items :hierarchical))))
	(pushnew field header-fields)))
    (reverse header-fields)))

(defun render-filter-row (data-type fields sub-p subs)
  (let* ((search-term (or (parameter "search") 
			  (getcx 
			   data-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (or (getcx data-type :filter)
		       (getcx data-type :filter-fields))))
    (with-html-string
      (:div :class (if (or search-p filter-p)
		       "collapse show"
		       "collapse")
	    :aria-expanded (if (or search-p filter-p)
			       "true"
			       "false")
	    :id "collapseFilter"
	    
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
		        (:div :class "col-sm-2")
		       )
		      (cl-who:htm	      
		       (:div :class "col-sm-2"
			(cl-who:str (render-grid-search data-type))))))))))

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
		  (list :collection-items :list-items :hierarchical))
	(pushnew field subs)))
    (with-html-string
      (unless sub-p
	(cl-who:htm
	 (:div :class "card-header bg-white"	      
	       (cl-who:str (render-filter-row data-type fields sub-p subs)))))
      
      (:div :class "card-header"
	    (cl-who:str (render-header-row data-type fields sub-p subs))))))

(defun get-data-fields (fields)
  (let ((data-fields))
    (dolist (field fields)
      (when (and (digx field :attributes :display)
		 (not (find (complex-type field)
			    (list :collection-items :list-items :hierarchical))))
	(pushnew field data-fields)))
    (reverse data-fields)))


(defun render-select-actions (data-type)  
  (with-html-string
    (:div :class " float-right" :id "select-stuff" :name "select-stuff"
	  (:div :class "dropdown"
		(:input :type "hidden" :class "selected-value" 
			:name "select-action" :value "")
		(:button :class "btn btn-secondary dropdown-toggle"
			 :type "button"
			 :data-toggle "dropdown"
			 :aria-haspopup "true" :aria-expanded "false"
			 (cl-who:str "Select To Clipboard"))
		
		(:div :class "dropdown-menu" 
		      (dolist (option (list "Select To Clipboard"
					    "Export Selected"
					    "Export Selected - Raw"))
			(cl-who:htm
			 (:span :id (id-string option)
				:class "dropdown-item"
				:onclick
				(grid-js-render-form-values
				 data-type
				  (id-string option)
				 :action "select-action")
				(:input :id "select-action"
					:type "hidden"
					:value (id-string option))
				(cl-who:str option))))
		      
		      (dolist (script (getx (context-spec *context*) :scripts))
			
			(when (find :select (getx script :events) :test #'equalp)
			  
			  (cl-who:htm
			  
			   (:span :id (id-string (digx script :script :name))
				  :class "dropdown-item"
				  :onclick
				  (grid-js-render-form-values
				   data-type
				   data-type;;"select-stuff" ;;(id-string (digx script :script :name))
				   :action "select-action"
				   :action-data (id-string
						  (digx script :script :name)))
				  (:input :id "select-action" 
					  :type "hidden"
					  :value (id-string
						  (digx script :script :name)))
				  (cl-who:str (digx script :script :name))))))))

	  (dolist (script (getx (context-spec *context*) :scripts))
	    (when (find :select-action (getx script :events) :test #'equalp)
	      (cl-who:htm
	       (:div :class "dropdown float-right"
		     (:input :type "hidden" :class "selected-value"
			     :id "select-action-value"
			     :name "select-action-value" :value "")
		     (:button :class "btn btn-secondary dropdown-toggle"
			      :type "button"
			      :data-toggle "dropdown"
			      :aria-haspopup "true" :aria-expanded "false")
		     
		     (:div :class "dropdown-menu"
			   (dolist (option (eval (digx script :script :code)))
			     (cl-who:htm
			      (:span :id (id-string (getx option :name))
				     :class "dropdown-item"
				     :onclick
				     (grid-js-render-form-values
				      data-type
				      data-type
				      :action "select-action"
				      :action-data (frmt "~A"
							   (item-hash option))
				      :action-script
				      (id-string
				       (digx script :script :name)))
				     (:input :id "select-action-valuex"
					     :id "select-action-valuex"
					     :type "hidden"
					     :value (frmt "~A"
							  (item-hash option)))
				     (cl-who:str (getx option :name)))))))))))))


(defvar *rendering-shit* nil)

(defun keysx (fields)
  (let ((keys))   
    (dolist (field fields)
      (when (getf field :key-p)
	(setf keys
	      (append keys
		      (list
		       (list
			:name (getf field :name)
			:accessor (if (find (complex-type field)
					    (list :collection
						  :collection-items :list-items
						  :hierarchical))
				      (dig field :db-type :accessor))))))))
    keys))

(defun sort-by-keys (items keys)
  (flet ((sort-val (item)
	   (let ((values))
	     (dolist (key keys)
	       (let* ((accessor (getf key :accessor))
		      (val (if accessor
			       (if (getx item (getf key :name))
				   (apply #'digx
					  (getx item (getf key :name))
					  (if (listp accessor)
					      accessor
					      (list accessor))))
			       (getx item (getf key :name)))))
		 (setf values (frmt "~A~A" values val))))	    
	     values)))

    (sort items #'string-lessp :key #'sort-val)))


(defun grid-js-render-new (data-type)
  (let ((active-page (getcx data-type :active-page))
	(items))

    (dolist (hierarchy-item *item-hierarchy*)
      (setf items (pushnew 
		   (list (getf hierarchy-item :data-type)
			 (item-hash (getf hierarchy-item :item)))
		   items)))
    (setf items (reverse items))
    (setf items (pushnew (list data-type 0)
		      items))
    (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-new-~A" data-type)	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "action" "new")
	       
	       (js-pair "item-hierarchy"
			(hierarchy-string (reverse items)))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-grid-data (data-type page-items sub-level
			 parent-item parent-spec)

  
  (let ((sub-level-p (not (equalp data-type 
				  (gethash :data-type (cache *context*))))))
    
    (with-html-string
      (let ((fields (getcx data-type :fields))
	    (subs))
	
	(dolist (field fields)
	  (when (find (complex-type field) (list :collection-items :list-items
						 :hierarchical))
	    (pushnew field subs)))
	
	(setf page-items (sort-by-keys page-items (keysx fields)))

	(if parent-item
	    (setf *item-hierarchy* (push
				    (list :data-type parent-spec
					  :item parent-item)
				    *item-hierarchy*))
	    (setf *item-hierarchy* nil))

	(dolist (item page-items)
	  (cl-who:htm
	   (:div :class "row"
		 (when sub-level-p
		   (cl-who:htm
		    (:div :class "col-sm-1")))
		 (:div :class "col"
		       (:div 
			:class "row "		 
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
				    (unless *rendering-shit*
				      (cl-who:str			      
				       (render-grid-buttons data-type item )))
				    (cl-who:str
				     (render-select-button item)))))
		       (:div :class "row"
			     (:div :class "col"
				   (when *rendering-shit*
				     (cl-who:htm (:div (frmt "ajax-edit-~A" (item-hash item))
						      ))
				     )
				   
				   (unless *rendering-shit*
				     (when (equalp (item-hash (getcx data-type :edit-item))
							    (item-hash item))									       
				       (cl-who:htm
					
					(:div :id (frmt "ajax-edit-~A" (item-hash item))
					      
					      (when (and (and (equalp (parameter "action") "save")
							      (getcx data-type :edit-object)
							      (getcx data-type :validation-errors))
							 (string-equal (parameter "data-type")
								       (frmt "~A" data-type)))

						(cl-who:str (render-grid-edit data-type fields
									      (or (getcx data-type :edit-item)
										  item )
									      parent-item parent-spec))))))
					   
				     (when (equalp (ensure-parse-integer
						    (getcx data-type :expand-id)) 
						   (item-hash item))
					     
				       (unless sub-level-p
					 (setf (getcx data-type :root-item) item))
					     
				       (dolist (sub subs)
					 (let* ((sub-data-spec (dig sub :db-type :data-type)))

					   (setf (getcx sub-data-spec :parent-item) item)

					   (setf (getcx sub-data-spec :collection-name)
						 (dig sub :db-type :collection))
						 
											 
					   (unless (getcx sub-data-spec :data-type)
					     (setf (getcx sub-data-spec :data-type)
						   (find-type-def *system* 
								  sub-data-spec))
						   
					     (setf (getcx sub-data-spec :fields) 
						   (dig (getcx sub-data-spec :data-type)
							:data-type :fields)))
						 
					   (setf (getcx sub-data-spec :active-item) item)

						 
					   (cl-who:htm
					    (:div
					     :id sub-data-spec
					     :class "row"
					     (:div
					      :class "col"
					      (:div :class "card"
						    (:h5 :class "card-header"
							 (cl-who:str
							  (frmt "~A" (string-capitalize
								      (getf sub :name)))))
						    (cl-who:str
						     (render-grid-header
						      sub-data-spec
						      t))
							  
						    (:div :class "card-block"
							  (cl-who:str
							   (render-grid-data
							    sub-data-spec
							    (getfx item sub) 
							    (+ sub-level 1)
								  
							    item
							    data-type)))
						    (when (and (equalp (parameter "action")
								       "select-from")
							       (string-equal
								(parameter "data-type")
								(frmt "~A" sub-data-spec)))
						      (let ((*rendering-shit* t))

							      
							(cl-who:htm
							 (:div
							  :id (frmt "select-from-~A"
								    sub-data-spec)
							  :class "card-block"
							  (:h6 :class "card-title text-muted"
							       (cl-who:str
								(frmt "Select ~A to add..."
								      (dig sub :db-type
									   :collection))))
							  (cl-who:str
							   (render-grid-data
							    sub-data-spec
							    (grid-fetch-items
							     sub-data-spec
							     (dig sub :db-type :collection)
							     :test (filter-function data-type))
							    -1 parent-item data-type))

							  (:div
							   :class "card-footer"
							   (:button
							    :name "select" :type "submit" 
							    :class
							    "btn btn-outline-success float-right"
							    :aria-pressed "false"
							    :onclick
							    (grid-js-render-form-values
							     sub-data-spec
							     (frmt "select-from-~A"
								   sub-data-spec)
							     :action "add-selection")
							    (cl-who:str "Add Selection")))))))
						    (:div
						     :class "card-footer bg-white"
						     (:div :class "row"	  
							   (:div :class "col"
								 (:button
								  :name "new" :type "submit" 
								  :class "btn btn-outline-success"
								  :aria-pressed "false"
								  :onclick 
								  (grid-js-render-new
								   sub-data-spec)
								  (cl-who:str "+"))
								 (when (find (complex-type sub)
									     (list :collection-items
										   :hierarchical))

								   (cl-who:htm
								    (:button
								     :name "new" :type "submit" 
								     :class "btn btn-outline-success"
								     :aria-pressed "false"
								     :onclick 
								     (grid-js-render
								      sub-data-spec
								      :action "select-from" )
								     (cl-who:str "Select From")))))))))))))))))))))

	

	(cl-who:htm (:div :id (frmt "ajax-new-~A" data-type)
			  (when (not (item-hash (getcx data-type :edit-item)))
			    (when (and (and (equalp (parameter "action") "save")
					    (getcx data-type :edit-object)
					    (getcx data-type :validation-errors))
				       (string-equal (parameter "data-type")
						     (frmt "~A" data-type)))

			      (cl-who:str
			       (render-grid-edit data-type fields
						 (getcx data-type :edit-item)
						 parent-item parent-spec))))))))))

(defun render-grid-sizing (data-type)
  (with-html-string
    (:input :type "text" :name "pages" :class "float-right" 
	    :size 2
	    :id "pages"
	    :value (or  (parameter "pages")
			(getcx 
			 data-type :show-pages-count)
			50)
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "pages"
	     13
	     "cl-wfx:ajax-grid"
	     (gethash :collection-name (cache *context*))
	     (js-pair "data-type"
		      (frmt "~A" data-type))	     
	     (js-pair "action" "grid-sizing")))
   ;; (:span :class "float-right" "Rows : ")
    ))

(defun render-grid-search (data-type)
  (with-html-string
    (:input :type "text"
	    :class "w-100"
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
		50)))
  
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
			  (list :collection-items :list-items
				:hierarchical))
		(let ((accessor (dig field :db-type :accessor)))
		  (dolist (sub-val (getfx item field))
		    (when sub-val
		      (let* ((val (apply #'digx
					 item
					 (if (listp accessor)
					     accessor
					     (list accessor)))))
		
			(if (filter-found-p filter-term val)
			    (push t found)
			    (push nil found)))))))
	      
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
	   
	    (when (find (complex-type field)
			(list :collection-items :list-items
			      :hierarchical))
	      
	      (let ((accessor (dig field :db-type :accessor)))
		(dolist (sub-val (getfx item field))

		  (when sub-val
		    (let* ((val (apply #'digx
				       item
				       (if (listp accessor)
					   accessor
					   (list accessor)))))
		     
		      (when val
			(when (search search-term 
				      val
				      :test #'string-equal)
			  (unless found		       
			    
			    (setf found t)))))))))

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





(defun fetch-grid-data (data-type &key test)
  (let* ((items )
	 (collection-name (gethash :collection-name (cache *context*)))
	 (search-term (or (parameter "search") 
			  (getcx 
			   data-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (getcx data-type :filter)))
    
     (unless (or search-p filter-p)
       (setf items (grid-fetch-items data-type collection-name
				     :test test)))

     
     (when (or search-p filter-p (getcx data-type :filter-fields))
       
       (when (getcx 
	      data-type 
	      :filter-fields)
	  
	  (setf items
		(grid-fetch-items data-type collection-name
				  :test (filter-function data-type)))
	  (when items
	    (setf items
		  (find-items-in-item-list
		   items
		   (search-function data-type search-term)))))
       (unless (getcx 
		data-type 
		:filter-fields)
	 
	   (setf items
		 (grid-fetch-items data-type collection-name
				   :test (search-function
					  data-type search-term)))))
    
     (fetch-grid-page-data data-type (if (listp items)
					 items
					 (list items)))))

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
				      (or (parameter "pages") 50))
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
					   (or (parameter "pages") 50))
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
					 (or (parameter "pages") 50))
				(js-pair "page"
					 (if (< active-page real-page-count)
					     (+ active-page 1)))
				(js-pair "action" "page-next"))
		     "Next")))))))))


(defmethod action-handler ((action (eql :add-selection)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (let ((persist-p))
    (dolist (param (hunchentoot:post-parameters*))
      
      (when (equalp (first param) "grid-selection")
	
	(let ((spliff (split-sequence:split-sequence #\, (cdr param))))
	  
	  (when (equalp (first spliff) "true")
	    (setf persist-p t)
	    (pushnew
	     (grid-fetch-item (parameter "data-type")
			      (getcx (parameter "data-type")
				     :collection-name)
			      :test (lambda (item)
				      (equalp
					       (item-hash item)
					       (ensure-parse-integer
						(second spliff)))))
	     (getx (getcx (parameter "data-type") :active-item)
		   (getcx (parameter "data-type") :expand-field-name)))))))
    (when persist-p
      (persist-item
       (get-collection
	(first (collection-stores *system*
				  (gethash :collection-name (cache *context*))))
	(gethash :collection-name (cache *context*)))
       (getcx (gethash :data-type (cache *context*)) :root-item)))))

(defgeneric select-handler (action selected))

(defmethod action-handler ((action (eql :select-action)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  (let ((selected))
    (dolist (param (hunchentoot:post-parameters*))
      (when (equalp (car param) "grid-selection")
	(let ((spliff (split-sequence:split-sequence #\, (cdr param))))
	  (when (equalp (first spliff) "true")
	    (pushnew
	     (grid-fetch-item (parameter "data-type")
			      (gethash :collection-name (cache *context*))
			      :test (lambda (item)
				      
				      (equalp
				       (item-hash item)
				       (ensure-parse-integer
					(second spliff)))))
	     selected)))))

    (when (parameter "action-script")
      (select-handler
       (intern (string-upcase (parameter "action-script")) :KEYWORD)
       selected))))

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


(defun set-grid-search (data-type)
  (when (or (equalp (parameter "action") "filter")
	    (equalp (parameter "action") "un-filter"))
    
    (when (equalp (parameter "action") "filter")
      (if (empty-p (parameter "search"))
	  (setf (getcx data-type :search)
		(parameter "search"))))
    
    (when (equalp (parameter "action") "un-filter")
      (setf (getcx data-type :search) nil)))

  (unless (or (equalp (parameter "action") "filter")
	      (equalp (parameter "action") "un-filter"))
    (if (and (parameter "search") (not (empty-p (parameter "search"))))
	(setf (getcx data-type :search) (parameter "search"))
	(if (string-equal (parameter "search") "")
	    (setf (getcx data-type :search) nil)))
    )
  )

(defun set-grid-filter (data-type)
  (when (equalp (parameter "action") "filter")
    (setf (getcx data-type :filter) t))
  
  (when (equalp (parameter "action") "un-filter")
    (setf (getcx data-type :filter) nil))

  (when (equalp (parameter "action") "grid-col-filter")
    (let ((fields (getcx data-type :filter-fields)))

      (dolist (field (getcx data-type :fields))	
	(when (parameter (frmt "~A-filter" (getf field :name)))
	  
	  (pushnew field fields)
	  
	  (setf (getcx data-type
		       (intern (string-upcase
				(frmt "~A-filter" (getf field :name)))))  
		(parameter (frmt "~A-filter" (getf field :name))))))
    
      (setf (getcx data-type :filter-fields) fields))))


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

    ))

(defun set-grid-expand ()
  
  (when (equalp (parameter "action") "expand")
    (setf (getcx (parameter "data-type") :expand-id) (parameter "item-id")))
  
  (when (equalp (parameter "action") "unexpand")    
    (setf (getcx (parameter "data-type") :expand-id) nil)))


(defun render-grid (collection-name)
  (when (context-access-p (context-spec *context*))
      (let* ((collection (if (not (gethash :collection-name (cache *context*)))
			     (find-collection-def *system*   
						  collection-name)))
	     (data-type (or (gethash :data-type (cache *context*))
			    (and collection
				 (dig collection :collection :data-type)))))   

	(set-grid-context collection-name data-type)

	(set-type-context data-type)

	(set-type-context (parameter "data-type"))

	(set-grid-search data-type)
	
	(set-grid-filter data-type)
	
	(set-grid-expand)
	
	(let ((page-items (fetch-grid-data data-type)))
	  
	  ;;(campaign-shit page-items)
	  
	  (with-html-string  
	    (:div :id (gethash :collection-name (cache *context*))
		  :class "card"
		  (:h4 :class "card-header"
		       (cl-who:str (string-capitalize collection-name))
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
		  (cl-who:str
			 (render-grid-header data-type nil))
		  (:div :class "card-block"
			:id data-type
			(cl-who:str
			 (render-grid-data data-type page-items 0 nil nil)))

		  (:div :class "card-footer"
			(:button 
				     :name "expand" :type "submit" 
				     :class "btn btn-outline-success"
				     :aria-pressed "false"
				     :onclick 
				     (grid-js-render-new data-type)
				     (cl-who:str "+"))
			(cl-who:str
			 (render-select-actions
			  data-type)))
		  
		  (:div :class "card-footer"
			(:div :class "row"	  
			      (:div :class "col"
				  
				    (cl-who:str
				     (render-grid-paging data-type)))))))))))

(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
;;  (break "~A" (hunchentoot:post-parameters*))

  (render-grid (getx (context-spec *context*) :name)))


(defun get-child (fields parent-item data-type hash)
  (dolist (field fields)
    (when (find (complex-type field)
		    (list :collection-items
			  :list-items
			  :hierarchical))
      (when (string-equal data-type (dig field :db-type :data-type))
	(dolist (item (getx parent-item (getf field :name)))
	    (when (equalp (item-hash item) (ensure-parse-integer hash))
	      (return-from get-child (list (getf field :name) item))))
	(return-from get-child (list (getf field :name)
				     (make-item :data-type data-type)))
	))))


(defun fetch-grid-root-edit-item (data-type hash)
  (let ((collection-name (gethash :collection-name (cache *context*))))
     (grid-fetch-item data-type collection-name
		      :test (lambda (item)
			      (when (equalp (item-hash item)
					    hash)
				item)))))

(defun ajax-auto-complete (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* ((data-type (parameter "data-type"))
	 (field-name (intern (parameter "field-name") :KEYWORD))
	 (fields (getcx data-type :fields))
	 (field))
    (dolist (fieldx fields)
      (when (string-equal (getf fieldx :name) field-name)
	(setf field fieldx)))
    
    (when field
      (let* ((accessors (dig field :db-type :accessor))
	     (list (grid-fetch-items
		    (dig field :db-type :data-type)
		    (dig field :db-type :collection)
		    :test (lambda (item)
			    (or
			     (string-equal (parameter
					     (frmt "~A-drop" field-name))
					    "")
			     (search (parameter
				      (frmt "~A-drop" field-name))
				     (apply #'digx
					    item
					    (if (listp accessors)
						accessors
						(list accessors)))
				     :test #'string-equal))))))
	(with-html-string
	  (:div
		;;:style "z-index:1070;position:absolute;top:30px;over-flow:scroll;"
		:class "auto-complete-menu nav flex-column bg-white  rounded border"
		;; :aria-labelledby (frmt "~A-drop" name)
		;;	(break "~A" list)

		(setf list (sort list #'string<
				 :key (lambda (item)
					(apply #'digx item
					       (if (listp accessors)
						   accessors
						   (list accessors))))))
		(dolist (option list)
		  (cl-who:htm
		   (:span :class "auto-complete-item nav-link" 			      
			  (:input :type "hidden"
				  :value (frmt "~A" (item-hash option)))
			  (cl-who:str
			   (trim-whitespace 
			    (apply #'digx option
				   (if (listp accessors)
				       accessors
				       (list accessors))))))))))))))

(defun ajax-grid-edit (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* ((data-type (parameter "data-type"))
	 (fields )
	 (hierarchy (cl-wfx:read-no-eval (parameter "item-hierarchy")))
	 (root-type)
	 (root-hash)
	 (root-item)
	 (edit-objects))
    (setf (getcx data-type :edit-object) nil)
    
     (setf root-type (if  hierarchy
			  (string-downcase (frmt "~A"
						  (first (first hierarchy))))))
     (setf root-hash (if hierarchy
			 (second (first hierarchy))))

     (setf fields (getcx root-type :fields))
     
     (setf root-item (fetch-grid-root-edit-item root-type root-hash))

     
     (unless root-item
       (setf root-item (make-item :data-type data-type)))

     (when root-item
       (setf edit-objects (list (list :data-type root-type :item root-item)))
       
       (if (> (length hierarchy) 1)
	   (let ((item ;;root-item
		  )
		 (item-type root-type)
		 )

	  ;;  (break "shit ~A" hierarchy)
	     (dolist (child (cdr hierarchy))
	      ;; (break "poes ~A~% ~A" item-type (getcx item-type :fields))
	       (setf item (get-child (getcx item-type :fields)
				     (or (if item (second item)) root-item)
				     (first child)
				     (second child)))
	       (setf item-type (string-downcase (frmt "~A" (first child))))
	  ;;     (break "~A ~%~A" item item-type)
	       (setf edit-objects
		     (push (list :data-type item-type
				 :item (second item)
				 :field-name (first item))
			   edit-objects)))
	     (setf (getcx data-type :edit-object) edit-objects)
	    ;; (break "wtf ~A" edit-objects)
	     (render-grid-edit item-type
			       (getcx item-type :fields)
			       (second item)
			       root-item root-type))
	   (progn
	     (setf (getcx data-type :edit-object) edit-objects)
	     (render-grid-edit root-type fields root-item nil nil))))))



(defmethod action-handler ((action (eql :save)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (let* ((data-type (parameter "data-type"))
	 (fields (getcx data-type :fields))	 
	 (root-item)
	 (parent-item)
	 (edit-item)
	 (parent-slot)
	 (edit-objects (reverse (getcx data-type :edit-object))))


    (when edit-objects      
      (setf root-item (getf (first edit-objects) :item))
      (when (> (length edit-objects) 1)
	(setf edit-item (getf (first (last edit-objects)) :item))
	(setf parent-slot (getf (first (last edit-objects)) :field-name))
	(setf parent-item
	      (getf (nth (- (length edit-objects) 2) edit-objects) :item)))
      
      (unless (> (length edit-objects) 1)
	(setf edit-item root-item)))
    
    (setf (getcx data-type :validation-errors) nil)
    (setf (getcx data-type :validation-errors-id) nil)

 
    (when fields

      (unless edit-item
	(setf edit-item (make-item :data-type data-type)))

      (dolist (field fields)
	  
	  (when (and (digx field :attributes :editable)
		     (getf field :db-type)
		     (not (find (complex-type field)
				(list :collection-items
				      :list-items
				      :hierarchical))))
	   
	    (let* ((field-name (frmt "~A" (getf field :name)))
		  (valid (if (equalp (complex-type field) :item)
			     (validate-sfx (complex-type field)
					   field 
					   edit-item 
					   (parameter field-name))
			     (list t nil))))
	      
	      (unless (first valid)
		(pushnew 
		 (list field-name (second valid))
		 (getcx data-type :validation-errors)))
	      
	      (when (first valid)
		(setf (getfx edit-item field :parent-item parent-item)
		      (parameter field-name)))))
	  
	  (when (getf field :key-p)
	   
	    (when (empty-p (parameter (getf field :name)))
	      (pushnew
	       "Key values may not be blank."
	       (getcx data-type :validation-errors))
	      )
	    ))


      	;;TODO: is this still needed????
	;;Doing this to keep edit window open.
	(when (getcx data-type :validation-errors)
	  (setf (getcx data-type :validation-errors-id)
		(item-hash edit-item)))

	
      (unless (getcx data-type :validation-errors)
	  ;;Append parent-slot only if new
	  (when parent-slot
	    (let ((exists (exist-child-item edit-item
					    (getx parent-item parent-slot))))
	     
	      (if exists
		  (setf exists edit-item)		  
		  (setf (getx parent-item parent-slot)
			(append (getx parent-item parent-slot)
				(list edit-item))))))

	  (when (getcx data-type :collection-name)
	    (setf (item-collection edit-item)
		  (get-collection
		   (first (collection-stores
			   *system*
			   (getcx data-type :collection-name)))
		   (getcx data-type :collection-name))))
	  
	  (let ((collection (get-perist-collection
			     (gethash :collection-name (cache *context*)))))
	    
	    (unless collection
	      (pushnew 
	       "No default store found check if license is selected."
	       (getcx data-type :validation-errors))
	    
	      (setf (getcx data-type :validation-errors-id)
		    (item-hash edit-item)))

	    (when collection
	      (persist-item collection root-item)))))))

(defun exist-child-item (item children)
  (let ((exists nil))
    (dolist (child children)
      (when (equalp (item-values child) (item-values item))
	
	(setf exists child)))
    exists))

(defun store-from-stash (store-name)
  (dolist (store (getcx (gethash :data-type (cache *context*)) :stores))
    (when (equalp store-name (name store))
      
      (return-from store-from-stash store))))


(defun get-delete-object ()
  (let* ((data-type (parameter "data-type"))
	 (fields )
	 (hierarchy (cl-wfx:read-no-eval (parameter "item-hierarchy")))
	 (root-type)
	 (root-hash)
	 (root-item)
	 (edit-objects))

    
     (setf root-type (if  hierarchy
			  (string-downcase (frmt "~A"
						  (first (first hierarchy))))))
     (setf root-hash (if hierarchy
			 (second (first hierarchy))))

     
     (setf fields (getcx root-type :fields))

     (setf root-item (fetch-grid-root-edit-item root-type root-hash))

     (unless root-item
       (setf root-item (make-item :data-type data-type)))

     
     (when root-item
       (setf edit-objects (list (list :data-type root-type :item root-item)))
       
       (if (> (length hierarchy) 1)
	   (let ((item root-item)
		 (item-type root-type))

	     (dolist (child (cdr hierarchy))
	       
	       (setf item (get-child (getcx item-type :fields) root-item
				     (first child)
				     (second child)))
	       (setf item-type (string-downcase (frmt "~A" (first child))))
	       (setf edit-objects
		     (push (list :data-type item-type
				 :item (second item)
				 :field-name (first item))
			   edit-objects)))
	    
	     edit-objects)
	   edit-objects))))

(defmethod action-handler ((action (eql :delete)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (let* ((edit-objects (reverse (get-delete-object)))
	 (root-item)
	 (parent-item)
	 (edit-item)
	 (parent-slot))

 
    (when edit-objects      
      (setf root-item (getf (first edit-objects) :item))
      (when (> (length edit-objects) 1)
	(setf edit-item (getf (first (last edit-objects)) :item))
	(setf parent-slot (getf (first (last edit-objects)) :field-name))
	(setf parent-item
	      (getf (nth (- (length edit-objects) 2) edit-objects) :item))))

   
 
    
    (when (and edit-item parent-slot)
      (when parent-slot
	(let ((clean-list (getx parent-item parent-slot)))
	  (dolist (item clean-list)
	    (when (equalp (item-hash item)
			  (ensure-parse-integer (parameter "item-id")))
	    
	      (setf clean-list
		    (remove item clean-list))))
	  (setf (getx parent-item parent-slot) clean-list)


	  (persist-item (item-collection root-item) root-item))))

    (unless (and edit-item parent-slot)

      (setf (cl-naive-store::item-deleted-p root-item) t)
      (persist-item (item-collection root-item) root-item)
      (cl-naive-store::remove-item root-item))))
