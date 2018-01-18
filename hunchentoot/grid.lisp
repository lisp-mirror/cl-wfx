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

(defun html-value (value)
  (cond ((symbolp value)
	 (frmt "~S" value))
	((stringp value)
	 value)
	((numberp value)
	 value)
	(t
	 (frmt "~S" value))))


(defun render-dropdown (name selected list
			&key  (select-prompt "Select a value")
			  key-func value-func
			  select-onclick-func)
  (let ((selected-value (if selected
			    (if key-func
				(funcall key-func selected)
				selected)
			    (or (parameter (frmt "~A" name))
				select-prompt))))
    ;;(break "~A" (hunchentoot::post-parameters*))
    (with-html-string
      (:div :class "bt-group dropdown"
	     (:input :type "hidden" :class "selected-value" 
		     :name (frmt "~A" name)
		     :value (html-value (or (parameter (frmt "~A" name)) "")))
	     (:button :class "btn btn-secondary dropdown-toggle"
		      :type "button"
		      :data-toggle "dropdown"
		      :aria-haspopup "true" :aria-expanded "false"
		      (cl-who:str (html-value (or selected-value ""))))
	     
	     (:div :class "dropdown-menu" :aria-labelledby (frmt "~A" name)
		   (dolist (option list)
		     (cl-who:htm
		      (:span :class "dropdown-item"
			     :onclick (if select-onclick-func
					  (funcall
					   select-onclick-func option))
			     
			     (:input :id "select-action"
				     :type "hidden"
				     :value (html-value
					     (if key-func
						 (funcall key-func option)
						 option)))
			     (cl-who:str
			      (html-value
			       (if value-func
				   (funcall value-func option)
				   option)))))))))))

(defmethod render-input-val ((type (eql :value-list)) field item
			     &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (dig field :db-type :values))
	 (selected (find (getfx item field) list :test #'equalp)))

    (with-html-string
      (cl-who:str (render-dropdown name selected list)))))



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


(defun render-item-list-auto-complete-x (collection data-type field-name selected
			&key  select-prompt
			  value-func
			  context-state-selected)
  (let ((selected-value (if selected
			    (if value-func
				(funcall value-func selected)
				selected)
			    (or (parameter (frmt "~A-drop" field-name))
				context-state-selected
				select-prompt))))
    (with-html-string
      (:div :class "auto-complete"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" field-name)
		    :value (html-value (or (parameter (frmt "~A" field-name)) "")))
	    
	    (:input :class "form-control auto-complete-text"
		    :type "text"
		    :placeholder (or select-prompt "Press enter for list or start typing and then press enter for list...")
		    :name (frmt "~A-drop" field-name) 
		    :id (frmt "~A-drop" field-name)
		    :value (html-value (or selected-value ""))
		    :onkeydown
		    ;;fires ajax call on enter (13)
		    (js-render-event-key 
		     (frmt "~A-drop" field-name)
		     13
		     "cl-wfx:ajax-auto-complete-x"
		     (frmt "~A-drop-div" field-name)
		     (js-pair "data-type"
			      data-type)
		     (js-pair "field-name"
			      (frmt "~A" field-name))
		     (js-pair "collection"
			      (frmt "~A" collection))
		     (js-pair "action" "auto-complete")))
	    
	    (:div :id (frmt "~A-drop-div" field-name) :class "auto-list")))))


(defun render-item-list-auto-complete (data-type field-name selected
			&key  select-prompt
			  value-func
			  context-state-selected)
  (let ((selected-value (if selected
			    (if value-func
				(funcall value-func selected)
				selected)
			    (or (parameter (frmt "~A-drop" field-name))
				context-state-selected
				select-prompt))))
    (with-html-string
      (:div :class "auto-complete"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" field-name)
		    :value (html-value (or selected-value "")))
	    
	    (:input :class "form-control auto-complete-text"
		    :type "text"
		    :placeholder (or select-prompt "Press enter for list or start typing and then press enter for list...")
		    :name (frmt "~A-drop" field-name) 
		    :id (frmt "~A-drop" field-name)
		    :value (html-value (or selected-value ""))
		    :onkeydown
		    ;;fires ajax call on enter (13)
		    (js-render-event-key 
		     (frmt "~A-drop" field-name)
		     13
		     "cl-wfx:ajax-auto-complete"
		     (frmt "~A-drop-div" field-name)
		     (js-pair "data-type"
			      data-type)
		     (js-pair "field-name"
			      (frmt "~A" field-name))
		     (js-pair "action" "grid-auto-complete")))
	    
	    (:div :id (frmt "~A-drop-div" field-name) :class "auto-list")))))

(defmethod render-input-val ((type (eql :collection)) field item 
			     &key data-type &allow-other-keys)

  (let* ((name (getf field :name))
	 (list (wfx-fetch-context-items (dig field :db-type :collection)))
	 (selected (find (getx item name)  
			 list :test #'equalp))
	 (accessors (dig field :db-type :accessor)))
   
    (with-html-string
      (cl-who:str (render-item-list-auto-complete
		   data-type name selected
		   :value-func (lambda (item)
				 (apply #'digx
					item
					(if (listp accessors)
					    accessors
					    (list accessors))))
		   :context-state-selected (getcx 
					    (dig field :db-type :data-type)
					    (frmt "~A-drop" name)))))))

(defmethod render-input-val ((type (eql :contained-item)) field item 
			     &key parent-item &allow-other-keys)
  (declare (ignore parent-item))
  (let* ((name (getf field :name))
	 (list (apply #'digx
		      (getf
		       (second (getcx (parameter "data-type") :edit-object))
		       :item)
		      (dig field :db-type :container-accessor)))
	 (selected (find (getx item name)  
			 list :test #'equalp))
	 (accessors (dig field :db-type :accessor)))

   
    (with-html-string

      (cl-who:str
       (render-dropdown name selected list
			:key-func 'cl-naive-store:item-hash
			:value-func (lambda (item)
				      (apply #'digx
					     item
					     (if (listp accessors)
						 accessors
						 (list accessors)))))))))

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

(defun grid-js-render-file-upload (data-type form-id row-id field-name)
  (js-render-form-values 
     "cl-wfx:ajax-render-file-upload"
     row-id
     form-id
     (js-pair "data-type"
	      (frmt "~A" data-type))
     (js-pair "field-name" field-name)
     (js-pair "action" "upload-file")))

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
	     :class "grid-selection"
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
;;      (break "~A" permissions)
      (dolist (permission permissions)
	(cond ((or (equalp permission :view) (equalp permission :update))
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
	
		 (cl-who:str (if (user-context-permission-p
				  (getx (context-spec *context*) :name)
				  :update)
				 "Edit"
				 "View")))))
	      ((and (equalp permission :delete)
		    (user-context-permission-p
		     (getx (context-spec *context*) :name)
		     :delete))
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
		    
		    (cl-who:htm
		     (:div :class "row"
			   (:div :clas "col"
				 (cl-who:str
				  (frmt "Errors ~S"
					errors)))))))
	
		(:div :class "row"
		      (:div :class "col"
			    (when (user-context-permission-p
				   (getx (context-spec *context*) :name)
				   :update)
			      (cl-who:htm
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
			       ))
			    
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
		       (getcx data-type
		       (intern (string-upcase
				(frmt "~A-filter" col-name))))
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
		     (:div :class "float-right"
		      (:input
		       :class "form-check-input "
		       :type "checkbox"
		       :id "grid-select-all"
		       :name "grid-select-all"
		       :onclick "gridSelectAll();"
		       :value "All"
		       :checked (parameter "grid-select-all")
		       
		       ))
		     (:div ;;:class "float-right"
			   (cl-who:str
			    (render-grid-sizing data-type)))))))))

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
  (let ((action-list))

    (dolist (script (getx (context-spec *context*) :scripts))
      (when (string-equal (digx script :script :name) "Select Action List")
	(setf action-list (eval (digx script :script :code)))))

    (when (user-context-permission-p
	   (getx (context-spec *context*) :name)
	   :delete)
      (setf action-list (append action-list
				(list (list :action-name "Delete Selected"
					    :handler-script "Delete Selected")))))
    (when action-list 
      (with-html-string
	;;TODO: float right fucks up dropdown menu positioning
	  (:div ;;:class " float-right"
		:id "select-stuff"
		:name "select-stuff"
		(cl-who:str
		 (render-dropdown
		  "grid-select-action"
		  nil
		  action-list			       
		  :key-func (lambda (action)
				(getf action :action-name))
		  :value-func (lambda (action)
				(getf action :action-name))
		  :select-onclick-func
		  (lambda (action)
		    (js-render-form-values
		     "cl-wfx:ajax-grid"
		     (gethash :collection-name
			      (cache *context*))
		     (frmt "~A" data-type)
		     (js-pair "action"
			      "grid-select-action")
		     
		     (js-pair "action-data"
			      (or (getf action :data) ""))
		     (js-pair "action-handler-script"
			      (getf action :handler-script))
		     (js-pair "pages"
			      (or (parameter "pages") 50))
		     (js-pair "page"
			      (or (getcx 
				   data-type :active-page)
				  1)))))))))))


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
    (sort (copy-list items) #'string-lessp :key #'sort-val)))


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

(defun render-sub-new-button (sub data-spec)
  (when (user-context-permission-p
	 (getx (context-spec *context*) :name)
	 :update)
    (with-html-string
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
		     data-spec)
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
			data-spec
			:action "select-from" )
		       (cl-who:str "Select From"))))))))))

(defun render-item-row (subs data-type item fields)
  (with-html-string
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
			  (if (not (equalp (complex-type field)
					   :image))
			      (if (> (length val) 100)
				  (cl-who:str 
				   (subseq val 0 100))
				  (cl-who:str 
				   val))
			      (cl-who:str 
			       val))))))))))

     (:div :class "col-sm-2"
	   (:div :class "btn-group float-right"
		 (unless *rendering-shit*
		   (cl-who:str			      
		    (render-grid-buttons data-type item )))
		 (cl-who:str
		  (render-select-button item)))))))

(defun render-select-from-grid (data-type sub sub-data-spec parent-item)
  (when (and (equalp (parameter "action")
		     "select-from")
	     (string-equal
	      (parameter "data-type")
	      (frmt "~A" sub-data-spec)))
				   
    (let ((*rendering-shit* t))

     
      (with-html-string
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
	 (render-grid-data sub-data-spec
			   (wfx-fetch-context-items
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
	  (js-render-form-values 
	   "cl-wfx:ajax-grid"
	   (gethash :collection-name (cache *context*))
	   (frmt "select-from-~A"
		 sub-data-spec)
	   (js-pair "data-type"
		    (frmt "~A" sub-data-spec))
	   
	   (js-pair "action" "add-selection")
	   (js-pair "add-selection-field" (frmt "~A" (dig sub :name)))
	   
	   (js-pair "pages"
		    (or (parameter "pages") 50))
	   (js-pair "page"
		    (or (getcx data-type :active-page) 1)))
	  
	 
	  (cl-who:str "Add Selection"))))))))

(defun render-expand (data-type item subs sub-level sub-level-p parent-item)
  (when (equalp (ensure-parse-integer
		 (getcx data-type :expand-id)) 
		(item-hash item))
		    
    (unless sub-level-p
      (setf (getcx data-type :root-item) item))

    (with-html-string
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
		   
		   (cl-who:str
		    (render-select-from-grid
		     data-type sub
		     sub-data-spec parent-item))
		   
		   (cl-who:str
		    (render-sub-new-button
		     sub
		     sub-data-spec)))))))))))

(defun render-data-edit (data-type item fields parent-item parent-spec)
  (with-html-string
    (if (getcx data-type :edit-item)
	
	(if (equalp (item-hash (getcx data-type :edit-item))
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
						 parent-item parent-spec)))))
	  (cl-who:htm (:div :id (frmt "ajax-edit-~A" (item-hash item)))))
	(cl-who:htm (:div :id (frmt "ajax-edit-~A" (item-hash item)))))))

(defun render-row-goodies (subs sub-level-p sub-level data-type
			   item fields parent-item parent-spec)
  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(when *rendering-shit*

		  (cl-who:htm
		   (:div :id (frmt "ajax-edit-~A"
				   (item-hash item)))))
		
		(unless *rendering-shit*
		  
		  (cl-who:str (render-data-edit data-type item fields
						parent-item parent-spec))

		  (cl-who:str (render-expand data-type item subs
					     sub-level sub-level-p
					     parent-item)))
		))))


(defun render-new-edit (data-type fields parent-item parent-spec)
  (with-html-string
    (:div :id (frmt "ajax-new-~A" data-type)
	  (when (and (getcx data-type :edit-item)
		     (not (item-hash (getcx data-type :edit-item))))
	    (when (and (and (equalp (parameter "action") "save")
			      (getcx data-type :edit-object)
			      (getcx data-type :validation-errors))
			 (string-equal (parameter "data-type")
				       (frmt "~A" data-type)))

		(cl-who:str
		 (render-grid-edit data-type fields
				   (getcx data-type :edit-item)
				   parent-item parent-spec)))))))

(defun render-grid-data (data-type page-items sub-level
			 parent-item parent-spec)

  (let ((sub-level-p (not (equalp data-type 
				  (gethash :data-type (cache *context*)))))
	(data-items))
    
    (with-html-string
      (let ((fields (getcx data-type :fields))
	    (subs))
	
	(dolist (field fields)
	  (when (find (complex-type field)
		      (list :collection-items :list-items
			    :hierarchical))
	    (pushnew field subs)))

	(setf data-items (sort-by-keys page-items (keysx fields)))

	(if parent-item
	    (setf *item-hierarchy* (push
				    (list :data-type parent-spec
					  :item parent-item)
				    *item-hierarchy*))
	    (setf *item-hierarchy* nil))

	(dolist (item data-items)

	  (cl-who:htm
	   (:div :class "row"
		 (when sub-level-p
		   (cl-who:htm
		    (:div :class "col-sm-1")))
		 
		 (:div :class "col"
		       (cl-who:str
			(render-item-row subs data-type item fields))
		       		       
		       (cl-who:str
			(render-row-goodies subs sub-level-p
					    sub-level data-type
					    item fields
					    parent-item parent-spec))))))
	
	(cl-who:str (render-new-edit data-type fields
				     parent-item parent-spec))))))

(defun render-grid-sizing (data-type)
  (with-html-string
    (:input :type "text" :name "pages" ;;:class "float-right" 
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
		(getcx data-type
		       (intern (string-upcase
				(frmt "~A-filter" (getf field :name))))))))
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
       (setf items (wfx-fetch-context-items collection-name
				     :test test)))

     
     (when (or search-p filter-p (getcx data-type :filter-fields))
       
       (when (getcx 
	      data-type 
	      :filter-fields)
	  
	  (setf items
		(wfx-fetch-context-items collection-name
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
		 (wfx-fetch-context-items collection-name
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
	     (wfx-fetch-context-item 
	      (getcx (parameter "data-type")
		     :collection-name)
	      :test (lambda (item)
		      (equalp
		       (item-hash item)
		       (ensure-parse-integer
			(second spliff)))))
	     (getx (getcx (parameter "data-type") :active-item)
		   (intern (string-upcase (parameter "add-selection-field"))
			   :keyword)))))))
    (when persist-p
      (persist-item
       (get-collection	
	(collection-store (gethash :collection-name (cache *context*)))
	(gethash :collection-name (cache *context*)))
       (getcx (gethash :data-type (cache *context*)) :root-item)))))

(defun delete-selected (selected)
  (dolist (item selected)
    (setf (cl-naive-store::item-deleted-p item) t)
    (persist-item (item-collection item) item)
    (cl-naive-store::remove-item item)))

(defmethod action-handler ((action (eql :grid-select-action)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  (let ((selected))
    (dolist (param (hunchentoot:post-parameters*))
      (when (equalp (car param) "grid-selection")
	(let ((spliff (split-sequence:split-sequence #\, (cdr param))))
	  (when (equalp (first spliff) "true")
	    (pushnew
	     (wfx-fetch-context-item 
	      (gethash :collection-name (cache *context*))
	      :test (lambda (item)				      
		      (equalp
		       (item-hash item)
		       (ensure-parse-integer
			(second spliff)))))
	     selected)))))
    
    (setf (getcx (parameter "data-type") :selected-grid-items) selected)

    
    (cond ((string-equal (parameter "action-handler-script")
			 "Delete Selected")
	   (delete-selected selected))
	  (t
	   (dolist (script (getx (context-spec *context*) :scripts))
	     
	     (when (string-equal (digx script :script :name)
				 "Select Action Handler")
	       (eval (digx script :script :code))))))
    (setf (getcx (parameter "data-type") :selected-grid-items) nil)))

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
	    (setf (getcx data-type :search) nil)))))

(defun set-grid-filter (data-type)
  (when (equalp (parameter "action") "filter")
    (setf (getcx data-type :filter) t))
  
  (when (equalp (parameter "action") "un-filter")
    (setf (getcx data-type :filter) nil))

  (let ((fields (getcx data-type :filter-fields)))
    (dolist (field (getcx data-type :fields))
	(when (parameter (frmt "~A-filter" (getf field :name)))
	  (pushnew field fields)
	  (setf (getcx data-type
		       (intern (string-upcase
				(frmt "~A-filter" (getf field :name)))))  
		(parameter (frmt "~A-filter" (getf field :name))))))
    
      (setf (getcx data-type :filter-fields) fields)))

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
    
    (unless (equalp (parameter "action") "save")
      (setf (getcx data-type :root-item) nil))))

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
		      (:div :class "row"
			    (:div :class "col"
				  (:button 
				   :name "expand" :type "submit" 
				   :class "btn btn-outline-success"
				   :aria-pressed "false"
				   :onclick 
				   (grid-js-render-new data-type)
				   (cl-who:str "+")))
			    (:div :class "col-2"
				  (cl-who:str
				   (render-select-actions
				    data-type)))))
		  
		(:div :class "card-footer"
		      (:div :class "row"	  
			    (:div :class "col"
				  (cl-who:str
				   (render-grid-paging data-type)))))))))))

(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
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
				     (make-item :data-type data-type)))))))

(defun fetch-grid-root-edit-item (hash)
  (let ((collection-name (gethash :collection-name (cache *context*))))   
     (wfx-fetch-context-item  collection-name
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
	     (list (wfx-fetch-context-items		   
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
	   :class "auto-complete-menu nav flex-column bg-white rounded border"
	   (setf list (sort (copy-list list) #'string<
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

(defun ajax-auto-complete-x (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* (
	 (field-name (intern (parameter "field-name") :KEYWORD))
	 (collection (parameter "collection")))
 
    (let* ((accessors (list  field-name))
	   (list (wfx-fetch-context-items		   
		    collection
		    :test (lambda (item)
			    (or
			     (string-equal (parameter
					     (frmt "~A-drop" field-name))
					    "")
			     (search (parameter
				      (frmt "~A-drop" field-name))
				     (apply #'digx
					    item
					    accessors)
				     :test #'string-equal))))))

	(with-html-string
	  (:div
	   :class "auto-complete-menu nav flex-column bg-white rounded border"
	   (setf list (sort (copy-list list) #'string<
			    :key (lambda (item)
				   (apply #'digx item
					  accessors))))
	   (dolist (option list)
	     (cl-who:htm
	      (:span :class "auto-complete-item nav-link"
		     (:input :type "hidden"
			     :value (frmt "~A" (item-hash option)))
		     (cl-who:str
		      (trim-whitespace 
		       (apply #'digx option
			      accessors)))))))))))

(declaim (optimize (debug 3))
	 (notinline ajax-grid-edit))

(defun set-edit-obects ()
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
     
    (setf root-item (fetch-grid-root-edit-item root-hash))
     
    (unless root-item
      (setf root-item (make-item :data-type data-type)))

    (when root-item
      (setf edit-objects (list (list :data-type root-type :item root-item)))
       
      (if (> (length hierarchy) 1)
	  (let ((item)
		(item-type root-type))

	    (dolist (child (cdr hierarchy))
	      (setf item (get-child (getcx item-type :fields)
				    (or (if item (second item)) root-item)
				    (first child)
				    (second child)))
	      (setf item-type (string-downcase (frmt "~A" (first child))))
	      (setf edit-objects
		    (push (list :data-type item-type
				:item (second item)
				:field-name (first item))
			  edit-objects)))
	    (setf (getcx data-type :edit-object) edit-objects))
	  (setf (getcx data-type :edit-object) edit-objects))
      edit-objects)))

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
     
    (setf root-item (fetch-grid-root-edit-item root-hash))

    (unless root-item
      (setf root-item (make-item :data-type data-type)))

    (when root-item
      (setf edit-objects (list (list :data-type root-type :item root-item)))
       
      (if (> (length hierarchy) 1)
	  (let ((item)
		(item-type root-type))

	    (dolist (child (cdr hierarchy))
	      (setf item (get-child (getcx item-type :fields)
				    (or (if item (second item)) root-item)
				    (first child)
				    (second child)))
	      (setf item-type (string-downcase (frmt "~A" (first child))))
	      (setf edit-objects
		    (push (list :data-type item-type
				:item (second item)
				:field-name (first item))
			  edit-objects)))
	    (setf (getcx data-type :edit-object) edit-objects)
	    (render-grid-edit item-type
			      (getcx item-type :fields)
			      (second item)
			      root-item root-type))
	  (progn
	    (setf (getcx data-type :edit-object) edit-objects)
	    (render-grid-edit root-type fields root-item nil nil))))))


(defun index-keys-x (fields item-values)
  (let ((keys))
    (dolist (field fields)
      (when (getf field :key-p)
	(if (equalp (type-of (getf item-values (getf field :name))) 'item)
	    (push (item-hash (getf item-values (getf field :name))) keys)
	    (push (getf item-values (getf field :name)) keys))))
    (reverse keys)))

(defun move-uploaded-file (fields edit-item)
  (let ((hash (item-hash edit-item))
	(collection (get-perist-collection
		     (gethash :collection-name (cache *context*)))))

    (unless hash
    
      (let* ((keys (index-keys-x fields
			      (item-values edit-item)))
	    (hashx (sxhash keys)))
	(setf hash hashx)))
    
    (dolist (field fields)

      (when (equalp (complex-type field) :image)
	
	(let ((server-path
	       (string-downcase
		(frmt "~A/files/~A/~A/"
		      (if (location collection)
			  (location collection)
			  (string-downcase
			   (frmt "~A~A"
				 (location (store collection))
				 (name collection))))
		      (parameter "data-type")
		      (getf field :name)
		     ;; hash
		      )))
	      (temp-path  (merge-pathnames				 
			   (replace-all
			    (parameter (string-downcase
					(frmt "~A" (getf field :name))))
			    "_"
			    "-")
			   (string-downcase
			    (frmt "~A/files/tmp/~A/~A/"
				  (if (location collection)
				      (location collection)
				      (string-downcase
				       (frmt "~A~A"
					     (location (store collection))
					     (name collection))))
				  
				  (parameter "data-type")
				  (getf field :name)))) ))

	  (ensure-directories-exist server-path)

	  (unless (probe-file temp-path)
	    (setf (getx edit-item (getf field :name))
		  (getx edit-item (getf field :name)))
	    )
	  
	  (when (probe-file temp-path)
	    (fad:copy-file
	     temp-path
	     (merge-pathnames (replace-all
			       (parameter (string-downcase
					   (frmt "~A" (getf field :name))))
			       "_"
			       "-")
			      server-path)
	     :overwrite t)
	    (delete-file temp-path)
	    (setf (getx edit-item (getf field :name))
		  (parameter (string-downcase
			      (frmt "~A" (getf field :name)))))))))))

(defun find-contained-item (hash list)
  (dolist (item list)
    (when (equalp (item-hash item) (ensure-parse-integer hash))
      (return-from find-contained-item item))))

(defun synq-value (field edit-item parent-item value)
  (cond ((equalp (complex-type field) :collection)
	 (setf (getfx edit-item field)
	       (wfx-fetch-context-item
		(dig field :db-type :collection)
		:test (lambda (item)
			(equalp (item-hash item)
				(ensure-parse-integer value))))))
	((equalp (complex-type field) :collection-contained-item)
	 (setf (getfx edit-item field)
	       (find-contained-item
		value
		(apply #'digx parent-item
		       (digx field :db-type :container-accessor)) )))
	
	((equalp (complex-type field) :contained-item)
	 (setf (getfx edit-item field)
	       (find-contained-item
		value
		(apply #'digx parent-item
		       (digx field :db-type :container-accessor)) )))
	(t
	 (setf (getfx edit-item field) value))))


(defun validate-value (field field-name edit-item parent-item value)
  (if (equalp (complex-type field) :item)
      (cond ((equalp (complex-type field) :collection)
	     (validate-sfx
	      (complex-type field)
	      field 
	      edit-item 
	      (parameter field-name)
	      :items (wfx-fetch-context-items
		      (dig field :db-type :collection)
		      :test (lambda (item)
			      (equalp (item-hash item)
				      (ensure-parse-integer value))))))
	    ((equalp (complex-type field) :collection-contained-item)
	     (validate-sfx (complex-type field)
			   field 
			   edit-item 
			   (parameter field-name)
			   :items
			   (apply #'digx parent-item
				  (digx field :db-type :container-accessor))))
	    ((equalp (complex-type field) :contained-item)
	     (validate-sfx (complex-type field)
			   field 
			   edit-item 
			   (parameter field-name)
			   :items
			   (apply #'digx parent-item
				  (digx field :db-type :container-accessor))))
	    (t
	     (list t nil)))
      (list t nil)))

(defun synq-item-values (data-type fields parent-item edit-item)
  (dolist (field fields)	  
    (when (and (digx field :attributes :editable)
	       (getf field :db-type)
	       (not (find (complex-type field)
			  (list :collection-items
				:list-items
				:hierarchical))))
	   
      (let* ((field-name (frmt "~A" (getf field :name)))
	     (valid (if (equalp (complex-type field) :item)
			(validate-value field
					field-name 
					edit-item
					parent-item
					(parameter field-name))
			(list t nil))))
	      
	(unless (first valid)
	  (pushnew 
	   (list field-name (second valid))
	   (getcx data-type :validation-errors)))
	      
	(when (first valid)
	  (synq-value field edit-item parent-item
		      (parameter field-name)))))	  
	 
    (when (getf field :key-p)	   
      (when (empty-p (parameter (getf field :name)))
	(pushnew
	 (frmt "Key values may not be blank. (~A)~%" (getf field :name))
	 (getcx data-type :validation-errors))))))

(defun grid-append-child (data-type parent-slot parent-item edit-item)
  (unless (getcx data-type :validation-errors)
    ;;Append parent-slot only if new	
    (when parent-slot
      (let ((exists (find-equalp-item edit-item
				      (getx parent-item parent-slot))))
	     
	(if exists
	    (setf exists edit-item)		  
	    (setf (getx parent-item parent-slot)
		  (append (getx parent-item parent-slot)
			  (list edit-item))))))))

(defun grid-persist-item (data-type root-item)
  (unless (getcx data-type :validation-errors)
    (let ((collection (wfx-get-collection
		       (gethash :collection-name (cache *context*)))))

      (unless collection
	(pushnew 
	 "No default store found check if license is selected."
	 (getcx data-type :validation-errors)))

      (when collection
	(setf (item-collection root-item) collection)
	(persist-item collection root-item :allow-key-change-p t)))))

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
 
    (when fields
      (unless edit-item
	(setf edit-item (make-item :data-type data-type)))

      (synq-item-values data-type fields parent-item edit-item)

      (move-uploaded-file fields edit-item)   
      
      (grid-append-child data-type parent-slot parent-item
			 edit-item)
      
      (grid-persist-item data-type root-item))))


(defun store-from-stash (store-name)
  (dolist (store (getcx (gethash :data-type (cache *context*)) :stores))
    (when (equalp store-name (name store))
      
      (return-from store-from-stash store))))





;;todo: file delete
(defmethod action-handler ((action (eql :delete)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  
  (let* ((edit-objects (reverse (set-edit-obects)))
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
       (let ((clean-list (getx parent-item parent-slot)))
	  (dolist (item clean-list)
	    (when (equalp (item-hash item)
			  (ensure-parse-integer (parameter "item-id")))
	      
	      (setf clean-list
		    (remove item clean-list))
	      (let* ((collection (get-perist-collection
		     (gethash :collection-name (cache *context*))))
		    (server-path
		      (string-downcase
		       (frmt "~A/files/~A/~A/"
			     (if (location collection)
				 (location collection)
				 (string-downcase
				  (frmt "~A~A"
					(location (store collection))
					(name collection))))
			     (parameter "data-type")
			     "**";;(getf field :name)
			    ;; (parameter "item-id")
			     )))
		     (files (directory server-path)))

		(dolist (file files)
		  (cl-fad:delete-directory-and-files file))

		)))
	  (setf (getx parent-item parent-slot) clean-list)

	  (persist-item (item-collection root-item) root-item)))

    (unless (and edit-item parent-slot)

      (setf (cl-naive-store::item-deleted-p root-item) t)
      (persist-item (item-collection root-item) root-item)
      (cl-naive-store::remove-item root-item))))
