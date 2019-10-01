(in-package :cl-wfx)

;;TODO: Move this to theme
(defparameter *limit-columns* 7)


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

(defun check-top-level-p (data-type)  
  (when data-type
    (when (getcx data-type :data-type)
      (dig (getcx data-type :data-type) :data-type :top-level-p))))

(defun sub-grid-p (field)
  (find (complex-type field)
	(list :collection-objects
	      :list-objects
	      :hierarchical)))

(defun html-value (value)
  (cond ((symbolp value)
	 (frmt "~S" value))
	((stringp value)
	 (if (search "'" value)
	     (replace-all (replace-all value "'" "&#39;") "\"" "&#34;")
	     value))
	((numberp value)
	 value)
	(t
	 (frmt "~S" value))))

(defun render-dropdown (name selected list
			&key (select-prompt "Select a value")
			  key-func value-func
			  select-onclick-func)
  (let ((selected-value (if selected
			    (if key-func
				(funcall key-func selected)
				selected)
			    (parameter (frmt "~A" name)))))

    (with-html-string
      (:div :class "bt-group dropdown"
	    (:input :type "hidden" :class "selected-value" 
		    :name (frmt "~A" name)
		    :value (html-value (or (parameter (frmt "~A" name))
					   selected-value
					   "")))
	    (:button :class "btn btn-secondary dropdown-toggle"
		     :type "button"
		     :data-toggle "dropdown"
		     :aria-haspopup "true" :aria-expanded "false"
		     (cl-who:str (html-value (or selected-value select-prompt ""))))
	     
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
	 (list (or
		(and (dig field :db-type :values-lambda)
		     (eval% (dig field :db-type :values-lambda)))

		(dig field :db-type :values)))
	  
	 (selected ))
    
    (if (functionp list)
	(setf list (funcall (eval% (dig field :db-type :values-lambda)) item)))

  
    (setf selected (find (getfx item field) list :test #'equalp))
    (with-html-string
      (cl-who:str (render-dropdown name selected list)))))

;;delete this
(defmethod render-input-val-x ((type (eql :value-list)) field item
			       &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (list (or (and (dig field :db-type :values-lambda)
			(cond ((equalp (first (dig field :db-type :values-lambda))
				       'cl-wfx::get-named-list-sorted-values)
			       (eval% (dig field :db-type :values-lambda)))
			      ((equalp (first (dig field :db-type :values-lambda))
				       'cl:lambda)
			 
			       )))
		   (dig field :db-type :values)))
	 (selected (find (getfx item field) list :test #'equalp)))

    (with-html-string
      (cl-who:str (render-dropdown name selected list)))))

(defmethod render-input-val ((type (eql :hierarchical)) field item 
			     &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :collection-objects))
			     field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql :list-objects)) field item
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
				context-state-selected))))
    (with-html-string
      (:div :class "col"
	    
       (:div :class "auto-complete"
	     (:input :type "hidden" :class "selected-value" 
		     :name (frmt "~A" field-name)
		     :value (html-value (or
					 (parameter (frmt "~A" field-name))
					 selected-value
					 "")))
	     
	     (:input :class "form-control auto-complete-text"
		     :type "text"
		     :autocomplete "off"
		     :placeholder
		     (or select-prompt
			 "Press Ctrl for list or start typing and then press Ctrl for list...")
		     :name (frmt "~A-drop" field-name) 
		     :id (frmt "~A-drop" field-name)
		     :value (html-value (or selected-value ""))
		     :onkeydown
		     ;;fires ajax call on Ctrl (13)
		     (js-render-event-key 
		      (frmt "~A-drop" field-name)
		      17
		      "cl-wfx:ajax-auto-complete-x"
		      (frmt "~A-drop-div" field-name)
		      nil
		      (js-pair "data-type"
			       data-type)
		      (js-pair "field-name"
			       (frmt "~A" field-name))
		      (js-pair "collection"
			       (frmt "~A" collection))
		      (js-pair "wfxaction" "auto-complete")))
	     
	     (:div :id (frmt "~A-drop-div" field-name) :class "auto-list"))))))


(defun render-item-list-auto-complete (data-type field item selected
				       &key  select-prompt
					 value-func
					 context-state-selected
					 required-p
					 hierarchy)
  (declare (ignore hierarchy) (ignore item))
  
  (let* ((field-name (dig field :name))
	(selected-value (if selected
			    (if value-func
				(funcall value-func selected)
				selected)
			    (or (parameter (frmt "~A" field-name))
				context-state-selected))))

    (with-html-string
      (:div :class "col"
	    
	    (:div :class "auto-complete"
		  (:input :type "hidden" :class "selected-value" 
			  :name (frmt "~A" field-name)
			  :value (html-value (or
					      (parameter (frmt "~A" field-name))
					      (if (item-p selected)
						  (item-hash selected)
						  selected-value)
					      "")))
		  
		  (:input :class "form-control auto-complete-text"
			  :type "text"
			  :autocomplete "off"
			  :placeholder
			  (or select-prompt
			      "Press Ctrl for list or start typing and then press Ctrl for list...")
			  :name (frmt "~A-drop" field-name) 
			  :id (frmt "~A-drop" field-name)
			  :value (html-value (or selected-value ""))
			  :required (if required-p
					"required")
			  :onkeydown
			  ;;fires ajax call on Ctrl (17)
			  (js-render-event-key 
				 (frmt "~A-drop" field-name)
				 17
				 "cl-wfx:ajax-auto-complete"
				 (frmt "~A-drop-div" field-name)
				 (string-downcase
				  (frmt "grid-edit-~A"  data-type))
				 (js-pair "data-type"
					  data-type)
				 (js-pair "field-name"
					  (frmt "~A" field-name))
				 (js-pair "wfxaction" "grid-auto-complete")))
		  
		  (:div :id (frmt "~A-drop-div" field-name) :class "auto-list"))))))

(defun accessor-value (item accessors)
  (let ((value))
    (if accessors
	(if (listp accessors)
	    (if (listp (first accessors))
		(let ((values))
		  (dolist (accessor accessors)
		    (push
		     (apply #'digx
			     item
			     accessor)
		     values))
		  (setf value (format nil
				      "~{~a~^ ~}"
				      (reverse values))))
		(setf value (apply #'digx
				   item
				   accessors)))
	    (setf value (apply #'digx
				item
				(list accessors))))
	item)))

(defun fetch-contained-item-list (field edit-item)
  
  (let ((list-containers
	 (wfx-query-context-data
	  (dig field :db-type :collection)
	  :query (lambda (itemx)
		  (if (getf field :filter)
		      (funcall
		       (eval% (getf field :filter))
		       itemx
		       edit-item)
		      t))))
	      
	(container-accessors (dig field :db-type :container-accessor))
	(accessors (dig field :db-type :accessor))
	(list))

    
    (when (not (dig field :db-type :container-fetch))
      
      (dolist (list-item list-containers)
	
	(setf list (pushnew (accessor-value list-item container-accessors) list))))


    (when  (dig field :db-type :container-fetch) 
      (dolist (list-item list-containers)
	(setf list
	      (append list (funcall
			    (eval% (dig field :db-type :container-fetch))
			    (accessor-value list-item container-accessors)
			    edit-item)))))

    (sort (copy-list list) #'string<
	  :key (lambda (item)
		 (accessor-value item accessors)))))


(defmethod render-input-val ((type (eql :collection-contained-item)) field item 
			     &key data-type hierarchy &allow-other-keys)

  (let* ((name (getf field :name))
	
	 (list (fetch-contained-item-list field item))
	 (selected)
	 (accessors (dig field :db-type :accessor)))

    (setf selected (find (getx item name)  
			 list :test #'equalp))
    
    
    (with-html-string
      (cl-who:str (render-item-list-auto-complete
		   data-type field item selected
		   :value-func (lambda (item)
				 (accessor-value item accessors))
		   :context-state-selected (getcx 
					    (dig field :db-type :data-type)
					    (frmt "~A-drop" name))
		   :required-p (getf field :key-p)
		   :hierarchy hierarchy)))))




(defmethod render-input-val ((type (eql :collection)) field item 
			     &key data-type hierarchy &allow-other-keys)

  (let* ((name (getf field :name))
	 (list (wfx-query-context-data
		(dig field :db-type :collection)
		:query (lambda (itemx)
			(if (getf field :filter)
			    (funcall
			     (eval% (getf field :filter))
			     itemx
			     item)
			    t))))
	 (selected (find (getx item name)  
			 list :test #'equalp))
	 (accessors (dig field :db-type :accessor)))
    
    (with-html-string
      (cl-who:str (render-item-list-auto-complete
		   data-type field item selected
		   :value-func (lambda (item)
				 (accessor-value item accessors))
		   :context-state-selected (getcx 
					    (dig field :db-type :data-type)
					    (frmt "~A-drop" name))
		   :required-p (getf field :key-p)
		   :hierarchy hierarchy
		    )))))

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
			:key-func 'cl-naive-items:item-hash
			:value-func (lambda (item)
				      (accessor-value item accessors)))))))

(defun grid-js-render-form-values (data-type form-id 
				   &key action action-lambda
				     action-data item-id
				     hierarchy)
  (let ((active-page (getcx 
		      data-type :active-page)))
   

    (js-render-form-values 
     "cl-wfx:ajax-grid"
     (gethash :collection-name (cache *context*))
     form-id
     (js-pair "data-type"
	      (frmt "~A" data-type))
     
     (js-pair "wfxaction" (or action ""))

     (js-pair "action-lambda" (or action-lambda ""))
     (js-pair "action-data" (or action-data ""))
     
     (js-pair "item-id" (frmt "~A" (or item-id (getcx data-type :item-id)
				       "")))
     (js-pair "item-hierarchy"
	      (hierarchy-string hierarchy))
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
     (js-pair "wfxaction" "upload-file")))

(defun grid-js-render (data-type &key action item-id)
  (let ((active-page (getcx data-type :active-page)))

    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "wfxaction" (or action ""))
	       
	       (js-pair "item-id" (frmt "~A" (or item-id "")))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-expand-buttons (subs data-type item)
  (if subs
      (if (string-equal
	  (frmt "~A" (getcx data-type :expand-id)) 
	  (frmt "~A" (item-hash item)))
	  (with-html-string
	    (:button
	     :name "expand" :type "submit" 
	     :class "btn btn-sm border-0 active"
	     :aria-pressed "true"
	     :onclick (grid-js-render data-type
				      :action "unexpand")
	     (:i :class "fa far fa-chevron-circle-up fa-lg  text-secondary")
	     
	     ))
	  (with-html-string
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-sm border-0"
	     :aria-pressed "false"
	     :onclick (grid-js-render data-type
					    :action "expand"
					    :item-id (item-hash item))
	     (:i :class "fa far fa-chevron-circle-right fa-lg text-light"))))))

(defun render-select-button (item)
  (with-html-string
    (:div :class "form-check"
	  (:input
	   :class "form-check-input grid-selection"
	   ;; :style "height:15px;width:15px;"
	   :type "checkbox"
	   :id "grid-selection"
	   :name "grid-selection"
	   :value (frmt "~A" (item-hash item))	    
	   :aria-label "Select Row"))))

(defun grid-js-render-delete (data-type &key item hierarchy)
  (let ((active-page (getcx data-type :active-page)))

    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "wfxaction" "delete")
	       
	       (js-pair "item-id" (frmt "~A" (or (item-hash item) "")))
	       (js-pair "item-hierarchy"
			(hierarchy-string hierarchy))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun grid-js-render-action (data-type action &key item)
  (let ((active-page (getcx data-type :active-page)))
    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       
	       (js-pair "wfxaction" "item-action")
	       (js-pair "action-name" (string-downcase (frmt "~A" (or action ""))))

		       
	       (js-pair "item-id" (frmt "~A" (or (item-hash item) "")))


	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun hierarchy-string (items)
  (let ((hierarchy ""))
     (dolist (item items)
      (setf hierarchy  (concatenate 'string hierarchy " "
				    (frmt "(~A ~A)"
					  (getf item :data-type)
					  (if (getf item :item)
					      (if (item-p (getf item :item))
						  (item-hash (getf item :item))
						  (getf item :item)
						  )
					      )))))
    (frmt "(~A)" hierarchy)))

(defun grid-js-render-edit (data-type &key action item hierarchy)
  (let ((active-page (getcx data-type :active-page)))
    
       (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-edit-~A" (item-hash item))	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "wfxaction" (string-downcase (frmt "~A" (or action ""))))
	     
	       
	       (js-pair "item-id" (frmt "~A" (or (item-hash item) "")))
	       (js-pair "item-hierarchy"
			(hierarchy-string hierarchy))
	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-grid-buttons (data-type item hierarchy)
  (let ((permissions (getx (context-spec *context*) :permissions)))
    (with-html-string
      (dolist (permission permissions)
	(cond ((or (equalp permission :view) (equalp permission :update))
	       (cl-who:htm
		(:i :name "edit" 
		 :class
		 (if (user-context-permission-p
				  (getx (context-spec *context*) :name)
				  :update)
		     "fa fa-edit fa-2x "
		     "fa fa-file-o fa-2x ")
		 :style "color:#5DADE2  "
		 :aria-pressed 
		 (if (and (parameter "item-id")
			  (string-equal 
			   (parameter "item-id") 
			   (frmt "~A" (item-hash item))))
		     "true"
		     "false")
		 :onclick
		 (frmt "~A;toggle_tbody(\"ajax-edit-~A\");toggle_tbody(\"ajax-editing-row-~A\");toggle_tbody(\"ajax-expand-row-~A\");"
		       (grid-js-render-edit data-type
					    :action "edit"
					    :item item
					    :hierarchy hierarchy)
		       (item-hash item)
		       (item-hash item)
		       (item-hash item)))))
	      ((and (equalp permission :delete)
		    (user-context-permission-p
		     (getx (context-spec *context*) :name)
		     :delete))
	       (cl-who:htm
		(:i 
		 :name "edit"
		 :class "fa fa-times fa-2x "
		  :style "color:#EC7063  "
		 :onclick
		 (frmt "if(confirm(\"Are you sure you want to delete?\")){~A}"
		       (grid-js-render-delete data-type
					      :item item
					      :hierarchy hierarchy)))))))
      
      

      (dolist (action (getf (getcx data-type :data-type) :item-actions))
	(cond ((equalp (getf action :type) :button)
	       (cl-who:htm
		(:button
		 :name (getf action :name)
		 :type "submit"
		 :onclick
		 
		 (if (getf action :confirmation)
		     (frmt "~A;alert(\"~A\")"
			   
			   (grid-js-render-action data-type
						  (getf action :name)
						  :item item)
			   (getf action :confirmation))
		     (grid-js-render-action data-type
						  (getf action :name)
						  :item item))
		 
		 (cl-who:str (getf action :label))
		 )))
	      ((equalp (getf action :type) :link)
	       (cl-who:htm
		(:a :class "btn"  :role "button"
		    :target "_blank"
		    :href (cl-who:str (apply%
				       (eval%
					(getf action :action))
				       (list item
					     (getcx data-type :parent-item))))
		    (cl-who:str (getf action :label))))

	       ))
	)
      )))

(defun render-edit-buttons (data-type hierarchy)
  (with-html-string
    
    (when (user-context-permission-p
	   (getx (context-spec *context*) :name)
	   :update)
      (cl-who:htm
       (:i :name "save" 				   	 
	   :class "fa fa-save fa-2x text-success"
	   :onclick
	   (frmt "if(document.getElementById(\"grid-edit-~A\").checkValidity()) {~A}else console.log(\"invalid form\");"
		 data-type
		 (grid-js-render-form-values			 
		  data-type
		  (string-downcase
		   (frmt "grid-edit-~A"  data-type))
		  
		  :action "save"
		  :hierarchy hierarchy)))))
    (:i :name "cancel" 				   
	:class "fa fa-eject fa-2x text-dark"
	:onclick (frmt "~A;toggle_display(\"ajax-edit-row-~A\");"
		       (grid-js-render data-type
				       :action "cancel")
		       (item-hash (getcx data-type :edit-item))))))


(defun render-grid-edit-row (data-type name field label item parent-item hierarchy)
  (with-html-string
   (:div
    :class
    (if (digx field
	      :attributes :editable)
	"form-group"
	"form-group disabled")
    (:label
     :for name 
     :class "col-form-label font-weight-bold"
     :style "font-size:13px;"
     (cl-who:str
      (if label
	  label
	  (string-capitalize 
	   (substitute 
	    #\Space  
	    (character "-")  
	    (format nil "~A" name) 
	    :test #'equalp)))))
    (or
     (cl-who:str
      (render-input-val 
       (complex-type field) 
       field item
       :parent-item
       parent-item
       :data-type data-type
       :hierarchy hierarchy))))))


(defun render-grid-edit-more (field item hierarchy)
  (with-html-string
    (let* ((sub-data-type
	    (digx field :db-type
		  :data-type))
	   (sub-fields
	    (getcx sub-data-type :fields)))

      (unless sub-fields
	(set-type-context sub-data-type)
	(setf sub-fields
	      (getcx sub-data-type :fields)))
      
      (cl-who:htm
       (:div
	:class "row"

	(:div :class "col"
	      (:button :class "btn btn-primary"
		       :type "button"
		       :data-toggle "collapse"
		       :data-target (frmt "#~A-more" sub-data-type)
		       :area-expanded "false"
		       :area-controls (frmt "~A-more" sub-data-type)
		       (cl-who:str (getf field :label)))
	      ))
  
       (let ((sub-item (getx item (getf field :name))))
	 (unless sub-item
		(setf sub-item (make-item :data-type sub-data-type)))
	 (cl-who:htm
	  (:div
	   :class "row collapse" :id (frmt "~A-more" sub-data-type)
	   (:div :class "col" 
		 (dolist (sub-field sub-fields)
		   (unless (sub-grid-p sub-field)
		     (cl-who:str
		      (render-grid-edit-row
		       sub-data-type
		       (getf sub-field :name)
		       sub-field
		       (getf sub-field :label)
		       sub-item
		       item
		       hierarchy)))
		   
		   )))))
       ))))

(defun render-grid-edit (data-type fields item parent-item
			 parent-spec hierarchy)
  (setf (getcx data-type :edit-item) item)
  (setf (getcx data-type :parent-spec) parent-item)
  (setf (getcx data-type :parent-spec) parent-spec)
  (setf (getcx (parameter "data-type") :item-id)
	(if item
	    (item-hash item)
	    nil))

  (let ((header-fields (get-header-fields fields))
	(sub-fields (get-sub-fields fields)))
    (with-html-string
      
      (:tbody :id (frmt "ajax-edit-~A"
			(if item
			    (item-hash item)
			    nil))	
	      (:tr :class "bg-light"

		   (if sub-fields
		       (cl-who:htm
			(:td :style "width: 25px;" )))
	
		   (dolist (field (limit-fields (get-data-fields
						 header-fields)
						parent-item))	  
		     (cl-who:str (render-table-cell field item)))

	

		   (:td 
			(:div
			 (cl-who:str (render-edit-buttons data-type hierarchy)))))
       
	      (:tr 
	       (:td :colspan (if sub-fields
				 (+ (length (limit-fields header-fields
							  parent-item))
				    2)
				 (+ (length (limit-fields header-fields
							  parent-item))
				    1))		 
		    (:form :class "card"
			   :style "border-left-style: dotted;border-width:3px; border-left-color:#F1948A;min-width:255px;max-width:99%"
			   :id (string-downcase (frmt "grid-edit-~A"  data-type))

			   (:div
			    :class "card-body p-0 m-0"

			    (:div :class "row" 
				  :id (frmt "~A" data-type)
				  (:div :class "col"
					
					(dolist (field fields)
					  (let* ((name (getf field :name))
						 (label (getf field :label)))
					
					    (when (and (digx field :attributes :display) 
						       (not (sub-grid-p field)))

					      (unless (equalp (complex-type field)
							      :item)
					
					  
						(cl-who:str
						 (render-grid-edit-row
						  data-type name
						  field label
						  item parent-item
						  hierarchy)))
				      
					      )))))
		    
			    (dolist (field fields)
			      (when (and (digx field :attributes :display) 
					 (not (sub-grid-p field)))

				(when (equalp (complex-type field)
					      :item)
			  
				  (when (data-type-access-p (digx field :db-type :data-type))
				    (cl-who:str
				     (render-grid-edit-more field item hierarchy)))))))

			   
			   (when (getcx data-type :validation-errors)
			     (let ((errors (getcx data-type :validation-errors)))
			       (setf (getcx data-type :validation-errors) nil)
			       (cl-who:htm
				(:div :class "card-footer"
				      (:div :class "row"
					    (:div :class "col text-danger"
						  (:strong
						   (cl-who:str
						    (frmt "Errors ~S"
							  errors))))))))))))))))

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
	     "collapseFilter"
	     (js-pair "data-type"
		      (frmt "~A" data-type))
	     (js-pair "wfxaction" "grid-col-filter")))))

(defun limit-fields (fields hierarchy)
  (if (> (length fields) 1)
      (if (and (not (item-p hierarchy))
	       (>= (length hierarchy) 1))
	  (if (> (length fields) 3)
	      (subseq fields 0 3)
	      fields)
	  (if (> (length fields) 7)
		  (subseq fields 0 7)
		  fields))
      fields))

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

(defun get-sub-fields (fields)
  (let ((sub-fields))
    (dolist (field fields)
      (when (and
	     (sub-grid-p field)
	     )
	(pushnew field sub-fields)))
    (reverse sub-fields)))

(defun get-header-fields (fields)
  (let ((header-fields))
    (dolist (field fields)
      (when (and (digx field :attributes :display)
		 (not (sub-grid-p field)))
	(pushnew field header-fields)))
    (reverse header-fields)))

(defun render-filter-row (data-type fields subs)
  (let* ((search-term (or (parameter "search") 
			  (getcx 
			   data-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (or (getcx data-type :filter)
		       (getcx data-type :filter-fields))))
    (with-html-string
      
      (:tr :class (if (or search-p filter-p)
		      "collapse show"
		      "collapse form-group")
	   :aria-expanded (if (or search-p filter-p)
			      "true"
			      "false")
	   :id "collapseFilter"

	   (if subs
	       (cl-who:htm (:th)))
	   
	   
	   (dolist (field (limit-fields (get-header-fields fields)
					nil))
		 (let* ((name (getf field :name)))
						
		   (cl-who:htm
		    (:th
			  (cl-who:str
			   (render-grid-col-filter 
			    data-type name))))))
	   (:th)))))

(defun limit-sub-fields (fields)
  (if (> (length fields) 2)
      (subseq fields 0 2)
      fields))

(defun render-header-row (data-type fields subs hierarchy)
  (let ((header-fields (limit-fields (get-header-fields fields)
				     hierarchy)))
    (with-html-string
      
      (:tr :class "bg-light"
	   :style "box-shadow: 0px 2px 2px;"
	   (if subs
	       (cl-who:htm (:th :style "width:25px;")))
	   
	   (dolist (field header-fields)
	     (let* ((name (getf field :name))
		    (label (getf field :label)))
	       
	       (cl-who:htm
		(:th ;;:class "text-center"
		     (:h6 (cl-who:str
			   (if label
			       label
			       (string-capitalize 
				(substitute #\Space  (character "-")  
					    (format nil "~A" name) 
					    :test #'equalp)))))))))

	   (:th 
	    )
	   
	   (when (check-top-level-p data-type)
	     
	     (cl-who:htm
	      
	      (:th :class "text-center"
		  
	       :width "5px"
		   (cl-who:htm
		    (:input
		      :style "margin-top:.5rem;"
		     :type "checkbox"
		     :id "grid-select-all"
		     :name "grid-select-all"
		     :onclick "gridSelectAll();"
		     :value "All"
		     :checked (parameter "grid-select-all"))
		    ;;	"All"
		    )
		   
		   )))))))

(defun render-grid-header (data-type sub-p hierarchy)
  (let ((fields (getcx	data-type :fields))
	(subs))

    (dolist (field fields)
      (when (sub-grid-p field)
	(pushnew field subs)))
    
    (with-html-string
      (unless sub-p
	(cl-who:str
	       (render-filter-row data-type fields subs)))
      
      (cl-who:str (render-header-row data-type fields subs hierarchy)))))

(defun get-data-fields (fields)
  (let ((data-fields))
    (dolist (field fields)
      (when (and (digx field :attributes :display)
		 (not (sub-grid-p field)))
	(pushnew field data-fields)))
    (reverse data-fields)))


(defun render-select-actions (data-type)  
  (let ((action-list))

    (dolist (lambdax (getx (context-spec *context*) :lambdas))
      (when (find :select-action-list (getx lambdax :events) :test 'equalp)
	(setf action-list (eval% (digx lambdax :lambda :code)))))

    (when (user-context-permission-p
	   (getx (context-spec *context*) :name)
	   :delete)
      (setf action-list (append action-list
				(list (list :action-name "Delete Selected"
					    :handler-lambda :delete-selected)))))
    (when action-list 
      (with-html-string
	;;TODO: float right fucks up dropdown menu positioning
	  (:div :class " float-right"
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
		     (js-pair "wfxaction"
			      "grid-select-action")
		     
		     (js-pair "action-data"
			      (or (getf action :data) ""))
		     (js-pair "action-handler-lambda"
			      (frmt "~A" (getf action :handler-lambda)))
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
			:accessor (if (sub-grid-p field)
				      (dig field :db-type :accessor)
				      (if (find (complex-type field)
						(list :collection
						      :collection-contained-items))
					  (dig field :db-type :accessor))
				      )))))))
    keys))

(defun sort-by-keys (items keys)
  (flet ((sort-val (item)
	   (let ((values))
	     (dolist (key keys)
	       (let* ((accessor (getf key :accessor))
		      (val (if accessor
			       (accessor-value (getx item (getf key :name)) accessor)
			       (getx item (getf key :name)))))
		
		 (setf values (frmt "~A~A" values val))))	    
	     values)))
    (sort (copy-list items) #'string-lessp :key #'sort-val)))


(defun grid-js-render-new (data-type hierarchy)
  (let ((active-page (getcx data-type :active-page))
	(items))

    (setf items (append hierarchy (list (list :data-type data-type
					      :item 0))))
    
    (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-new-~A" data-type)	      
	       (js-pair "data-type" (frmt "~A" data-type))
	       
	       (js-pair "wfxaction" "new")
	       
	       (js-pair "item-hierarchy"
			(hierarchy-string items))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-sub-new-button (sub data-spec hierarchy)
  (when (user-context-permission-p
	   (getx (context-spec *context*) :name)
	   :update)
    (with-html-string
      (when (find (complex-type sub)
		  (list :collection-objects
			:hierarchical))
	(cl-who:htm
	 (:button
	  :name "select-from" :type "submit" 
	  :class "btn btn-sm btn-outline-success float-right"
	  :aria-pressed "false"
	  :onclick 
	  (grid-js-render
	   data-spec
	   :action "select-from" )
	  (cl-who:str "Select From"))
	 ))
      (:button
       :name "new" :type "submit" 
       :class "btn btn-sm btn-outline-success float-right"
       :aria-pressed "false"
       :onclick 
       (grid-js-render-new data-spec hierarchy)
       (:i :class "fa fa-plus-square")))
    ))

(defun render-table-cell (field item)
  
  (let ((val (print-item-val 
	      (complex-type field)
	      field item)))
   
    (with-html-string
      (cl-who:htm
       (:td

	:style (cond
		 ((or (equalp (simple-type field) :integer)
		      (equalp (simple-type field) :number))
		  "text-align:right;")
		 (t
		  "text-align:left;"))
	(cond
	  ((empty-p val)
	   (cl-who:str val))
	  ((or (equalp (complex-type field) :lambda)
	       (equalp (complex-type field) :lisp-code)
	       (equalp (complex-type field) :css)
	       (equalp (complex-type field) :html)
	       (equalp (complex-type field) :java-script))
	   (cl-who:htm
	    (:textarea :readonly "readonly" :style "width:100%;"
		       (cl-who:str val))))
	      
	  ((equalp (complex-type field) :text)
	   (cl-who:htm
	    (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		  (cl-who:str val)
		  )))
	  ((and (or (equalp (complex-type field) :string)
		    (equalp (complex-type field) :link)
		    (equalp (complex-type field) :list)
		    (equalp (complex-type field) :collection))
		(or (> (length val) 15)
		    (and
		     (> (length val) 15)
		     (> (length val) (length (getf field :label))))))

	   (if (equalp (complex-type field) :link)
		(cl-who:htm
		 (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		       (:a :target "_blank" :href val (cl-who:str val))))
		(cl-who:htm
		 (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		       (cl-who:str val))))
	   )
	  ((or (equalp (complex-type field) :number)
	       (equalp (complex-type field) :integer))
	   (cl-who:str val))
	  (t
	       
	   (cl-who:htm
		 
	    (:div
	     :style "display:table-cell;height:25px;vertical-align:middle;width:100%;"
			    
	     (cl-who:str val))))))))))

(defun render-item-row (subs data-type item fields hierarchy)
  (let ((row-fields (limit-fields (get-data-fields fields)
				  (if (> (length hierarchy) 1)
				      hierarchy
				      nil))))

    (with-html-string
      (:tbody
       :class "grow"
       :style "display:table-row-group;"
       :id (frmt "ajax-editing-row-~A" (item-hash item))
       :data-hash (item-hash item)
       :data-type data-type
       :data-action "expand"
       :data-pages (or (parameter "pages") 50)
       :data-page (or (getcx data-type :active-page)  1)
       :data-collection (gethash :collection-name (cache *context*))
       :data-expanded (if (string-equal (frmt "~A" (getcx data-type :expand-id)) 
					(frmt "~A" (item-hash item)))
			  "Yes"
			  "No")
       :data-subs (if subs
		      "Yes"
		      "No")
       
       (:tr 
	(if subs
	    (cl-who:htm
	     (:td :style "width:25px;"
		  (cl-who:str
		   (render-expand-buttons subs data-type item)))))
	
	(dolist (field row-fields)
	  (cl-who:str (render-table-cell field item)))

	(:td :style "width:40px;"
	      (:div :class "btn-group"
		   
		   (when (not *rendering-shit*)
		     (cl-who:str			      
		      (render-grid-buttons data-type item hierarchy)))
		   
		   ))
	(when (check-top-level-p data-type)
	  (cl-who:htm
	   (:td :style "width:40px;text-align:center;"
	    
	    (cl-who:str
	     (render-select-button item)))))
	
	)))))



(defun render-select-item-row (data-type items fields)
  (with-html-string
      (:tbody :style "display:table-row-group;"
	      :id (frmt "select-from-~A"
			data-type)
	      (dolist (item items)
		(cl-who:htm
		 (:tr
		  (:td)
	
		  (dolist (field (limit-fields fields
					       nil))
		    (cl-who:str (render-table-cell field item)))

		  (:td :width "5px"
		       (:div :class "btn-group"
			     (cl-who:str
				(render-select-button item))))))))))

(defun render-select-from-grid (data-type sub sub-data-spec hierarchy)
 
  (when (and (equalp (parameter "wfxaction")
		     "select-from")
	     (string-equal
	      (parameter "data-type")
	      (frmt "~A" sub-data-spec)))


    (let ((*rendering-shit* t)
	  (fields (get-data-fields
		   (getcx (dig sub :db-type :data-type) :fields))))
      
      (with-html-string
	(:tbody :style "display:table-row-group;"
		:id (frmt "ajax-select-from-group-~A"
			  (dig sub :db-type :data-type))
		(:tr
		 (:td :colspan (+ (length (limit-fields fields
							nil))
				  2)
		      (:span :class "font-weight-bold"
		       (cl-who:str
			  (frmt "Select ~A to add..."
				(dig sub :db-type
				     :collection))))

		      (:i :name "cancel" 				   
			  :class "fa fa-eject fa-2x text-dark float-right"
			  :onclick
			  (frmt
			   "~A;toggle_display(\"ajax-select-from-group-~A\");"
			   (grid-js-render data-type
					   :action "cancel")
			   (dig sub :db-type :data-type)))
		      (:span :class "float-right"
		       "&nbsp")
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
		       
			   (js-pair "wfxaction" "add-selection")
			   (js-pair "add-selection-field"
				    (frmt "~A" (dig sub :name)))
			   (js-pair "item-hierarchy"
				    (hierarchy-string hierarchy))
			   (js-pair "pages"
				    (or (parameter "pages") 50))
			   (js-pair "page"
				    (or (getcx data-type :active-page) 1)))
			  (cl-who:str "Add Selection")))))
	
	(:tbody :style "display:table-row-group;"
		(:tr
		 (:td :colspan (+ (length (limit-fields fields
							nil))
				  2)		      
		      (:table :class "table table-sm grid-table-stuffx"
		       (cl-who:str (render-select-item-row
				    (dig sub :db-type :data-type)
				    (wfx-query-context-data
				     (dig sub :db-type :collection)
				     :query (lambda (item)
					     (if (getf sub :filter)
						 (funcall
						  (eval% (getf sub :filter)) item (first (last (car hierarchy))))
						 item)))
				    fields))))))))))

(defun render-expand (data-type item subs sub-level hierarchy)

  (when (string-equal (frmt "~A" (getcx data-type :expand-id)) 
		      (frmt "~A" (item-hash item)))

   
    (with-html-string
      (:div :class "row card-columns"
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
		 (:div :class "col"
		       (:div :class "card"
			     :id sub-data-spec
			     :style "border-left-style: dotted;border-width:2px; border-left-color:#48C9B0;min-width:255px;"
			     (:h5 :class "card-header"
				  (cl-who:str
				   (frmt "~A" (string-capitalize
					       (getf sub :name))))
				  (cl-who:str
				   (render-sub-new-button
				    sub
				    sub-data-spec
				    hierarchy)))

			     (:div :class "card-body p-0 m-0"
				   (:div  :class "row"
					  (:div :class "col"
						(:table
						 :class "table table-sm grid-table-stuffx"
						 :style "width: 100%;"

						 (:tbody
						  :style "display:none"
						  :id (frmt "ajax-edit-nil" ))
						 (:tbody
						 
						  (cl-who:str
						   (render-grid-header
						    sub-data-spec
						    t
						    hierarchy)))
						 
						 (cl-who:str
						  
						  (render-grid-data
						   sub-data-spec
						   (getfx item sub) 
						   (+ sub-level 1)
						   item
						   data-type
						   hierarchy))))))
			     
			     (cl-who:str
			      (render-select-from-grid
			       data-type sub
			       sub-data-spec
			       hierarchy)))))))))))



(defun render-row-goodies (subs sub-level data-type
			   item  parent-item parent-spec
			   fields
			   hierarchy)
  (unless *rendering-shit*
    (let ((header-fields (get-header-fields fields)))

      (with-html-string
	
	(if (and (equalp (parameter "wfxaction") "save")
		 (getcx data-type :edit-object)
		 (getcx data-type :validation-errors))
	    
	    (cl-who:str (render-grid-edit data-type
					  fields
					  item
					  parent-item
					  parent-spec
					  hierarchy
					  ))
	    (cl-who:htm (:tbody
			 :class ""
			 :style "display:none"
				:id (frmt "ajax-edit-~A" (item-hash item)))
			(:tbody
			 :class ""
			 :id (frmt "ajax-expand-row-~A" (item-hash item))
			 :style (if (and (string-equal (frmt "~A" (getcx data-type :expand-id)) 
						       (frmt "~A" (item-hash item))))
				    "display:table-row-group"
				    "display:none")
			 (if (string-equal (frmt "~A" (getcx data-type :expand-id)) 
					   (frmt "~A" (item-hash item)))
		    
			     (cl-who:htm
			      (:tr
			  
			       (:td :colspan (if subs
						 (+ (length
						     (limit-fields header-fields
								   (if (> (length hierarchy) 1)
								       hierarchy)))
						    3)
						 (+ (length
						     (limit-fields header-fields
								   (if (> (length hierarchy) 1)
								       hierarchy)))
						    1))
			   
				    (cl-who:str (render-expand data-type item subs
							       sub-level
							       hierarchy)))))))))
	
	))))


(defun render-new-edit (data-type fields parent-item parent-spec hierarchy)
  (with-html-string

    (:tbody ;;:style "display:none"
	    :id (frmt "ajax-new-~A" data-type)
	    
	    (when (and (getcx data-type :edit-item)
		       (not (item-hash (getcx data-type :edit-item))))

	      (when (and (and (equalp (parameter "wfxaction") "save")
			      (getcx data-type :edit-object)
			      (getcx data-type :validation-errors))
			 (string-equal (parameter "data-type")
				       (frmt "~A" data-type)))
		
		(cl-who:str
		 (render-grid-edit data-type fields
				   (getcx data-type :edit-item)
				   parent-item parent-spec
				   hierarchy)))))))


(defun data-type-access-p (data-type)
  (let ((access-p))
  
    (when (and (active-user)
	       (digx (active-user) :selected-licenses))
      (cond ((getx (current-user) :super-user-p)
	     (setf access-p t))
	    (t (dolist (lic (digx (active-user)
				  :selected-licenses))
		 (when (license-user lic)
		   (let ((no-spec t))
		     (dolist (permission
			       (getx (license-user lic)
				     :data-type-permissions))
		       
		       (when (equalp data-type
				     (digx permission
					   :type-name))
			 (setf no-spec nil)
			 
			 (when (getf (digx permission
					   :permissions)
				     :update)
			   (setf access-p t))))
		     (when no-spec
		       (setf access-p t))))))))
    access-p))

(defun render-grid-data (data-type page-items sub-level
			 parent-item parent-spec hierarchy)
  (let ((data-objects))

    
    
    (with-html-string
      (let ((fields (getcx data-type :fields))
	    (subs))
		    
	(dolist (field fields)
	  (when (sub-grid-p field)
	 
	    (when (data-type-access-p (getf (getf field :db-type)
					    :data-type))
	     
	      (pushnew field subs))))

	
	(setf data-objects (sort-by-keys page-items (keysx fields)))	

	(dolist (item data-objects)
	  (let ((item-hierarchy (append hierarchy (list (list :data-type
							     data-type
							     :item item)))))

	    (unless (and (equalp (parameter "wfxaction") "save")
			 (getcx data-type :edit-object)
			 (getcx data-type :validation-errors))
	      (cl-who:str
	       (render-item-row subs data-type item fields
				item-hierarchy)))

	    
	    (cl-who:str
	     (render-row-goodies subs
				 sub-level
				 data-type
				 item
				 parent-item
				 parent-spec
				 fields
				 item-hierarchy))))
		    
	(cl-who:str (render-new-edit data-type fields
				     parent-item parent-spec
				     hierarchy))))))

(defun render-grid-sizing (data-type)
  (with-html-string
    (:input :type "text" :name "pages"
	    :class "float-right form-control-sm"
	    :size 2
	    :id "pages"
	    :value (or (parameter "pages")
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
	     nil
	     (js-pair "data-type"
		      (frmt "~A" data-type))	     
	     (js-pair "wfxaction" "grid-sizing")))))

(defun render-grid-search (data-type)
  (with-html-string
    (:input :type "text"
	     :class "form-control" ;;"form-control-sm float-right"
	     :name "search" 	   
	     :id "search"
	     :value (or (parameter "search")
			(getcx 
			 data-type :search)
			"")
	     :placeholder "Enter a deep search value..."
	     :onkeydown
	     ;;fires ajax call on enter (13)
	     (js-render-event-key 
	      "search"
	      13
	      "cl-wfx:ajax-grid"
	      (gethash :collection-name (cache *context*))
	      nil
	      (js-pair "data-type"
		       (frmt "~A" data-type))
	      (js-pair "wfxaction" "grid-search")))))

(defun fetch-grid-page-data (data-type items)

  (let* ((keys (keysx (getcx 
		      data-type 
		      :fields)))

	 (sorted-items (sort-by-keys items keys)))

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
	(floor (ensure-num (getcx data-type :data-count)) 
	       (getcx data-type :show-pages-count))
      
      (setf (getcx data-type :page-count) page-count)
      (setf (getcx data-type :page-count-remaining) rem))
 
    
    (setf (getcx data-type :start-page-count) 
	  (- (* (ensure-num (getcx 
			     data-type :active-page)) 
		(ensure-num (getcx 
			     data-type :show-pages-count))) 
	     (ensure-num (getcx 
			  data-type :show-pages-count))))

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
 
    (when (or (not (getcx data-type :end-page-count))
	      (>= (getcx data-type :end-page-count)
		  (length sorted-items)))
      (setf (getcx data-type :end-page-count) (length sorted-items)))


    (setf (getcx data-type :data-subset-count) (length items))
    
    (when items
      (subseq sorted-items 
	      (getcx data-type :start-page-count) 
	      (getcx data-type :end-page-count)))))

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
	      (when (sub-grid-p field)
		(let ((accessor (dig field :db-type :accessor)))
		  (dolist (sub-val (getfx item field))
		    (when sub-val
		      (let* ((val (accessor-value item accessor)))
		
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
	    (when (sub-grid-p field)
	      (let ((accessor (dig field :db-type :accessor)))
	
		(dolist (sub-val (getfx item field))
		  (when sub-val
		    
		    (let* ((val (accessor-value item accessor)))
		     
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
      
      (multiple-value-bind (itemsx count)
	    (wfx-query-context-data collection-name
				    :query test)
	  (setf (getcx data-type :data-count) count)
	  (setf items itemsx)))

    (when (or search-p filter-p (getcx data-type :filter-fields))
      (when (getcx 
	     data-type 
	     :filter-fields)
	(multiple-value-bind (itemsx count)
	    (wfx-query-context-data collection-name
				     :query (filter-function data-type))
	  (setf (getcx data-type :data-count) count)
	  (setf items itemsx))
	
	(when items
	  (setf items
		(query-data
		 items
		 :query (search-function data-type search-term)))))
      (unless (getcx 
	       data-type 
	       :filter-fields)

	(multiple-value-bind (itemsx count)
	    (wfx-query-context-data collection-name
				       :query (search-function
					      data-type search-term))
	  (setf (getcx data-type :data-count) count)
	  (setf items itemsx))))

    
    
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
		 (:span :class "font-weight-bold"
		  (cl-who:str
			 (frmt "Showing ~A of ~A"
			       (if (> (getcx data-type :data-count)
				      (cl-wfx::parse-integer
				       (or (parameter "pages") "50")))
				   (if (< (getcx data-type :data-subset-count)
					  (or (and (parameter "pages") (parse-integer (parameter "pages"))) 50))
				       (getcx data-type :data-subset-count)
				       (or (parameter "pages") 50))
				   (getcx data-type :data-count))
			       (getcx data-type :data-count))))
		 (:span "&nbsp"))
	    
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
			     (js-pair "wfxaction" "page-previous"))
		  "Previous"))
	    
	    (let ((real-page-count (if (>  how-many-rem 0)
				       (+ how-many-pages 1)
				       how-many-pages)))
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
				  (js-pair "wfxaction" "page"))
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
				(js-pair "wfxaction" "page-next"))
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
	     (wfx-query-context-data-object
	      (getcx (parameter "data-type")
		     :collection-name)
	      :query (lambda (item)
		       (string-equal
			(frmt "~A" (item-hash item))
			(frmt "~A" (second spliff)))))
	     (getx (getcx (parameter "data-type") :active-item)
		   (intern (string-upcase (parameter "add-selection-field"))

			   :keyword)))))))
    (when persist-p

           
      (let* ((hierarchy (cl-wfx:read-no-eval (parameter "item-hierarchy")))
	     (root-hash (if hierarchy
			    (second (first hierarchy))))
	     (root-item
	      (fetch-grid-root-edit-item root-hash)))

	(persist-object
	 (wfx-get-collection	
	  (gethash :collection-name (cache *context*)))
	 (if (listp root-item)
	     (first root-item)
	     root-item))))))

(defun delete-selected (selected)
  (dolist (item selected)
    (setf (item-deleted-p item) t)
    (persist-object (item-collection item) item)
    ;;(cl-naive-store::remove-data-object (item-collection item) item)
    ))

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
	     (wfx-query-context-data-object
	      (gethash :collection-name (cache *context*))
	      :query (lambda (item)				      
		      (string-equal
		       (frmt "~A" (item-hash item))
		       (frmt "~A" (second spliff)))))
	     selected)))))
    
    (setf (getcx (parameter "data-type") :selected-grid-items) selected)
    
    (cond ((string-equal (parameter "action-handler-lambda")
			 :delete-selected)
	   (delete-selected selected))
	  (t
	   (dolist (lambdax (getx (context-spec *context*) :lambdas))	     
	     (when (string-equal (digx lambdax :event)
				 :select-action-handler)
	       (eval% (digx lambdax :lamda :code))))))
    
    (setf (getcx (parameter "data-type") :selected-grid-items) nil)))



(defmethod action-handler ((action (eql :item-action)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (let* ((data-type (getcx (parameter "data-type") :data-type))
	 (item (wfx-query-context-data-object
		(gethash :collection-name (cache *context*))
		:query (lambda (item)				      
			(string-equal
			 (frmt "~A" (item-hash item))
			 (frmt "~A" (parameter "item-id")))))))
    

    (when item
      
      (dolist (action (getf data-type :item-actions))
	(when (string-equal (parameter "action-name") (getf action :name))

	  (funcall% (eval% (getf action :action))  item )
	  )))))

(defun getcx (&rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))
    
    (if indicators
	(progn
	  
	  (apply 'digx place indicators)) ;;digx also uses rest and that causes to many ()
	place)))

(defun (setf getcx) (value &rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))
    
    (if indicators
	(setf (gethash indicator (cache *context*)) 
	      (cl-naive-items::set-naive-dig place indicators value)) 
	(setf (gethash indicator (cache *context*)) value))))

(defun set-grid-search (data-type)
  (when (or (equalp (parameter "wfxaction") "filter")
	    (equalp (parameter "wfxaction") "un-filter"))
    
    (when (equalp (parameter "wfxaction") "filter")
      (if (empty-p (parameter "search"))
	  (setf (getcx data-type :search)
		(parameter "search"))))
    
    (when (equalp (parameter "wfxaction") "un-filter")
      (setf (getcx data-type :search) nil)))

  (unless (or (equalp (parameter "wfxaction") "filter")
	      (equalp (parameter "wfxaction") "un-filter"))
    (if (and (parameter "search") (not (empty-p (parameter "search"))))
	(setf (getcx data-type :search) (parameter "search"))
	(if (string-equal (parameter "search") "")
	    (setf (getcx data-type :search) nil)))))

(defun set-grid-filter (data-type)
  (when (equalp (parameter "wfxaction") "filter")
    (setf (getcx data-type :filter) t))
  
  (when (equalp (parameter "wfxaction") "un-filter")
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
    
    (unless (equalp (parameter "wfxaction") "save")
      (setf (getcx data-type :root-item) nil))))

(defun set-grid-expand ()  
  (when (equalp (parameter "wfxaction") "expand")
    (setf (getcx (parameter "data-type") :expand-id) (parameter "item-id")))
  
  (when (equalp (parameter "wfxaction") "unexpand")    
    (setf (getcx (parameter "data-type") :expand-id) nil)))



(defun render-grid-menu (collection-name data-type)
  (with-html-string
    (:div :class "navitem dropdown dropleft"
	  (:button :class "btn btn-sm dropdown-toggle"
		   :id "gridDropdown" 
		   :data-toggle "dropdown" 
		   :aria-haspopup="true"
		   :aria-expanded "false"
		   :text ""
		   (:strong "&#8942;"))
	  
	  
	  (:div :class "dropdown-menu p-2"
		:aria-labelledby "gridDropdown"
		(:table :class "table table-sm"
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"
			   :onclick (grid-js-render-new data-type nil)
			   
			   (:i	
			    :class "fa far fa-plus"))
			  (:td
			   :class "bg-light"
			   (:button
			    :class "btn btn-light w-100 text-left"
			    :name "new"
			    :type "button"	
			    :onclick (grid-js-render-new data-type nil)
			    (cl-who:str "Add New"))))))
		

		(:table :class "table table-sm"
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"

			   :data-toggle "collapse"
			   :href "#collapseFilter" 
			   :aria-expanded "false"
			   :aria-controls="collapseFilter"
			   :aria-pressed "false"
		       
			  
			   
			   (:i	
			    :class "fa far fa-filter"))
			  (:td
			   :class "bg-light"
			   (:button
			    :class "btn btn-light w-100 text-left"
			    :name "filter-grid"
			    :data-toggle "collapse"
			    :href "#collapseFilter" 
			    :aria-expanded "false"
			    :aria-controls="collapseFilter"
			    :aria-pressed "false"	
			   
			    (cl-who:str "Toggle Col Filter")))))
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"
			   :target "_blank"
			   :href (frmt "~A&collection=~A" 
					     (context-url 
					      "export")
					     collection-name )
			   
			   (:i	
			    :class "fa far fa-download"))
			  (:td
			   :class "bg-light"
			   (:a
			    :class "btn btn-light w-100 text-left"
			    :name "export"			    
			    :target "_blank"
			    :href (frmt "~A&collection=~A" 
					     (context-url 
					      "export")
					     collection-name )
			    (cl-who:str "Export")))))
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"
			   :target "_blank"
			   :href (frmt "~A&collection=~A&export-type=csv" 
					     (context-url 
					      "export")
					     collection-name )
			   
			   (:i	
			    :class "fa far fa-download"))
			  (:td
			   :class "bg-light"
			   (:a
			    :class "btn btn-light w-100 text-left"
			    :name "export"			    
			    :target "_blank"
			    :href (frmt "~A&collection=~A&export-type=csv" 
					     (context-url 
					      "export")
					     collection-name )
			    (cl-who:str "Export CSV")))))
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"
			   :target "_blank"
			   :href (frmt "~A&collection=~A&export-type=json" 
					     (context-url 
					      "export")
					     collection-name )
			   
			   (:i	
			    :class "fa far fa-download"))
			  (:td
			   :class "bg-light"
			   (:a
			    :class "btn btn-light w-100 text-left"
			    :name "export"			    
			    :target "_blank"
			    :href (frmt "~A&collection=~A&export-type=json" 
					     (context-url 
					      "export")
					     collection-name )
			    (cl-who:str "Export JSON")))))
			
			)
		
		(:div :class "dropdown-divider")
		
		(:table :class "table table-sm"
			(:tr
			 (:td
			  :class "bg-light"
			  :style "width:45px;"
			  (:a
       
			   :class "btn btn-light"
			   :style "width:45px;"
			   :onclick (grid-js-render-new data-type nil)
			   
			   (:i	
			    :class "fa far fa-plus"))
			  (:td
			   :class "bg-light"
			   (:button
			    :class "btn btn-light w-100 text-left"
			    :name "sanitize-collection"
			    :type "button"
			    ;;TODO: Figure out ajax call that does not render. Or give feeback stats.
			    :onclick (js-render "cl-wfx:ajax-sanitize-collection"
						(gethash :collection-name (cache *context*))	      
						(js-pair "data-type" (frmt "~A" data-type))
						(js-pair "wfxaction" "sanitize-collection"))
			    (cl-who:str "Sanitize Collection"))))))
		
		(:div :class "dropdown-divider")
	
		(:div :class "form-group"
		      (:label "Pages")
		      (:input :type "text" :name "pages"
			      :class "form-control"
			      :size 2
			      :id "pages"
			      :value (or (parameter "pages")
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
			       nil
			       (js-pair "data-type"
					(frmt "~A" data-type))	     
			       (js-pair "wfxaction" "grid-sizing"))))
		))))

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

     ;; (set-type-context (parameter "data-type"))

      (set-grid-search data-type)
	
      (set-grid-filter data-type)
	
      (set-grid-expand)
	
      (let ((page-items (fetch-grid-data data-type)))
	(with-html-string  
	  (:div
	   :class "card"
	   :style "min-width:255px;box-shadow: 0px 1px 2px;"

	   :id (gethash :collection-name (cache *context*))
	   
	   (:div :class "card-header"
		
		 (:div :class "row"
		       (:div :class "col-2"
			     (:h5
			      (cl-who:str (getx 
					   (context-spec *context*)
					   :name))))
		     
		       (:div :class "col"
			     (cl-who:str
			      (render-grid-search data-type)))
		  
		      
		       
		       (:div :class "col-2 "
			     (:table :class "grid-table-stuffx float-right"
			      (:tr
			       (:td
				(:button
				 :class "btn btn-sm btn-outline-success"
				 :name "new"
				 :type "submit" 
				 :aria-pressed "false"
				 :onclick 
				 (grid-js-render-new data-type nil)
				 (cl-who:str "+")))
			       (:td
				(cl-who:str (render-grid-menu collection-name data-type))))))))
	
		(:div :class "card-body  p-0 m-0" 		    
		      :id data-type
		      (:div :class "row"
			    (:div :class "col"
				  (:table
				   :class "table table-sm"
				   :style "width: 100%;"
						      
				   (:tbody
				    (cl-who:str
				     (render-grid-header data-type nil nil)))
				   (cl-who:str
				    (render-grid-data
				     data-type page-items 0 nil nil
				     nil))))))

		(:div :class "card-footer"
		      (:div :class "row"
			    (:div :class "col"
				  (:button 
				   :name "new" :type "submit" 
				   :class "btn btn-outline-success"
				   :aria-pressed "false"
				   :onclick 
				   (grid-js-render-new data-type nil)
				   (cl-who:str "+")))
			    (:div :class "col"
				  (cl-who:str
				   (render-select-actions
				    data-type)))))
		  
		(:div :class "card-footer"
		      (:div :class "row"	  
			    (:div :class "col"
				  (cl-who:str
				   (render-grid-paging data-type))))))
	  (:div
	   
	   (cl-who:str (gethash :context-lambda-event (cache *context*))))
	  (:div

	   (cl-who:str (gethash :context-lambda-code (cache *context*))))
	  (:div

	   (cl-who:str (gethash :context-lambda-result (cache *context*)))))))))



(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (render-grid (getx (context-spec *context*) :name)))

(defun ajax-sanitize-collection (&key id from-ajax)
  (declare (ignore from-ajax))
  (let* ((collection-name id)
	(stores (collection-stores *system* collection-name)))


    (dolist (store stores)
      (let ((collection (get-collection
				  store 
				  collection-name)))

	(when collection
	  (sanitize-data-file collection))))
    
    )
  
 
  (render-grid (getx (context-spec *context*) :name)))

(defun get-child (fields parent-item data-type hash)
  (dolist (field fields)
    (when (sub-grid-p field)
      (when (string-equal data-type (dig field :db-type :data-type))
	(dolist (item (getx parent-item (getf field :name)))
	  (when (string-equal (frmt "~A" (item-hash item))
			      (frmt "~A" hash))
	      (return-from get-child (list (getf field :name) item))))

	(return-from get-child (list (getf field :name)
				     (make-item :data-type
						(string-downcase
						 (frmt "~A" data-type)))))))))

(defun fetch-grid-root-edit-item (hash)
  (let ((collection-name (gethash :collection-name (cache *context*))))   
    (wfx-query-context-data-object
     collection-name
     :query (lambda (item)
	     (when (string-equal (frmt "~A" (item-hash item))
				 (frmt "~A" hash))
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
	     (list ))
	(cond ((equalp (complex-type field) :collection-contained-item)
	       
	       (setf list (fetch-contained-item-list field (getcx data-type :edit-item))))
	      
	      (t

	       (setf list (wfx-query-context-data		   
			   (dig field :db-type :collection)
			   :query (lambda (item)
				   
				    (and
				     (if (getf field :filter)
					 (funcall
					  (eval% (getf field :filter))
					  item
					  (getcx data-type :edit-item) )
					 t)
			     			  
				     (or
				      (string-equal (parameter
						     (frmt "~A-drop" field-name))
						    "")
				      (search (parameter
					       (frmt "~A-drop" field-name))
					      (accessor-value item accessors)
					      :test #'string-equal))))))))

	(with-html-string
	  (:div
	   :class "auto-complete-menu nav flex-column bg-white rounded border"
	   (setf list (sort (copy-list list) #'string<
			    :key (lambda (item)
				   (accessor-value item accessors))))

	   (setf list (append (list nil) list))
	   
	   (dolist (option list)
	     (cl-who:htm
	      (:span :class "auto-complete-item nav-link"
		     (:input :type "hidden"
			     :value (frmt "~A" (if option (item-hash option))))
		     (cl-who:str
		      (if option
			  (trim-whitespace
			   (accessor-value option accessors)))))))))))))

(defun ajax-auto-complete-x (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* (
	 (field-name (intern (parameter "field-name") :KEYWORD))
	 (collection (parameter "collection")))
 
    (let* ((accessors (list  field-name))
	   (list (wfx-query-context-data		   
		  collection
		  :query (lambda (item)
			  (or
			   (string-equal (parameter
					  (frmt "~A-drop" field-name))
					 "")
			   (search (parameter
				    (frmt "~A-drop" field-name))
				   (accessor-value item accessors)
				   :test #'string-equal))))))

      (with-html-string
	(:div
	 :class "auto-complete-menu nav flex-column bg-white rounded border"
	 (setf list (sort (copy-list list) #'string<
			  :key (lambda (item)
				 (accessor-value item accessors))))
	 (dolist (option list)
	   (cl-who:htm
	    (:span :class "auto-complete-item nav-link"
		   (:input :type "hidden"
			   :value (frmt "~A" (item-hash option)))
		   (cl-who:str
		    (trim-whitespace 
		     (accessor-value option accessors)))))))))))

(declaim (optimize (debug 3))
	 (notinline ajax-grid-edit))

(defun set-edit-objects ()
  (let* ((data-type (string-downcase (parameter "data-type")))
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

  (let* ((data-type (string-downcase (parameter "data-type")))
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
			      root-item root-type
			      (reverse edit-objects)))
	  (progn
	    
	    (setf (getcx data-type :edit-object) edit-objects)
	    (render-grid-edit root-type fields root-item nil nil
				     (list (list :data-type data-type
						 :item root-item))))))))

(defun index-keys-x (fields item-values)
  (let ((keys))
    (dolist (field fields)
      (when (getf field :key-p)
	(if (item-p (getf item-values (getf field :name)))
	    (push (item-hash (getf item-values (getf field :name))) keys)
	    (push (getf item-values (getf field :name)) keys))))
    (reverse keys)))

(defun move-uploaded-file (fields edit-item)
  (let ((hash (item-hash edit-item))
	(collection (wfx-get-collection
		     (gethash :collection-name (cache *context*)))))

    (unless hash
      (let* (
	    (hashx (uuid:make-v4-uuid)))
	(setf hash hashx)))
    
    (dolist (field fields)

      (when (or
	     (equalp (complex-type field) :image)
	     (equalp (complex-type field) :file))
	(let* ((server-path
	       (string-downcase
		(frmt "~A/files/~A/~A/"
		      (if (location collection)
			  (location collection)
			  (string-downcase
			   (frmt "~A~A"
				 (location (store collection))
				 (name collection))))
		      (parameter "data-type")
		      (getf field :name))))
	       (file-name (sanitize-file-name
			   (parameter (string-downcase
				       (frmt "~A" (getf field :name))))))
	      (temp-path  (merge-pathnames				 
			   file-name
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
		  (getx edit-item (getf field :name))))
	  
	  (when (probe-file temp-path)
	    (fad:copy-file
	     temp-path
	     (merge-pathnames file-name
			      server-path)
	     :overwrite t)
	    (delete-file temp-path)
	    (setf (getx edit-item (getf field :name))
		  (parameter (string-downcase
			      (frmt "~A" (getf field :name)))))))))))

(defun find-contained-item (hash list)
  (dolist (item list)
    (when (string-equal (frmt "~A" (item-hash item))
			(frmt "~A" hash))
      (return-from find-contained-item item))))


(defun synq-value (field edit-item parent-item value)
  (cond ((equalp (complex-type field) :collection)
	 (setf (getfx edit-item field)
	       (wfx-query-context-data-object
		(dig field :db-type :collection)
		:query (lambda (item)
			(string-equal (frmt "~A" (item-hash item))
				      (frmt "~A" value))))))
	
	((equalp (complex-type field) :collection-contained-item)
	 (setf (getfx edit-item field)
	       (find-contained-item
		value
		(fetch-contained-item-list
		 field
		 edit-item))))
	
	((equalp (complex-type field) :contained-item)
	 (setf (getfx edit-item field)
	       (find-contained-item
		value
		(accessor-value parent-item
				(digx field :db-type :container-accessor)) )))
	((equalp (complex-type field) :item)
	 (let* ((sub-data-type (digx field :db-type :data-type))
		(more-item (or (getfx edit-item field)
			       (make-item :data-type
					  sub-data-type))))
	   (synq-item-values sub-data-type
			     (getcx sub-data-type :fields)
			     edit-item
			     more-item)

	   (setf (getfx edit-item field) more-item)))
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
	      :items (wfx-query-context-data
		      (dig field :db-type :collection)
		      :query (lambda (item)
			      (string-equal (frmt "~A" (item-hash item))
					    (frmt "~A" value))))))
	    
	    ((equalp (complex-type field) :collection-contained-item)
	     (validate-sfx (complex-type field)
			   field 
			   edit-item 
			   (parameter field-name)
			   :items
			   (fetch-contained-item-list field edit-item )
			   
			   ))

	    
	    ((equalp (complex-type field) :contained-item)
	     (validate-sfx (complex-type field)
			   field 
			   edit-item 
			   (parameter field-name)
			   :items
			   (accessor-value parent-item
				(digx field :db-type :container-accessor))
			   ))
	    ((getf field :validation)
	     (if (functionp (getf field :validation))
		 (funcall (eval% (getf field :validation)) edit-item value)))
	    (t
	     (list t nil)))
      (list t nil)))

(defun synq-item-values (data-type fields parent-item edit-item)
  (dolist (field fields)

    (when (and (digx field :attributes :editable)
	       (getf field :db-type)
	       (not (sub-grid-p field)))

      (let* ((field-name (frmt "~A" (getf field :name)))
	     (valid (if (equalp (complex-type field) :item)
			(validate-value field
					field-name 
					edit-item
					parent-item
					(or
					 (parameter (string-downcase field-name))
					 (parameter field-name)))
			(list t nil))))
	      
	(unless (first valid)
	  
	  (pushnew 
	   (list field-name (second valid))
	   (getcx data-type :validation-errors)))
	      
	(when (first valid)
	  
	  (synq-value field edit-item parent-item
		      (or (parameter (string-downcase field-name))
			  (parameter field-name))))))	  
  
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
	    edit-item		  
	    (setf (getx parent-item parent-slot)
		  (append (getx parent-item parent-slot)
			  (list edit-item))))))))

(defun grid-persist-object (data-type root-item)
  (unless (getcx data-type :validation-errors)
    (let ((collection (wfx-get-collection
		       (gethash :collection-name (cache *context*)))))
    
      (setf (getx root-item :user) (getx (current-user) :email))
      (unless collection
	(pushnew 
	 "No default store found check if license is selected."
	 (getcx data-type :validation-errors)))

      (when collection
	(setf (item-collection root-item) collection)
	(setf (item-store root-item) (store collection))
	(persist-object collection root-item :allow-key-change-p t)))))

(defun prepare-edit-objects ()
  (let* ((data-type (string-downcase (parameter "data-type")))
	(edit-objects (reverse (set-edit-objects)))
	(collection (wfx-get-collection
		     (gethash :collection-name (cache *context*))))
	
	(root-item)
	(parent-item)
	(edit-item)
	(parent-slot))
      
    (when edit-objects
      
      (setf root-item (getf (first edit-objects) :item))

      ;;Create new item if saving item higher up in the store
      ;;hierarchy
      (when (item-store root-item)
	(unless (equalp (store collection)
			(item-store root-item))
	  (setf root-item (make-item :data-type (item-data-type root-item)
				     :collection collection
				     :values (item-values root-item)
				     :changes (item-changes root-item)))
	  (setf (getf (first edit-objects) :item) root-item)))

      
      (when (> (length edit-objects) 1)
	(setf edit-item (getf (first (last edit-objects)) :item))
	(setf parent-slot (getf (first (last edit-objects)) :field-name))
	(setf parent-item
	      (getf (nth (- (length edit-objects) 2) edit-objects) :item)))
      
      ;;Create new item if saving item higher up in the store
      ;;hierarchy
      (when (and edit-item (item-store edit-item))
	(unless (equalp (store collection)
			(item-store edit-item))
	  (setf edit-item (make-item :data-type data-type
				     :collection (wfx-get-collection
						  (name (item-collection
							 edit-item)))
				     :values (item-values edit-item)
				     :changes (item-changes edit-item)))
	  (setf (getf (first (last edit-objects)) :item) edit-item)

	  (let ((clean-list (getxo parent-item parent-slot)))
	    
	    (dolist (item clean-list)
	      (when (string-equal (frmt "~A" (item-hash item))
				  (frmt "~A" (parameter "item-id")))
		
		(setf clean-list (remove item clean-list))
		(setf clean-list (pushnew edit-item clean-list))))
	    (setf (getx parent-item parent-slot) clean-list))))

      (unless (> (length edit-objects) 1)
	(setf edit-item root-item)))

    (values root-item parent-slot parent-item edit-item)))


(defun fire-context-event (context event root-item edit-item)
  (let ((context-spec (context-spec context)))
    (when (getx context-spec :lambdas)
      (dolist (lambdax (getx context-spec :lambdas))
	(when (find event (getx lambdax :events) :test 'equalp)
	  (let* ((code (digx lambdax :lambda :code))
		 (result (eval%
			  `(let ((*root-item* ,root-item)
				 (*edit-item* ,edit-item))
			     ,code))))
	    (setf (gethash :context-lambda-code (cache *context*))
		  code)
	    (setf (gethash :context-lambda-event (cache *context*))
		  event)
	    (setf (gethash :context-lambda-result (cache *context*))	   
		  (or (first result) (second result)) )
	    ))))))

(defmethod action-handler ((action (eql :save)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)


  (let* ((data-type (string-downcase (parameter "data-type")))
	 (fields (getcx data-type :fields)))

    (setf (getcx data-type :validation-errors) nil)
    
    (multiple-value-bind (root-item parent-slot parent-item edit-item)
	(prepare-edit-objects)

      (when fields
	
	(unless edit-item
	  (setf edit-item (make-item :data-type data-type)))

	
	(when (dig (getcx data-type :data-type) :data-type :client-validation)
	    (let ((valid (funcall (eval% (dig (getcx data-type :data-type) :data-type :client-validation))
				  root-item
				  edit-item)))
	      (unless (first valid)
		
		(pushnew 
		 (list data-type (second valid))
		 (getcx data-type :validation-errors)))))

	(unless (getcx data-type :validation-errors)

	  
	  (synq-item-values data-type fields parent-item edit-item)

	  
	  (move-uploaded-file fields edit-item)   
	  
	  (grid-append-child data-type parent-slot parent-item
			     edit-item)

	  
	  (grid-persist-object data-type root-item)

	  (fire-context-event context :save root-item edit-item))
	

	))))


(defmethod action-handler ((action (eql :delete)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (multiple-value-bind (root-item parent-slot parent-item edit-item)
	(prepare-edit-objects)
   
      (when (and edit-item parent-slot)
	(let ((clean-list (getx parent-item parent-slot)))
	  (dolist (item clean-list)
	    (when (string-equal (frmt "~A" (item-hash item))
				(frmt "~A" (parameter "item-id")))
	      (setf clean-list
		    (remove item clean-list))
	      (let* ((collection (wfx-get-collection
		       (gethash :collection-name (cache *context*))))
		     (server-path
		      (string-downcase
		       (frmt "~A/files/~A/"
			     (if (location collection)
				 (location collection)
				 (string-downcase
				  (frmt "~A~A"
					(location (store collection))
					(name collection))))
			     (parameter "data-type")))))
		
		(dolist (field (getcx (parameter "data-type") :fields))
		  (when (or (equalp (simple-type field) :image)
			    (equalp (simple-type field) :file))
		    (delete-file
		     (string-downcase
		      (frmt "~A~A/~A" server-path
			    (getf field :name)
			    (sanitize-file-name
			     (getx edit-item (getf field :name)))))))))))
	  
	  (setf (getx parent-item parent-slot) clean-list)
	  
	  (persist-object (item-collection root-item) root-item)))

      (unless (and edit-item parent-slot)
	(setf (item-deleted-p root-item) t)
	(persist-object (item-collection root-item) root-item)
	(cl-naive-store::remove-data-object (item-collection root-item) root-item))))
