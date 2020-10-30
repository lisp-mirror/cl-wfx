(in-package :cl-wfx)

;;TODO: Move this to theme
(defparameter *limit-columns* 7)


(defun complex-type (element)
  (if (listp (digx element :type-def))
      (or (digx element :type-def :complex-type)
	  (digx element :type-def :type))
      (digx element :type-def)))

(defun simple-type (element)
  (if (listp (digx element :type-def))
      (digx element :type-def :type)
      (digx element :type-def)))

(defun entity-type-p (elements)
  (dolist (element elements)
    (when (equalp (getx element :name) :entity)
      (return-from entity-type-p t))))

(defun check-top-level-p (document-type)  
  (when document-type
    (when (getcx document-type :type-def)
      (digx (getcx document-type :type-def) :top-level-p))))

(defun sub-grid-p (element)
  (find (complex-type element)
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



(defmethod render-input-val ((type (eql :value-list)) element document
			     &key parent-document hierarchy &allow-other-keys)
  (let* ((name (getx element :name))
	 (list (or
		(and (digx element :type-def :elements-lambda)
		     (eval% (digx element :type-def :elements-lambda)))

		(digx element :type-def :elements)))
	  
	 (selected ))
    
    (when (functionp list)      
	(setf list (funcall (eval% (digx element :type-def :elements-lambda)) document parent-document hierarchy)))

  
    (setf selected (find (getx document element) list :test #'equalp))
    (with-html-string
      (cl-who:str (render-dropdown name selected list)))))

;;delete this
(defmethod render-input-val-x ((type (eql :value-list)) element document
			       &key &allow-other-keys)
  (let* ((name (getx element :name))
	 (list (or (and (digx element :type-def :elements-lambda)
			(cond ((equalp (first (digx element :type-def :elements-lambda))
				       'cl-wfx::get-named-list-sorted-values)
			       (eval% (digx element :type-def :elements-lambda)))
			      ((equalp (first (digx element :type-def :elements-lambda))
				       'cl:lambda)
			       (break "render-input-val-x ??? value-lambda")
			       )))
		   (digx element :type-def :elements)))
	 (selected (find (getx document element) list :test #'equalp)))

    (with-html-string
      (cl-who:str (render-dropdown name selected list)))))

(defmethod render-input-val ((type (eql :hierarchical)) element document 
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :collection-objects))
			     element document &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :list-objects)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :document)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defun render-document-list-auto-complete-x (collection document-type element-name selected
			&key  select-prompt
			  value-func
			  context-state-selected)
  (let ((selected-value (if selected
			    (if value-func
				(funcall value-func selected)
				selected)
			    (or (parameter (frmt "~A-drop" element-name))
				context-state-selected))))
    (with-html-string
      (:div :class "col"
	    
       (:div :class "auto-complete"
	     (:input :type "hidden" :class "selected-value" 
		     :name (frmt "~A" element-name)
		     :value (html-value (or
					 (parameter (frmt "~A" element-name))
					 selected-value
					 "")))
	     
	     (:input :class "form-control auto-complete-text"
		     :type "text"
		     :autocomplete "off"
		     :placeholder
		     (or select-prompt
			 "Press Ctrl for list or start typing and then press Ctrl for list...")
		     :name (frmt "~A-drop" element-name) 
		     :id (frmt "~A-drop" element-name)
		     :value (html-value (or selected-value ""))
		     :onkeydown
		     ;;fires ajax call on Ctrl (13)
		     (js-render-event-key 
		      (frmt "~A-drop" element-name)
		      17
		      "cl-wfx:ajax-auto-complete-x"
		      (frmt "~A-drop-div" element-name)
		      nil
		      (js-pair "document-type"
			       document-type)
		      (js-pair "element-name"
			       (frmt "~A" element-name))
		      (js-pair "collection"
			       (frmt "~A" collection))
		      (js-pair "wfxaction" "auto-complete")))
	     
	     (:div :id (frmt "~A-drop-div" element-name) :class "auto-list"))))))


(defun render-document-list-auto-complete (document-type element document selected
				       &key  select-prompt
					 value-func
					 context-state-selected
					 required-p
					 hierarchy)
  (declare (ignore hierarchy) (ignore document))

  (let* ((element-name (digx element :name))
	(selected-value (if selected
			    (if value-func
				(funcall value-func selected)
				selected)
			    (or (parameter (frmt "~A" element-name))
				context-state-selected))))
    
    (with-html-string
      (:div :class "col"
	    
	    (:div :class "auto-complete"
		  (:input :type "hidden" :class "selected-value" 
			  :name (frmt "~A" element-name)
			  :value (html-value (or
					      (parameter (frmt "~A" element-name))
					      (if (document-p selected)
						  (document-hash selected)
						  selected-value)
					      "")))
		  
		  (:input :class "form-control auto-complete-text"
			  :type "text"
			  :autocomplete "off"
			  :placeholder
			  (or select-prompt
			      "Press Ctrl for list or start typing and then press Ctrl for list...")
			  :name (frmt "~A-drop" element-name) 
			  :id (frmt "~A-drop" element-name)
			  :value (html-value (or selected-value ""))
			  :required (if required-p
					"required")
			  :onkeydown
			  ;;fires ajax call on Ctrl (17)
			  (js-render-event-key 
				 (frmt "~A-drop" element-name)
				 17
				 "cl-wfx:ajax-auto-complete"
				 (frmt "~A-drop-div" element-name)
				 (string-downcase
				  (frmt "grid-edit-~A"  document-type))
				 (js-pair "document-type"
					  document-type)
				 (js-pair "element-name"
					  (frmt "~A" element-name))
				 (js-pair "wfxaction" "grid-auto-complete")))
		  
		  (:div :id (frmt "~A-drop-div" element-name) :class "auto-list"))))))

(defun accessor-value (document accessors)
  (let ((value))
    (if accessors
	(if (listp accessors)
	    (if (listp (first accessors))
		(let ((values))
		  (dolist (accessor accessors)
		    (push
		     (apply #'digx 
			     document
			     accessor)
		     values))
		  (setf value (format nil
				      "~{~a~^ ~}"
				      (reverse values))))
		(setf value (apply #'digx 
				   document
				   accessors)))
	    (setf value (apply #'digx 
				document
				(list accessors))))
	document)))

(defun fetch-contained-document-list (element edit-document)
  
  (let ((list-containers
	 (wfx-query-context-data
	  (digx element :type-def :collection)
	  :query (lambda (documentx)
		  (if (getx element :filter)
		      (funcall
		       (eval% (getx element :filter))
		       documentx
		       edit-document
		       (getcx (digx element :type-def :type-def) :edit-object))
		      t))))
	      
	(container-accessors (digx element :type-def :container-accessor))
	(accessors (digx element :type-def :accessor))
	(list))

    
    (when (not (digx element :type-def :container-fetch))
      
      (dolist (list-document list-containers)
	
	(setf list (pushnew (accessor-value list-document container-accessors) list))))


    (when (digx element :type-def :container-fetch)
      
      (dolist (list-document list-containers)
	(setf list
	      (append list (funcall
			    (eval% (digx element :type-def :container-fetch))
			    (accessor-value list-document container-accessors)
			    edit-document
			    (getcx (digx element :type-def :type-def) :edit-object))))))

    (sort (copy-list list) #'string<
	  :key (lambda (document)
		 (accessor-value document accessors)))))


(defmethod render-input-val ((type (eql :collection-contained-document)) element document 
			     &key document-type hierarchy &allow-other-keys)

  (let* ((name (getx element :name))
	
	 (list (fetch-contained-document-list element document))
	 (selected)
	 (accessors (digx element :type-def :accessor)))

    (setf selected (find (getx document name)  
			 list :test #'equalp))
    
    
    (with-html-string
      (cl-who:str (render-document-list-auto-complete
		   document-type element document selected
		   :value-func (lambda (document)
				 (accessor-value document accessors))
		   :context-state-selected (getcx 
					    (digx element :type-def :type-def)
					    (frmt "~A-drop" name))
		   :required-p (getx element :key-p)
		   :hierarchy hierarchy)))))


(defun find-in-hierarchy (document-type hierarchy)
  (dolist (hierarchy-document hierarchy)
    (when (string-equal (getx hierarchy-document :type-def)
			document-type)
      (return-from find-in-hierarchy (getx hierarchy-document :document)))))

(defmethod render-input-val ((type (eql :collection)) element document 
			     &key document-type hierarchy &allow-other-keys)

  (let* ((name (getx element :name))
	 (list (wfx-query-context-data
		(digx element :type-def :collection)
		:query (lambda (documentx)
			(if (getx element :filter)
			    (funcall
			     (eval% (getx element :filter))
			     documentx
			     document
			     hierarchy)
			    t))))
	 (selected (find (getx document name)  
			 list :test #'equalp))
	 (accessors (digx element :type-def :accessor)))
    
    (with-html-string
      (cl-who:str (render-document-list-auto-complete
		   document-type element document selected
		   :value-func (lambda (document)
				 (accessor-value document accessors))
		   :context-state-selected (getcx 
					    (digx element :type-def :type-def)
					    (frmt "~A-drop" name))
		   :required-p (getx element :key-p)
		   :hierarchy hierarchy
		    )))))

(defmethod render-input-val ((type (eql :contained-document)) element document 
			     &key parent-document &allow-other-keys)
  (declare (ignore parent-document))
  (let* ((name (getx element :name))
	 (list (apply #'digx 
		      (getx
		       (second (getcx (parameter "document-type") :edit-object))
		       :document)
		      (digx element :type-def :container-accessor)))
	 (selected (find (getx document name)  
			 list :test #'equalp))
	 (accessors (digx element :type-def :accessor)))

    (with-html-string
      (cl-who:str
       (render-dropdown name selected list
			:key-func 'cl-naive-documents:document-hash
			:value-func (lambda (document)
				      (accessor-value document accessors)))))))

(defun grid-js-render-form-values (document-type form-id 
				   &key action action-lambda
				     action-data document-id
				     hierarchy)
  (let ((active-page (getcx 
		      document-type :active-page)))
   

    (js-render-form-values 
     "cl-wfx:ajax-grid"
     (gethash :collection-name (cache *context*))
     form-id
     (js-pair "document-type"
	      (frmt "~A" document-type))
     
     (js-pair "wfxaction" (or action ""))

     (js-pair "action-lambda" (or action-lambda ""))
     (js-pair "action-data" (or action-data ""))
     
     (js-pair "document-id" (frmt "~A" (or document-id (getcx document-type :document-id)
				       "")))
     (js-pair "document-hierarchy"
	      (hierarchy-string hierarchy))
     (js-pair "pages"
	      (or (parameter "pages") 50))
     (js-pair "page"
	      (or active-page 1)))))

(defun grid-js-render-file-upload (document-type form-id row-id element-name)
  (js-render-form-values 
     "cl-wfx:ajax-render-file-upload"
     row-id
     form-id
     (js-pair "document-type"
	      (frmt "~A" document-type))
     (js-pair "element-name" element-name)
     (js-pair "wfxaction" "upload-file")))

(defun grid-js-render (document-type &key action document-id)
  (let ((active-page (getcx document-type :active-page)))

    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "document-type" (frmt "~A" document-type))
	       
	       (js-pair "wfxaction" (or action ""))
	       
	       (js-pair "document-id" (frmt "~A" (or document-id "")))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-expand-buttons (subs document-type document)
  (if subs
      (if (string-equal
	  (frmt "~A" (getcx document-type :expand-id)) 
	  (frmt "~A" (document-hash document)))
	  (with-html-string
	    (:button
	     :name "expand" :type "submit" 
	     :class "btn btn-sm border-0 active"
	     :aria-pressed "true"
	     :onclick (grid-js-render document-type
				      :action "unexpand")
	     (:i :class "fa far fa-chevron-circle-up fa-lg  text-secondary")
	     
	     ))
	  (with-html-string
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-sm border-0"
	     :aria-pressed "false"
	     :onclick (grid-js-render document-type
					    :action "expand"
					    :document-id (document-hash document))
	     (:i :class "fa far fa-chevron-circle-right fa-lg text-light"))))))

(defun render-select-button (document)
  (with-html-string
    (:div :class "form-check"
	  (:input
	   :class "form-check-input grid-selection"
	   ;; :style "height:15px;width:15px;"
	   :type "checkbox"
	   :id "grid-selection"
	   :name "grid-selection"
	   :value (frmt "~A" (document-hash document))	    
	   :aria-label "Select Row"))))

(defun grid-js-render-delete (document-type &key document hierarchy)
  (let ((active-page (getcx document-type :active-page)))

    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "document-type" (frmt "~A" document-type))
	       
	       (js-pair "wfxaction" "delete")
	       
	       (js-pair "document-id" (frmt "~A" (or (document-hash document) "")))
	       (js-pair "document-hierarchy"
			(hierarchy-string hierarchy))

	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun grid-js-render-action (document-type action &key document)
  (let ((active-page (getcx document-type :active-page)))
    (js-render "cl-wfx:ajax-grid"
	       (gethash :collection-name (cache *context*))	      
	       (js-pair "document-type" (frmt "~A" document-type))
	       
	       
	       (js-pair "wfxaction" "document-action")
	       (js-pair "action-name" (string-downcase (frmt "~A" (or action ""))))

		       
	       (js-pair "document-id" (frmt "~A" (or (document-hash document) "")))


	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun hierarchy-string (documents)
  (let ((hierarchy ""))
     (dolist (document documents)
      (setf hierarchy  (concatenate 'string hierarchy " "
				    (frmt "(~A ~A)"
					  (getx document :type-def)
					  (if (getx document :document)
					      (if (document-p (getx document :document))
						  (document-hash (getx document :document))
						  (getx document :document)
						  )
					      )))))
    (frmt "(~A)" hierarchy)))

(defun grid-js-render-edit (document-type &key action document hierarchy)
  (let ((active-page (getcx document-type :active-page)))
    
       (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-edit-~A" (document-hash document))	      
	       (js-pair "document-type" (frmt "~A" document-type))
	       
	       (js-pair "wfxaction" (string-downcase (frmt "~A" (or action ""))))
	     
	       
	       (js-pair "document-id" (frmt "~A" (or (document-hash document) "")))
	       (js-pair "document-hierarchy"
			(hierarchy-string hierarchy))
	       (js-pair "pages"
			(or (parameter "pages") 50))
	       (js-pair "page"
			(or active-page 1)))))

(defun render-grid-buttons (document-type document hierarchy)
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
		 (if (and (parameter "document-id")
			  (string-equal 
			   (parameter "document-id") 
			   (frmt "~A" (document-hash document))))
		     "true"
		     "false")
		 :onclick
		 (frmt "~A;toggle_tbody(\"ajax-edit-~A\");toggle_tbody(\"ajax-editing-row-~A\");toggle_tbody(\"ajax-expand-row-~A\");"
		       (grid-js-render-edit document-type
					    :action "edit"
					    :document document
					    :hierarchy hierarchy)
		       (document-hash document)
		       (document-hash document)
		       (document-hash document)))))
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
		       (grid-js-render-delete document-type
					      :document document
					      :hierarchy hierarchy)))))))
      
      

      (dolist (action (getx (getcx document-type :type-def) :document-actions))
	(cond ((equalp (getx action :type) :button)
	       (cl-who:htm
		(:button
		 :name (getx action :name)
		 :type "submit"
		 :onclick
		 
		 (if (getx action :confirmation)
		     (frmt "~A;alert(\"~A\")"
			   
			   (grid-js-render-action document-type
						  (getx action :name)
						  :document document)
			   (getx action :confirmation))
		     (grid-js-render-action document-type
						  (getx action :name)
						  :document document))
		 
		 (cl-who:str (getx action :label))
		 )))
	      ((equalp (getx action :type) :link)
	       (cl-who:htm
		(:a :class "btn"  :role "button"
		    :target "_blank"
		    :href (cl-who:str (apply%
				       (eval%
					(getx action :action))
				       (list document
					     (getcx document-type :parent-document))))
		    (cl-who:str (getx action :label))))

	       ))
	)
      )))

(defun render-edit-buttons (document-type hierarchy)
  (with-html-string
    
    (when (user-context-permission-p
	   (getx (context-spec *context*) :name)
	   :update)
      (cl-who:htm
       (:i :name "save" 				   	 
	   :class "fa fa-save fa-2x text-success"
	   :onclick
	   (frmt "if(document.getElementById(\"grid-edit-~A\").checkValidity()) {~A}else console.log(\"invalid form\");"
		 document-type
		 (grid-js-render-form-values			 
		  document-type
		  (string-downcase
		   (frmt "grid-edit-~A"  document-type))
		  
		  :action "save"
		  :hierarchy hierarchy)))))
    (:i :name "cancel" 				   
	:class "fa fa-eject fa-2x text-dark"
	:onclick (frmt "~A;toggle_display(\"ajax-edit-row-~A\");"
		       (grid-js-render document-type
				       :action "cancel")
		       (document-hash (getcx document-type :edit-document))))))


(defun render-grid-edit-row (document-type name element label document parent-document hierarchy)
  
  (with-html-string
   (:div
    :class
    (if (digx  element
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
       (complex-type element) 
       element document
       :parent-document
       parent-document
       :document-type document-type
       :hierarchy hierarchy))))))


(defun render-grid-edit-more (element document hierarchy)
  (with-html-string
    (let* ((sub-document-type
	    (digx  element :type-def
		  :type-def))
	   (sub-elements
	    (getcx sub-document-type :elements)))

      (unless sub-elements
	(set-type-context (getx sub-document-type :collection)  sub-document-type)
	(setf sub-elements
	      (getcx sub-document-type :elements)))
      
      (cl-who:htm
       (:div
	:class "row"

	(:div :class "col"
	      (:button :class "btn btn-primary"
		       :type "button"
		       :data-toggle "collapse"
		       :data-target (frmt "#~A-more" sub-document-type)
		       :area-expanded "false"
		       :area-controls (frmt "~A-more" sub-document-type)
		       (cl-who:str (getx element :label)))
	      ))
  
       (let ((sub-document (getx document (getx element :name))))
	 (unless sub-document
		(setf sub-document (make-document :type-def sub-document-type)))
	 (cl-who:htm
	  (:div
	   :class "row collapse" :id (frmt "~A-more" sub-document-type)
	   (:div :class "col" 
		 (dolist (sub-element sub-elements)
		   (unless (sub-grid-p sub-element)
		     (cl-who:str
		      (render-grid-edit-row
		       sub-document-type
		       (getx sub-element :name)
		       sub-element
		       (getx sub-element :label)
		       sub-document
		       document
		       hierarchy)))
		   
		   )))))
       ))))

(defun render-grid-edit (document-type elements document parent-document
			 parent-spec hierarchy)
  (setf (getcx document-type :edit-document) document)
  (setf (getcx document-type :parent-spec) parent-document)
  (setf (getcx document-type :parent-spec) parent-spec)
  (setf (getcx (parameter "document-type") :document-id)
	(if document
	    (document-hash document)
	    nil))

  (let ((header-elements (get-header-elements elements))
	(sub-elements (get-sub-elements elements)))
    (with-html-string
      
      (:tbody :id (frmt "ajax-edit-~A"
			(if document
			    (document-hash document)
			    nil))	
	      (:tr :class "bg-light"

		   (if sub-elements
		       (cl-who:htm
			(:td :style "width: 25px;" )))
	
		   (dolist (element (limit-elements (get-data-elements
						 header-elements)
						parent-document))	  
		     (cl-who:str (render-table-cell element document)))

	

		   (:td 
			(:div
			 (cl-who:str (render-edit-buttons document-type hierarchy)))))
       
	      (:tr 
	       (:td :colspan (if sub-elements
				 (+ (length (limit-elements header-elements
							  parent-document))
				    2)
				 (+ (length (limit-elements header-elements
							  parent-document))
				    1))		 
		    (:form :class "card"
			   :style "border-left-style: dotted;border-width:3px; border-left-color:#F1948A;min-width:255px;max-width:99%"
			   :id (string-downcase (frmt "grid-edit-~A"  document-type))

			   (:div
			    :class "card-body p-0 m-0"

			    (:div :class "row" 
				  :id (frmt "~A" document-type)
				  (:div :class "col"
					
					(dolist (element elements)
					  (let* ((name (getx element :name))
						 (label (getx element :label)))
					
					    (when (and (digx  element :attributes :display) 
						       (not (sub-grid-p element)))

					      (unless (equalp (complex-type element)
							      :document)
                                        
						(cl-who:str
						 (render-grid-edit-row
						  document-type name
						  element label
						  document parent-document
						  hierarchy))))))))
		    
			    (dolist (element elements)
			      (when (and (digx  element :attributes :display) 
					 (not (sub-grid-p element)))

				(when (equalp (complex-type element)
					      :document)
			  
				  (when (document-type-access-p (digx  element :type-def :type-def))
				    (cl-who:str
				     (render-grid-edit-more element document hierarchy)))))))

			   
			   (when (getcx document-type :validation-errors)
			     (let ((errors (getcx document-type :validation-errors)))
			       (setf (getcx document-type :validation-errors) nil)
			       (cl-who:htm
				(:div :class "card-footer"
				      (:div :class "row"
					    (:div :class "col text-danger"
						  (:strong
						   (cl-who:str
						    (frmt "Errors ~S"
							  errors))))))))))))))))

(defun render-grid-col-filter (document-type col-name)
   (with-html-string
    (:input :class "w-100"
	    :type "text" 
	    :name (frmt "~A-filter" col-name) 

	    :id (frmt "~A-filter" col-name)
	    :value (or (parameter (frmt "~A-filter" col-name))
		       (getcx document-type
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
	     (js-pair "document-type"
		      (frmt "~A" document-type))
	     (js-pair "wfxaction" "grid-col-filter")))))

(defun limit-elements (elements hierarchy)
  (if (> (length elements) 1)
      (if (and (not (document-p hierarchy))
	       (>= (length hierarchy) 1))
	  (if (> (length elements) 3)
	      (subseq elements 0 3)
	      elements)
	  (if (> (length elements) 7)
		  (subseq elements 0 7)
		  elements))
      elements))

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

(defun get-sub-elements (elements)
  (let ((sub-elements))
    (dolist (element elements)
      (when (and
	     (sub-grid-p element)
	     )
	(pushnew element sub-elements)))
    (reverse sub-elements)))

(defun get-header-elements (elements)
  (let ((header-elements))
    (dolist (element elements)
      (when (and (digx  element :attributes :display)
		 (not (sub-grid-p element)))
	(pushnew element header-elements)))
    (reverse header-elements)))

(defun render-filter-row (document-type elements subs)
  (let* ((search-term (or (parameter "search") 
			  (getcx 
			   document-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (or (getcx document-type :filter)
		       (getcx document-type :filter-elements))))
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
	   
	   
	   (dolist (element (limit-elements (get-header-elements elements)
					nil))
		 (let* ((name (getx element :name)))
						
		   (cl-who:htm
		    (:th
			  (cl-who:str
			   (render-grid-col-filter 
			    document-type name))))))
	   (:th)))))

(defun limit-sub-elements (elements)
  (if (> (length elements) 2)
      (subseq elements 0 2)
      elements))

(defun render-header-row (document-type elements subs hierarchy)
  (let ((header-elements (limit-elements (get-header-elements elements)
				     hierarchy)))
    (with-html-string
      
      (:tr :class "bg-light"
	   :style "box-shadow: 0px 2px 2px;"
	   (if subs
	       (cl-who:htm (:th :style "width:25px;")))
	   
	   (dolist (element header-elements)
	     (let* ((name (getx element :name))
		    (label (getx element :label)))
	       
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
	   
	   (when (check-top-level-p document-type)
	     
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

(defun render-grid-header (document-type sub-p hierarchy)
  (let ((elements (getcx	document-type :elements))
	(subs))

    (dolist (element elements)
      (when (sub-grid-p element)
	(pushnew element subs)))
    
    (with-html-string
      (unless sub-p
	(cl-who:str
	       (render-filter-row document-type elements subs)))
      
      (cl-who:str (render-header-row document-type elements subs hierarchy)))))

(defun get-data-elements (elements)
  (let ((data-elements))
    (dolist (element elements)
      (when (and (digx  element :attributes :display)
		 (not (sub-grid-p element)))
	(pushnew element data-elements)))
    (reverse data-elements)))


(defun render-select-actions (document-type)  
  (let ((action-list))

    (dolist (lambdax (getx (context-spec *context*) :lambdas))
      (when (find :select-action-list (getx lambdax :events) :test 'equalp)
	(setf action-list (eval% (digx  lambdax :lambda :code)))))

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
				(getx action :action-name))
		  :value-func (lambda (action)
				(getx action :action-name))
		  :select-onclick-func
		  (lambda (action)
		    (js-render-form-values
		     "cl-wfx:ajax-grid"
		     (gethash :collection-name
			      (cache *context*))
		     (frmt "~A" document-type)
		     (js-pair "wfxaction"
			      "grid-select-action")
		     
		     (js-pair "action-data"
			      (or (getx action :data) ""))
		     (js-pair "action-handler-lambda"
			      (frmt "~A" (getx action :handler-lambda)))
		     (js-pair "pages"
			      (or (parameter "pages") 50))
		     (js-pair "page"
			      (or (getcx 
				   document-type :active-page)
				  1)))))))))))


(defvar *rendering-shit* nil)

(defun keysx (elements)
  (let ((keys))   
    (dolist (element elements)
      (when (getx element :key-p)
	(setf keys
	      (append keys
		      (list
		       (list
			:name (getx element :name)
			:accessor (if (sub-grid-p element)
				      (digx element :type-def :accessor)
				      (if (find (complex-type element)
						(list :collection
						      :collection-contained-documents))
					  (digx element :type-def :accessor))
				      )))))))
    keys))

(defun sort-by-keys (documents keys)
  (flet ((sort-val (document)
	   (let ((values))
	     (dolist (key keys)
	       (let* ((accessor (getx key :accessor))
		      (val (if accessor
			       (accessor-value (getx document (getx key :name)) accessor)
			       (getx document (getx key :name)))))
		
		 (setf values (frmt "~A~A" values val))))	    
	     values)))
    (sort (copy-list documents) #'string-lessp :key #'sort-val)))


(defun grid-js-render-new (document-type hierarchy)
  (let ((active-page (getcx document-type :active-page))
	(documents))

    (setf documents (append hierarchy (list (list :type-def document-type
					      :document 0))))
    
    (js-render "cl-wfx:ajax-grid-edit"
	       (frmt "ajax-new-~A" document-type)	      
	       (js-pair "document-type" (frmt "~A" document-type))
	       
	       (js-pair "wfxaction" "new")
	       
	       (js-pair "document-hierarchy"
			(hierarchy-string documents))

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

(defun render-table-cell (element document)
  
  (let ((val (print-document-val 
	      (complex-type element)
	      element document)))
    
    (with-html-string
      (cl-who:htm
       (:td

	:style (cond
		 ((or (equalp (simple-type element) :integer)
		      (equalp (simple-type element) :number))
		  "text-align:right;")
		 (t
		  "text-align:left;"))
	(cond
	  ((empty-p val)
	   (cl-who:str val))
	  ((or (equalp (complex-type element) :lambda)
	       (equalp (complex-type element) :lisp-code)
	       (equalp (complex-type element) :css)
	       (equalp (complex-type element) :html)
	       (equalp (complex-type element) :java-script))
	   (cl-who:htm
	    (:textarea :readonly "readonly" :style "width:100%;"
		       (cl-who:str val))))
	      
	  ((equalp (complex-type element) :text)
	   (cl-who:htm
	    (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		  (cl-who:str val)
		  )))
	  ((and (or (equalp (complex-type element) :string)
		    (equalp (complex-type element) :link)
		    (equalp (complex-type element) :list)
		    (equalp (complex-type element) :collection))
		(or (> (length val) 15)
		    (and
		     (> (length val) 15)
		     (> (length val) (length (getx element :label))))))

	   (if (equalp (complex-type element) :link)
		(cl-who:htm
		 (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		       (:a :target "_blank" :href val (cl-who:str val))))
		(cl-who:htm
		 (:div :style "resize: vertical; text-overflow: ellipsis;overflow: hidden;height:15px;"
		       (cl-who:str val))))
	   )
	  ((or (equalp (complex-type element) :number)
	       (equalp (complex-type element) :integer))
	   (cl-who:str val))
	  (t
	       
	   (cl-who:htm
		 
	    (:div
	     :style "display:table-cell;height:25px;vertical-align:middle;width:100%;"
			    
	     (cl-who:str val))))))))))

(defun render-document-row (subs document-type document elements hierarchy)
  (let ((row-elements (limit-elements (get-data-elements elements)
				  (if (> (length hierarchy) 1)
				      hierarchy
				      nil))))
    

    (with-html-string
      (:tbody
       :class "grow"
       :style "display:table-row-group;"
       :id (frmt "ajax-editing-row-~A" (document-hash document))
       :data-hash (document-hash document)
       :type-def document-type
       :data-action "expand"
       :data-pages (or (parameter "pages") 50)
       :data-page (or (getcx document-type :active-page)  1)
       :data-collection (gethash :collection-name (cache *context*))
       :data-expanded (if (string-equal (frmt "~A" (getcx document-type :expand-id)) 
					(frmt "~A" (document-hash document)))
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
		   (render-expand-buttons subs document-type document)))))

	(dolist (element row-elements)	  
	  (cl-who:str (render-table-cell element document)))

	(:td :style "width:40px;"
	      (:div :class "btn-group"
		   
		   (when (not *rendering-shit*)
		     (cl-who:str			      
		      (render-grid-buttons document-type document hierarchy)))))
	
	(when (check-top-level-p document-type)
	  (cl-who:htm
	   (:td :style "width:40px;text-align:center;"
	    
	    (cl-who:str
	     (render-select-button document))))))))))


(defun render-select-document-row (document-type documents elements)
  (with-html-string
      (:tbody :style "display:table-row-group;"
	      :id (frmt "select-from-~A"
			document-type)
	      (dolist (document documents)
		(cl-who:htm
		 (:tr
		  (:td)
	
		  (dolist (element (limit-elements elements
					       nil))
		    (cl-who:str (render-table-cell element document)))

		  (:td :width "5px"
		       (:div :class "btn-group"
			     (cl-who:str
				(render-select-button document))))))))))

(defun render-select-from-grid (document-type sub sub-data-spec hierarchy)
 
  (when (and (equalp (parameter "wfxaction")
		     "select-from")
	     (string-equal
	      (parameter "document-type")
	      (frmt "~A" sub-data-spec)))


    (let ((*rendering-shit* t)
	  (elements (get-data-elements
		   (getcx (digx sub :type-def :type-def) :elements))))
      
      (with-html-string
	(:tbody :style "display:table-row-group;"
		:id (frmt "ajax-select-from-group-~A"
			  (digx sub :type-def :type-def))
		(:tr
		 (:td :colspan (+ (length (limit-elements elements
							nil))
				  2)
		      (:span :class "font-weight-bold"
		       (cl-who:str
			  (frmt "Select ~A to add..."
				(digx sub :type-def
				     :collection))))

		      (:i :name "cancel" 				   
			  :class "fa fa-eject fa-2x text-dark float-right"
			  :onclick
			  (frmt
			   "~A;toggle_display(\"ajax-select-from-group-~A\");"
			   (grid-js-render document-type
					   :action "cancel")
			   (digx sub :type-def :type-def)))
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
			   (js-pair "document-type"
				    (frmt "~A" sub-data-spec))
		       
			   (js-pair "wfxaction" "add-selection")
			   (js-pair "add-selection-element"
				    (frmt "~A" (digx sub :name)))
			   (js-pair "document-hierarchy"
				    (hierarchy-string hierarchy))
			   (js-pair "pages"
				    (or (parameter "pages") 50))
			   (js-pair "page"
				    (or (getcx document-type :active-page) 1)))
			  (cl-who:str "Add Selection")))))
	
	(:tbody :style "display:table-row-group;"
		(:tr
		 (:td :colspan (+ (length (limit-elements elements
							nil))
				  2)		      
		      (:table :class "table table-sm grid-table-stuffx"
		       (cl-who:str (render-select-document-row
				    (digx sub :type-def :type-def)
				    (wfx-query-context-data
				     (digx sub :type-def :collection)
				     :query (lambda (document)
					     (if (getx sub :filter)
						 (funcall
						  (eval% (getx sub :filter))
						  document
						  (first (last (car hierarchy)))
						  hierarchy)
						 document)))
				    elements))))))))))

(defun render-expand (document-type document subs sub-level hierarchy)

  (when (string-equal (frmt "~A" (getcx document-type :expand-id)) 
		      (frmt "~A" (document-hash document)))

   
    (with-html-string
      (:div :class "row card-columns"
	    (dolist (sub subs)
	      (let* ((sub-data-spec (digx sub :type-def :type-def)))

		(setf (getcx sub-data-spec :parent-document) document)

		(setf (getcx sub-data-spec :collection-name)
		      (digx sub :type-def :collection))

                
		(unless (getcx sub-data-spec :type-def)
		  (setf (getcx sub-data-spec :type-def)
			(find-type-defenition *system* (digx sub :type-def :collection)
				       sub-data-spec))
			      
		  (setf (getcx sub-data-spec :elements) 
			(digx (getcx sub-data-spec :type-def)
			     :elements)))
			    
		(setf (getcx sub-data-spec :active-document) document)

			    
		(cl-who:htm
		 (:div :class "col"
		       (:div :class "card"
			     :id sub-data-spec
			     :style "border-left-style: dotted;border-width:2px; border-left-color:#48C9B0;min-width:255px;"
			     (:h5 :class "card-header"
				  (cl-who:str
				   (frmt "~A" (string-capitalize
					       (getx sub :name))))
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
						   (getx document sub) 
						   (+ sub-level 1)
						   document
						   document-type
						   hierarchy))))))
			     
			     (cl-who:str
			      (render-select-from-grid
			       document-type sub
			       sub-data-spec
			       hierarchy)))))))))))



(defun render-row-goodies (subs sub-level document-type
			   document  parent-document parent-spec
			   elements
			   hierarchy)
  (unless *rendering-shit*
    (let ((header-elements (get-header-elements elements)))

      (with-html-string
	
	(if (and (equalp (parameter "wfxaction") "save")
		 (getcx document-type :edit-object)
		 (getcx document-type :validation-errors))
	    
	    (cl-who:str (render-grid-edit document-type
					  elements
					  document
					  parent-document
					  parent-spec
					  hierarchy
					  ))
	    (cl-who:htm (:tbody
			 :class ""
			 :style "display:none"
				:id (frmt "ajax-edit-~A" (document-hash document)))
			(:tbody
			 :class ""
			 :id (frmt "ajax-expand-row-~A" (document-hash document))
			 :style (if (and (string-equal (frmt "~A" (getcx document-type :expand-id)) 
						       (frmt "~A" (document-hash document))))
				    "display:table-row-group"
				    "display:none")
			 (if (string-equal (frmt "~A" (getcx document-type :expand-id)) 
					   (frmt "~A" (document-hash document)))
		    
			     (cl-who:htm
			      (:tr
			  
			       (:td :colspan (if subs
						 (+ (length
						     (limit-elements header-elements
								   (if (> (length hierarchy) 1)
								       hierarchy)))
						    3)
						 (+ (length
						     (limit-elements header-elements
								   (if (> (length hierarchy) 1)
								       hierarchy)))
						    1))
			   
				    (cl-who:str (render-expand document-type document subs
							       sub-level
							       hierarchy)))))))))
	
	))))


(defun render-new-edit (document-type elements parent-document parent-spec hierarchy)
  (with-html-string

    (:tbody ;;:style "display:none"
	    :id (frmt "ajax-new-~A" document-type)
	    
	    (when (and (getcx document-type :edit-document)
		       (not (document-hash (getcx document-type :edit-document))))

	      (when (and (and (equalp (parameter "wfxaction") "save")
			      (getcx document-type :edit-object)
			      (getcx document-type :validation-errors))
			 (string-equal (parameter "document-type")
				       (frmt "~A" document-type)))
		
		(cl-who:str
		 (render-grid-edit document-type elements
				   (getcx document-type :edit-document)
				   parent-document parent-spec
				   hierarchy)))))))


(defun document-type-access-p (document-type)
  (let ((access-p))
  
    (when (and (active-user)
	       (digx  (active-user) :selected-licenses))
      (cond ((getx (current-user) :super-user-p)
	     (setf access-p t))
	    (t (dolist (lic (digx  (active-user)
				  :selected-licenses))
		 (when (license-user lic)
		   (let ((no-spec t))
		     (dolist (permission
			       (getx (license-user lic)
				     :type-def-permissions))
		       
		       (when (equalp document-type
				     (digx  permission
					   :type-name))
			 (setf no-spec nil)
			 
			 (when (getx (digx  permission
					   :permissions)
				     :update)
			   (setf access-p t))))
		     (when no-spec
		       (setf access-p t))))))))
    access-p))



(defun render-grid-data (document-type page-documents sub-level
			 parent-document parent-spec hierarchy)
  (let ((documents))

    
    
    (with-html-string
      (let ((elements (getcx document-type :elements))
	    (subs))

	(unless elements
	  
	  (let ((document-def (wfx-get-document-type document-type)))
            
	    (when document-def
	      (setf (getcx document-type :elements) (elements document-def)))

	    (setf elements (elements document-def))))

	
	(dolist (element elements)
	  (when (sub-grid-p element)
	 
	    (when (document-type-access-p (getx (digx element :type-def)
					    :type-def))
	     
	      (pushnew element subs))))

	
	(setf documents (sort-by-keys page-documents (keysx elements)))	

	(dolist (document documents)
	  (let ((document-hierarchy (append hierarchy (list (list :type-def
							     document-type
							     :document document)))))

	    (unless (and (equalp (parameter "wfxaction") "save")
			 (getcx document-type :edit-object)
			 (getcx document-type :validation-errors))
	      (cl-who:str
	       (render-document-row subs document-type document elements
				document-hierarchy)))

	    
	    (cl-who:str
	     (render-row-goodies subs
				 sub-level
				 document-type
				 document
				 parent-document
				 parent-spec
				 elements
				 document-hierarchy))))
		    
	(cl-who:str (render-new-edit document-type elements
				     parent-document parent-spec
				     hierarchy))))))

(defun render-grid-sizing (document-type)
  (with-html-string
    (:input :type "text" :name "pages"
	    :class "float-right form-control-sm"
	    :size 2
	    :id "pages"
	    :value (or (parameter "pages")
		       (getcx 
			document-type :show-pages-count)
		       50)
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "pages"
	     13
	     "cl-wfx:ajax-grid"
	     (gethash :collection-name (cache *context*))
	     nil
	     (js-pair "document-type"
		      (frmt "~A" document-type))	     
	     (js-pair "wfxaction" "grid-sizing")))))

(defun render-grid-search (document-type)
  (with-html-string
    (:input :type "text"
	     :class "form-control" ;;"form-control-sm float-right"
	     :name "search" 	   
	     :id "search"
	     :value (or (parameter "search")
			(getcx 
			 document-type :search)
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
	      (js-pair "document-type"
		       (frmt "~A" document-type))
	      (js-pair "wfxaction" "grid-search")))))

(defun fetch-grid-page-data (document-type documents)

  (let* ((keys (keysx (getcx 
		      document-type 
		      :elements)))

	 (sorted-documents (sort-by-keys documents keys)))

    (setf (getcx document-type :show-pages-count) 
	  (if (not (empty-p (parameter "pages")))
	      (parse-integer (parameter "pages"))
	      (or (getcx 
		   document-type :show-pages-count)
		  50)))
    
    (setf (getcx document-type :active-page)
	  (if (not (empty-p (parameter "page")))
	      (parse-integer (parameter "page"))
	      (or (getcx 
		   document-type :active-page)
		  1)))

    (multiple-value-bind (page-count rem)
	(floor (ensure-num (getcx document-type :data-count)) 
	       (getcx document-type :show-pages-count))
      
      (setf (getcx document-type :page-count) page-count)
      (setf (getcx document-type :page-count-remaining) rem))
 
    
    (setf (getcx document-type :start-page-count) 
	  (- (* (ensure-num (getcx 
			     document-type :active-page)) 
		(ensure-num (getcx 
			     document-type :show-pages-count))) 
	     (ensure-num (getcx 
			  document-type :show-pages-count))))

    (setf (getcx document-type :end-page-count) 
	  (if (< (* (getcx 
		     document-type :active-page)
		    (getcx 
		     document-type :show-pages-count)) 
		 (getcx 
		  document-type :data-count))
	      (* (getcx 
		  document-type :active-page)
		 (getcx 
		  document-type :show-pages-count))))
 
    (when (or (not (getcx document-type :end-page-count))
	      (>= (getcx document-type :end-page-count)
		  (length sorted-documents)))
      (setf (getcx document-type :end-page-count) (length sorted-documents)))


    (setf (getcx document-type :data-subset-count) (length documents))
    
    (when documents
      (subseq sorted-documents 
	      (getcx document-type :start-page-count) 
	      (getcx document-type :end-page-count)))))

(defun found-nil-p (list)
  (dolist (document list)
    (unless document
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

(defun filter-function (document-type)
  (lambda (document)
    (let ((found nil))
      (dolist (element (getcx 
		      document-type 
		      :filter-elements))
	(let ((filter-term 
	       (or
		(parameter 
		 (frmt "~A-filter" (getx element :name)))
		(getcx document-type
		       (intern (string-upcase
				(frmt "~A-filter" (getx element :name))))))))
	  (when filter-term			       
	    (when (digx element :type-def)
	      (when (sub-grid-p element)
		(let ((accessor (digx element :type-def :accessor)))
		  (dolist (sub-val (getx document element))
		    (when sub-val
		      (let* ((val (accessor-value document accessor)))
		
			(if (filter-found-p filter-term val)
			    (push t found)
			    (push nil found)))))))
	      
	      (let ((val (print-document-val 
			  (complex-type element) element document)))
		(if (filter-found-p filter-term val)
		    (push t found)
		    (push nil found)))))))
      (unless (found-nil-p found)
	document))))

(defun search-function (document-type search-term)
  (lambda (document)
    (let ((found nil))
      (dolist (element (getcx 
		      document-type 
		      :elements))
	(when (digx element :type-def)
	    (when (sub-grid-p element)
	      (let ((accessor (digx element :type-def :accessor)))
	
		(dolist (sub-val (getx document element))
		  (when sub-val
		    
		    (let* ((val (accessor-value document accessor)))
		     
		      (when val
			(when (search search-term 
				      val
				      :test #'string-equal)
			  (unless found		       
			    (setf found t)))))))))

	   

	    (let ((val (print-document-val 
			(complex-type element) element document)))
	      
	      (when val
		(when (search search-term 
			      val
			      :test #'string-equal)
		  (unless found		       				     
		    (setf found t)))))))
      (when found
	document))))

(defun fetch-grid-data (document-type &key test)
  (let* ((documents )
	 (collection-name (gethash :collection-name (cache *context*)))
	 (search-term (or (parameter "search") 
			  (getcx 
			   document-type :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (getcx document-type :filter)))

    (unless (or search-p filter-p)
      
      (multiple-value-bind (documentsx count)
	    (wfx-query-context-data collection-name
				    :query test)
	  (setf (getcx document-type :data-count) count)
	  (setf documents documentsx)))

    (when (or search-p filter-p (getcx document-type :filter-elements))
      (when (getcx 
	     document-type 
	     :filter-elements)
	(multiple-value-bind (documentsx count)
	    (wfx-query-context-data collection-name
				     :query (filter-function document-type))
	  (setf (getcx document-type :data-count) count)
	  (setf documents documentsx))
	
	(when documents
	  (setf documents
		(query-data
		 documents
		 :query (search-function document-type search-term)))))
      (unless (getcx 
	       document-type 
	       :filter-elements)

	(multiple-value-bind (documentsx count)
	    (wfx-query-context-data collection-name
				       :query (search-function
					      document-type search-term))
	  (setf (getcx document-type :data-count) count)
	  (setf documents documentsx))))

    
    
    (fetch-grid-page-data document-type (if (listp documents)
					documents
					(list documents)))))

(defun render-grid-paging (document-type)  
  (let ((active-page (getcx 
		      document-type :active-page))
	(how-many-rem (getcx 
		       document-type :page-count-remaining))
	(how-many-pages (getcx 
			 document-type :page-count)))
    
    (with-html
      (:nav
       (:ul :class "pagination justify-content-end"

	    (:li :class "page-document"
		 (:span :class "font-weight-bold"
		  (cl-who:str
			 (frmt "Showing ~A of ~A"
			       (if (> (getcx document-type :data-count)
				      (cl-wfx::parse-integer
				       (or (parameter "pages") "50")))
				   (if (< (getcx document-type :data-subset-count)
					  (or (and (parameter "pages") (parse-integer (parameter "pages"))) 50))
				       (getcx document-type :data-subset-count)
				       (or (parameter "pages") 50))
				   (getcx document-type :data-count))
			       (getcx document-type :data-count))))
		 (:span "&nbsp"))
	    
	    (:li :class "page-document"
		 (:button ;;:tabindex -1 ;;when disabled
		  :name "page" :type "submit" 
		  :class (if (> active-page 1)
			     
			     "btn page-link"
			     "btn page-link disabled")		  
		  :onclick 
		  (js-render "cl-wfx:ajax-grid"
			     (gethash :collection-name (cache *context*))
			     (js-pair "document-type"
				      (frmt "~A" document-type))
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
				 "page-document active"
				 "page-document")
		      (:button 
		       :name "page" :type "submit" 
		       :class "btn page-link"
		       :onclick 
		       (js-render "cl-wfx:ajax-grid"
				  (gethash :collection-name (cache *context*))
				  (js-pair "document-type"
					   (frmt "~A" document-type))
				  (js-pair "pages"
					   (or (parameter "pages") 50))
				  (js-pair "page"
					   (+ i 1))				 
				  (js-pair "wfxaction" "page"))
		       (cl-who:str (+ i 1))))))
	      
	      (cl-who:htm
	       (:li :class "page-document"
		    (:button ;;:tabindex -1
		     :name "page" :type "submit" 
		     :class (if (< active-page real-page-count)
				
				"btn page-link"
				"btn page-link disabled")		     
		     :onclick 
		     (js-render "cl-wfx:ajax-grid"
				(gethash :collection-name (cache *context*))
				(js-pair "document-type"
					 (frmt "~A" document-type))
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
	     (wfx-query-context-document
	      (getcx (parameter "document-type")
		     :collection-name)
	      :query (lambda (document)
		       (string-equal
			(frmt "~A" (document-hash document))
			(frmt "~A" (second spliff)))))
	     (getx (getcx (parameter "document-type") :active-document)
		   (intern (string-upcase (parameter "add-selection-element"))

			   :keyword)))))))
    (when persist-p

           
      (let* ((hierarchy (cl-wfx:read-no-eval (parameter "document-hierarchy")))
	     (root-hash (if hierarchy
			    (second (first hierarchy))))
	     (root-document
	      (fetch-grid-root-edit-document root-hash)))

	(persist-document
	 (wfx-get-collection	
	  (gethash :collection-name (cache *context*)))
	 (if (listp root-document)
	     (first root-document)
	     root-document))))))

(defun delete-selected (selected)
  (dolist (document selected)
    (setf (document-deleted-p document) t)
    (persist-document (document-collection document) document)
    ;;(cl-naive-store::remove-document (document-collection document) document)
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
	     (wfx-query-context-document
	      (gethash :collection-name (cache *context*))
	      :query (lambda (document)				      
		      (string-equal
		       (frmt "~A" (document-hash document))
		       (frmt "~A" (second spliff)))))
	     selected)))))
    
    (setf (getcx (parameter "document-type") :selected-grid-documents) selected)
    
    (cond ((string-equal (parameter "action-handler-lambda")
			 :delete-selected)
	   (delete-selected selected))
	  (t
	   (dolist (lambdax (getx (context-spec *context*) :lambdas))	     
	     (when (string-equal (digx  lambdax :event)
				 :select-action-handler)
	       (eval% (digx  lambdax :lamda :code))))))
    
    (setf (getcx (parameter "document-type") :selected-grid-documents) nil)))



(defmethod action-handler ((action (eql :document-action)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (let* ((document-type (getcx (parameter "document-type") :type-def))
	 (document (wfx-query-context-document
		(gethash :collection-name (cache *context*))
		:query (lambda (document)				      
			(string-equal
			 (frmt "~A" (document-hash document))
			 (frmt "~A" (parameter "document-id")))))))
    

    (when document
      
      (dolist (action (getx document-type :document-actions))
	(when (string-equal (parameter "action-name") (getx action :name))

	  (funcall% (eval% (getx action :action))  document )
	  )))))

(defun getcx (&rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))
    
    (when place
      (if indicators
	  (apply 'digx  place indicators)
	  place))))

(defun (setf getcx) (value &rest indicators)
  (let* ((indicator (pop indicators))
	 (place (gethash indicator (cache *context*))))

    (if indicators
	(progn
	  (unless place
	    (setf place (list (first indicators) nil)))
	  (setf (digx place indicators) value)          
	  (setf (gethash indicator (cache *context*)) 
		place)) 
	(setf (gethash indicator (cache *context*)) value))))

(defun set-grid-search (document-type)
  (when (or (equalp (parameter "wfxaction") "filter")
	    (equalp (parameter "wfxaction") "un-filter"))
    
    (when (equalp (parameter "wfxaction") "filter")
      (if (empty-p (parameter "search"))
	  (setf (getcx document-type :search)
		(parameter "search"))))
    
    (when (equalp (parameter "wfxaction") "un-filter")
      (setf (getcx document-type :search) nil)))

  (unless (or (equalp (parameter "wfxaction") "filter")
	      (equalp (parameter "wfxaction") "un-filter"))
    (if (and (parameter "search") (not (empty-p (parameter "search"))))
	(setf (getcx document-type :search) (parameter "search"))
	(if (string-equal (parameter "search") "")
	    (setf (getcx document-type :search) nil)))))

(defun set-grid-filter (document-type)
  (when (equalp (parameter "wfxaction") "filter")
    (setf (getcx document-type :filter) t))
  
  (when (equalp (parameter "wfxaction") "un-filter")
    (setf (getcx document-type :filter) nil))

  (let ((elements (getcx document-type :filter-elements)))
    (dolist (element (getcx document-type :elements))
	(when (parameter (frmt "~A-filter" (getx element :name)))
	  (pushnew element elements)
	  (setf (getcx document-type
		       (intern (string-upcase
				(frmt "~A-filter" (getx element :name)))))  
		(parameter (frmt "~A-filter" (getx element :name))))))
    
      (setf (getcx document-type :filter-elements) elements)))

(defun set-type-context (collection document-type)
  (unless (getcx document-type :type-def)
    (setf (getcx document-type :type-def)
	  (find-type-defenition *system* (digx collection :collection :name) document-type))    
    (setf (getcx document-type :elements) 
	  (digx (getcx document-type :type-def) :elements))))

(defun set-grid-context (collection-name document-type)
  (unless (gethash :collection-name (cache *context*))
    (setf  (gethash :collection-name (cache *context*)) collection-name)
    (setf  (gethash :type-def (cache *context*)) document-type)
    
    (unless (equalp (parameter "wfxaction") "save")
      (setf (getcx document-type :root-document) nil))))

(defun set-grid-expand ()  
  (when (equalp (parameter "wfxaction") "expand")
    (setf (getcx (parameter "document-type") :expand-id) (parameter "document-id")))
  
  (when (equalp (parameter "wfxaction") "unexpand")    
    (setf (getcx (parameter "document-type") :expand-id) nil)))



(defun render-grid-menu (collection-name document-type)
  (with-html-string
    (:div :class "navdocument dropdown dropleft"
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
			   :onclick (grid-js-render-new document-type nil)
			   
			   (:i	
			    :class "fa far fa-plus"))
			  (:td
			   :class "bg-light"
			   (:button
			    :class "btn btn-light w-100 text-left"
			    :name "new"
			    :type "button"	
			    :onclick (grid-js-render-new document-type nil)
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
			   :onclick (grid-js-render-new document-type nil)
			   
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
						(js-pair "document-type" (frmt "~A" document-type))
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
					  document-type :show-pages-count)
					 50)
			      :onkeydown
			      ;;fires ajax call on enter (13)
			      (js-render-event-key 
			       "pages"
			       13
			       "cl-wfx:ajax-grid"
			       (gethash :collection-name (cache *context*))
			       nil
			       (js-pair "document-type"
					(frmt "~A" document-type))	     
			       (js-pair "wfxaction" "grid-sizing"))))
		))))

(defun render-grid (collection-name)
  (when (context-access-p (context-spec *context*))
    (let* ((collection (if (not (gethash :collection-name (cache *context*)))
			   (find-collection-def *system*   
						collection-name)))
	   (document-type (or (gethash :type-def (cache *context*))
			  (and collection
			       (digx collection :collection :type-def)))))   

      (set-grid-context collection-name document-type)
      

      (set-type-context collection document-type)

     ;; (set-type-context (parameter "document-type"))

      (set-grid-search document-type)
	
      (set-grid-filter document-type)
	
      (set-grid-expand)
	
      (let ((page-documents (fetch-grid-data document-type)))
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
			      (render-grid-search document-type)))
		  
		      
		       
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
				 (grid-js-render-new document-type nil)
				 (cl-who:str "+")))
			       (:td
				(cl-who:str (render-grid-menu collection-name document-type))))))))
	
		(:div :class "card-body  p-0 m-0" 		    
		      :id document-type
		      (:div :class "row"
			    (:div :class "col"
				  (:table
				   :class "table table-sm"
				   :style "width: 100%;"
						      
				   (:tbody
				    (cl-who:str
				     (render-grid-header document-type nil nil)))
				   (cl-who:str
				    (render-grid-data
				     document-type page-documents 0 nil nil
				     nil))))))

		(:div :class "card-footer"
		      (:div :class "row"
			    (:div :class "col"
				  (:button 
				   :name "new" :type "submit" 
				   :class "btn btn-outline-success"
				   :aria-pressed "false"
				   :onclick 
				   (grid-js-render-new document-type nil)
				   (cl-who:str "+")))
			    (:div :class "col"
				  (cl-who:str
				   (render-select-actions
				    document-type)))))
		  
		(:div :class "card-footer"
		      (:div :class "row"	  
			    (:div :class "col"
				  (cl-who:str
				   (render-grid-paging document-type))))))
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
	  (cl-naive-store:sanitize-data-file collection)))))
  
 
  (render-grid (getx (context-spec *context*) :name)))

(defun get-child (elements parent-document document-type hash)
  (dolist (element elements)
    (when (sub-grid-p element)
      (when (string-equal document-type (digx element :type-def :type-def))
	(dolist (document (getx parent-document (getx element :name)))
	  (when (string-equal (frmt "~A" (document-hash document))
			      (frmt "~A" hash))
	      (return-from get-child (list (getx element :name) document))))

	(return-from get-child (list (getx element :name)
				     (make-document :type-def
						(string-downcase
						 (frmt "~A" document-type))
						:hash (uuid:make-v4-uuid))))))))

(defun fetch-grid-root-edit-document (hash)
  (let ((collection-name (gethash :collection-name (cache *context*))))   
    (wfx-query-context-document
     collection-name
     :query (lambda (document)
	     (when (string-equal (frmt "~A" (document-hash document))
				 (frmt "~A" hash))
	       document)))))

(defun ajax-auto-complete (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* ((document-type (parameter "document-type"))
	 (element-name (intern (parameter "element-name") :KEYWORD))
	 (elements (getcx document-type :elements))
	 (element))
    (dolist (elementx elements)
      (when (string-equal (getx elementx :name) element-name)
	(setf element elementx)))

    (when element
      
      (let* ((accessors (digx element :type-def :accessor))
	     (list ))
	(cond ((equalp (complex-type element) :collection-contained-document)
	       
	       (setf list (fetch-contained-document-list element (getcx document-type :edit-document))))
	      
	      (t

	       (setf list (wfx-query-context-data		   
			   (digx element :type-def :collection)
			   :query (lambda (document)
				   
				    (and
				     (if (getx element :filter)
					 (funcall
					  (eval% (getx element :filter))
					  document
					  (getcx document-type :edit-document)
					  (getcx document-type :edit-object))
					 t)
			     			  
				     (or
				      (string-equal (parameter
						     (frmt "~A-drop" element-name))
						    "")
				      (search (parameter
					       (frmt "~A-drop" element-name))
					      (accessor-value document accessors)
					      :test #'string-equal))))))))

	(with-html-string
	  (:div
	   :class "auto-complete-menu nav flex-column bg-white rounded border"
	   (setf list (sort (copy-list list) #'string<
			    :key (lambda (document)
				   (accessor-value document accessors))))

	   (setf list (append (list nil) list))
	   
	   (dolist (option list)
	     (cl-who:htm
	      (:span :class "auto-complete-item nav-link"
		     (:input :type "hidden"
			     :value (frmt "~A" (if option (document-hash option))))
		     (cl-who:str
		      (if option
			  (naive-impl:trim-whitespace
			   (accessor-value option accessors)))))))))))))

(defun get-element-accessor (document-type collection)
  (declare (ignorable document-type) (ignorable collection))
  (error "Not Imlemented"))

(defun ajax-auto-complete-x (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let* (
	 (element-name (intern (parameter "element-name") :KEYWORD))
	 (collection (parameter "collection")))
 
    (let* ((accessors (list  element-name))
	   (list (wfx-query-context-data		   
		  collection
		  :query (lambda (document)
			  (or
			   (string-equal (parameter
					  (frmt "~A-drop" element-name))
					 "")
			   (search (parameter
				    (frmt "~A-drop" element-name))
				   (accessor-value document accessors)
				   :test #'string-equal))))))

      (with-html-string
	(:div
	 :class "auto-complete-menu nav flex-column bg-white rounded border"
	 (setf list (sort (copy-list list) #'string<
			  :key (lambda (document)
				 (accessor-value document accessors))))
	 (dolist (option list)
	   (cl-who:htm
	    (:span :class "auto-complete-item nav-link"
		   (:input :type "hidden"
			   :value (frmt "~A" (document-hash option)))
		   (cl-who:str
		    (naive-impl:trim-whitespace 
		     (accessor-value option accessors)))))))))))

(declaim (optimize (debug 3))
	 (notinline ajax-grid-edit))

(defun set-edit-objects ()
  (let* ((document-type (string-downcase (parameter "document-type")))
	 (elements )
	 (hierarchy (cl-wfx:read-no-eval (parameter "document-hierarchy")))
	 (root-type)
	 (root-hash)
	 (root-document)
	 (edit-objects))


    (setf (getcx document-type :edit-object) nil)
    
    (setf root-type (if  hierarchy
			 (string-downcase (frmt "~A"
						(first (first hierarchy))))))
    (setf root-hash (if hierarchy
			(second (first hierarchy))))

    (setf elements (getcx root-type :elements))
     
    (setf root-document (fetch-grid-root-edit-document root-hash))
     
    (unless root-document
      (setf root-document (make-document :type-def document-type)))

    (when root-document
      (setf edit-objects (list (list :type-def root-type :document root-document)))
       
      (if (> (length hierarchy) 1)
	  (let ((document)
		(document-type root-type))

	    (dolist (child (cdr hierarchy))
	      (setf document (get-child (getcx document-type :elements)
				    (or (if document (second document)) root-document)
				    (first child)
				    (second child)))
	      (setf document-type (string-downcase (frmt "~A" (first child))))
	      (setf edit-objects
		    (push (list :type-def document-type
				:document (second document)
				:element-name (first document))
			  edit-objects)))
	    (setf (getcx document-type :edit-object) edit-objects))
	  (setf (getcx document-type :edit-object) edit-objects))
      edit-objects)))




(defun ajax-grid-edit (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))

  
  (let* ((document-type (string-downcase (parameter "document-type")))
	 (elements )
	 (hierarchy (cl-wfx:read-no-eval (parameter "document-hierarchy")))
	 (root-type)
	 (root-hash)
	 (root-document)
	 (edit-objects))


    (setf (getcx document-type :edit-object) nil)
    
    (setf root-type (if  hierarchy
			 (string-downcase (frmt "~A"
						(first (first hierarchy))))))
    (setf root-hash (if hierarchy
			(second (first hierarchy))))
    
    (setf elements (getcx root-type :elements))

    (setf root-document (fetch-grid-root-edit-document root-hash))

    (unless root-document
      (setf root-document (make-document :type-def document-type)))

    (when root-document
      (setf edit-objects (list (list :type-def root-type :document root-document)))

      
      
      (if (> (length hierarchy) 1)
	  (let ((document)
		(document-type root-type))
	    
	    (dolist (child (cdr hierarchy))
	      (setf document (get-child (getcx document-type :elements)
				    (or (if document (second document)) root-document)
				    (first child)
				    (second child)))
	      (setf document-type (string-downcase (frmt "~A" (first child))))
	      (setf edit-objects
		    (push (list :type-def document-type
				:document (second document)
				:element-name (first document))
			  edit-objects)))
	    (setf (getcx document-type :edit-object) edit-objects)

	    	    
	    (render-grid-edit document-type
			      (getcx document-type :elements)
			      (second document)
			      root-document root-type
			      (reverse edit-objects)))
	  (progn
	    (setf (getcx document-type :edit-object) edit-objects)
	    (render-grid-edit root-type elements root-document nil nil
				     (list (list :type-def document-type
						 :document root-document))))))))

(defun index-keys-x (elements document-values)
  (let ((keys))
    (dolist (element elements)
      (when (getx element :key-p)
	(if (document-p (getx document-values (getx element :name)))
	    (push (document-hash (getx document-values (getx element :name))) keys)
	    (push (getx document-values (getx element :name)) keys))))
    (reverse keys)))

(defun move-uploaded-file (elements edit-document)
  (let ((hash (document-hash edit-document))
	(collection (wfx-get-collection
		     (gethash :collection-name (cache *context*)))))

    (unless hash
      (let* (
	    (hashx (uuid:make-v4-uuid)))
	(setf hash hashx)))
    
    (dolist (element elements)

      (when (or
	     (equalp (complex-type element) :image)
	     (equalp (complex-type element) :file))
	(let* ((server-path
	       (string-downcase
		(frmt "~A/files/~A/~A/"
		      (if (location collection)
			  (location collection)
			  (string-downcase
			   (frmt "~A~A"
				 (location (store collection))
				 (name collection))))
		      (parameter "document-type")
		      (getx element :name))))
	       (file-name (sanitize-file-name
			   (parameter (string-downcase
				       (frmt "~A" (getx element :name))))))
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
				  
				  (parameter "document-type")
				  (getx element :name)))) ))

	  (ensure-directories-exist server-path)

	  (unless (probe-file temp-path)
	   
	    (setf (getx edit-document (getx element :name))
		  (getx edit-document (getx element :name))))
	  
	  (when (probe-file temp-path)
	    (fad:copy-file
	     temp-path
	     (merge-pathnames file-name
			      server-path)
	     :overwrite t)
	    (delete-file temp-path)
	    (setf (getx edit-document (getx element :name))
		  (parameter (string-downcase
			      (frmt "~A" (getx element :name)))))))))))

(defun find-contained-document (hash list)
  (dolist (document list)
    (when (string-equal (frmt "~A" (document-hash document))
			(frmt "~A" hash))
      (return-from find-contained-document document))))


(defun synq-value (element edit-document parent-document value)
  
  (cond ((equalp (complex-type element) :collection)
	 
	 (setf (getx edit-document element)
		 (wfx-query-context-document
			(digx element :type-def :collection)
			:query (lambda (document)
				 
				 (string-equal (frmt "~A" (document-hash document))
					       (frmt "~A" value))))))	
	((equalp (complex-type element) :collection-contained-document)
	 (setf (getx edit-document element)
	       (if (not (empty-p value))
		   (progn
		     
		     (find-contained-document
		      value
		      (fetch-contained-document-list
		       element
		       edit-document))))))
	
	((equalp (complex-type element) :contained-document)
         
	 (setf (getx edit-document element)
	       (find-contained-document
		value
		(accessor-value parent-document
				(digx  element :type-def :container-accessor)) )))
	((equalp (complex-type element) :document)
	 (break "mtf ?? ~A" element edit-document)
	 
	 (let* ((sub-document-type (digx  element :type-def :type-def))
		(more-document (or (getx edit-document element)
			       (make-document :type-def
					  sub-document-type))))
	   (synq-document-values sub-document-type
			     (getcx sub-document-type :elements)
			     edit-document
			     more-document)

	   (setf (getx edit-document element) more-document)))
	(t
	 (setf (getx edit-document element) value))))


(defun validate-value (element element-name edit-document parent-document value)
  (if (equalp (complex-type element) :document)
      (cond ((equalp (complex-type element) :collection)
	     (validate-xe
	      edit-document 
	      element 
	      (complex-type element)
	      (parameter element-name)
	      :documents (wfx-query-context-data
			  (digx element :type-def :collection)
			  :query (lambda (document)
				   (string-equal (frmt "~A" (document-hash document))
						 (frmt "~A" value))))))
	    
	    ((equalp (complex-type element) :collection-contained-document)
	     (validate-xe edit-document
			   element 
			   (complex-type element)
			   (parameter element-name)
			   :documents
			   (fetch-contained-document-list element edit-document )))
	    ((equalp (complex-type element) :contained-document)
	     (validate-xe edit-document
			  element 
			  (complex-type element)
			  (parameter element-name)
			  :documents
			  (accessor-value parent-document
					  (digx  element :type-def :container-accessor))))
	    ((getx element :validation)
	     (if (functionp (getx element :validation))
		 (funcall (eval% (getx element :validation)) edit-document value)))
	    (t
	     (list t nil)))
      (list t nil)))

(defun synq-document-values (document-type elements parent-document edit-document)
  
  (dolist (element elements)

    (when (and (digx element :attributes :editable)
	       (digx element :type-def)
	       (not (sub-grid-p element)))

      (let* ((element-name (frmt "~A" (getx element :name)))
	     (valid (if (equalp (complex-type element) :document)
			(validate-value element
					element-name 
					edit-document
					parent-document
					(or
					 (parameter (string-downcase element-name))
					 (parameter element-name)))
			(list t nil))))

	(unless (first valid)
	  (pushnew 
	   (list element-name (second valid))
	   (getcx document-type :validation-errors)))
	      
	(when (first valid)
	  (synq-value element edit-document parent-document
		      (or (parameter (string-downcase element-name))
			  (parameter element-name))))))	  

    (when (getx element :key-p)
      
      (when (empty-p (parameter (getx element :name)))
	(pushnew
	 (frmt "Key values may not be blank. (~A)~%" (getx element :name))
	 (getcx document-type :validation-errors))))))

(defun grid-append-child (document-type parent-slot parent-document edit-document)
  (unless (getcx document-type :validation-errors)
    ;;Append parent-slot only if new	
    (when parent-slot
      (let ((exists
              
	      (find-equalp-document edit-document
					  (getx parent-document parent-slot))))
	;;(break "append ~A~%~A" exists parent-document)

	#|
	(break "llmfh ~A~% ~A"
	       edit-document
	       (append (getx parent-document parent-slot)
		       
		       (list edit-document)))
	|#
	(if exists
	    edit-document            
	    (setf (getx parent-document parent-slot)
		  (append (getx parent-document parent-slot)
			  (list edit-document))))
	;;(break "append x ~A" parent-document)
	))))

(defun grid-persist-document (document-type root-document)
  (unless (getcx document-type :validation-errors)
    (let ((collection (wfx-get-collection
		       (gethash :collection-name (cache *context*)))))
    
      (setf (getx root-document :user) (getx (current-user) :email))
      (unless collection
	(pushnew 
	 "No default store found check if license is selected."
	 (getcx document-type :validation-errors)))

      (when collection
	(setf (document-collection root-document) collection)
	(setf (document-store root-document) (store collection))
	(persist-document collection root-document :allow-key-change-p t)))))

(defun prepare-edit-objects ()
  (let* ((document-type (string-downcase (parameter "document-type")))
	(edit-objects (reverse (set-edit-objects)))
	(collection (wfx-get-collection
		     (gethash :collection-name (cache *context*))))
	
	(root-document)
	(parent-document)
	(edit-document)
	(parent-slot))

    
    (when edit-objects
      
      (setf root-document (getx (first edit-objects) :document))

      ;;Create new document if saving document higher up in the store
      ;;hierarchy
      (when (document-store root-document)
	(unless (equalp (store collection)
			(document-store root-document))
	  (setf root-document (make-document :type-def (document-type root-document)
				     :collection collection
				     :elements (document-values root-document)
				     :changes (document-changes root-document)))
	  (setf (getx (first edit-objects) :document) root-document)))

      
      (when (> (length edit-objects) 1)
	(setf edit-document (getx (first (last edit-objects)) :document))
	(setf parent-slot (getx (first (last edit-objects)) :element-name))
	(setf parent-document
	      (getx (nth (- (length edit-objects) 2) edit-objects) :document)))
      
      ;;Create new document if saving document higher up in the store
      ;;hierarchy
      (when (and edit-document (document-store edit-document))
	(unless (equalp (store collection)
			(document-store edit-document))
	  (setf edit-document (make-document :type-def document-type
				     :collection (wfx-get-collection
						  (name (document-collection
							 edit-document)))
				     :elements (document-values edit-document)
				     :changes (document-changes edit-document)))
	  (setf (getx (first (last edit-objects)) :document) edit-document)

	  (let ((clean-list (getxo parent-document parent-slot)))
	    
	    (dolist (document clean-list)
	      (when (string-equal (frmt "~A" (document-hash document))
				  (frmt "~A" (parameter "document-id")))
		
		(setf clean-list (remove document clean-list))
		(setf clean-list (pushnew edit-document clean-list))))
	    (setf (getx parent-document parent-slot) clean-list))))

      (unless (> (length edit-objects) 1)
	(setf edit-document root-document)))

    (values root-document parent-slot parent-document edit-document)))


(defun fire-context-event (context event root-document edit-document)
  (let ((context-spec (context-spec context)))
    (when (getx context-spec :lambdas)
      (dolist (lambdax (getx context-spec :lambdas))
	(when (find event (getx lambdax :events) :test 'equalp)
	  (let* ((code (digx  lambdax :lambda :code))
		 (result (eval%
			  `(let ((*root-document* ,root-document)
				 (*edit-document* ,edit-document))
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


  
  (let* ((document-type (string-downcase (parameter "document-type")))
	 (elements (getcx document-type :elements)))

    (setf (getcx document-type :validation-errors) nil)
    
    (multiple-value-bind (root-document parent-slot parent-document edit-document)
	(prepare-edit-objects)
      
      (when elements
	
	(unless edit-document
	  (setf edit-document (make-document :type-def document-type)))

	
	(when (digx (getcx document-type :type-def) :type-def :client-validation)
	  (let ((valid (funcall (eval% (digx (getcx document-type :type-def)
					    :type-def :client-validation))
				  root-document
				  edit-document)))
	      (unless (first valid)
		
		(pushnew 
		 (list document-type (second valid))
		 (getcx document-type :validation-errors)))))
        
	(unless (getcx document-type :validation-errors)
          
	  (handler-case (synq-document-values document-type elements parent-document edit-document)
	    (error (c)
              (break "~S" c)
	      (pushnew 
	       (list document-type (cl-who:escape-string  (frmt "~S" c)))
		 (getcx document-type :validation-errors))
	      
	      ))
	  
          
	  (unless (getcx document-type :validation-errors)
                  
		  (move-uploaded-file elements edit-document)   
		  
		  (grid-append-child document-type parent-slot parent-document
				     edit-document)

		  
		  (grid-persist-document document-type root-document)

		  (fire-context-event context :save root-document edit-document)))
	

	))))


(defmethod action-handler ((action (eql :delete)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (multiple-value-bind (root-document parent-slot parent-document edit-document)
	(prepare-edit-objects)
   
      (when (and edit-document parent-slot)
	(let ((clean-list (getx parent-document parent-slot)))
	  (dolist (document clean-list)
	    (when (string-equal (frmt "~A" (document-hash document))
				(frmt "~A" (parameter "document-id")))
	      (setf clean-list
		    (remove document clean-list))
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
			     (parameter "document-type")))))
		
		(dolist (element (getcx (parameter "document-type") :elements))
		  (when (or (equalp (simple-type element) :image)
			    (equalp (simple-type element) :file))
		    (delete-file
		     (string-downcase
		      (frmt "~A~A/~A" server-path
			    (getx element :name)
			    (sanitize-file-name
			     (getx edit-document (getx element :name)))))))))))
	  
	  (setf (getx parent-document parent-slot) clean-list)
	  
	  (persist-document (document-collection root-document) root-document)))

      (unless (and edit-document parent-slot)
	(setf (document-deleted-p root-document) t)
	(persist-document (document-collection root-document) root-document)
	(cl-naive-store::remove-document (document-collection root-document) root-document))))
