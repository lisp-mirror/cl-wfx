(in-package :cl-wfx)

(defun print-item-val-s* (field item)
  (let ((*print-case* :downcase))
    (frmt "~S"
	  (item-val (field-data-type field) 
		    field 
		    item))))

(defun print-item-val-a* (field item)
  (let ((*print-case* :downcase))
    (frmt "~A"
	  (item-val (field-data-type field) 
		    field 
		    item))))

(defmethod print-item-val ((type (eql 'symbol)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'keyword)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'script)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'string)) field item &key &allow-other-keys)
   (print-item-val-a* field item))

(defmethod print-item-val ((type (eql 'email)) field item &key &allow-other-keys)
   (print-item-val-a* field item))

(defmethod print-item-val ((type (eql 'number)) field item &key &allow-other-keys)
   (print-item-val-a* field item))

(defmethod print-item-val ((type (eql 'date)) field item &key &allow-other-keys)
   (print-item-val-a* field item))

(defmethod print-item-val ((type (eql 'boolean)) field item &key &allow-other-keys)
  (if (item-val (field-data-type field) 
		field 
		item)
      "checked"
      ""))

(defmethod print-item-val ((type (eql 'list)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'list-item)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'data-group)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'data-list)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'data-member)) field item &key &allow-other-keys)

  (let ((full-type (cdr (getf field :db-type)))
	 (item-val (item-val (field-data-type field) 
			     field 
			     item)))    
    (when item-val
      (frmt "~A" (slot-value 
		  item-val
		  (getf full-type :key-accessor))))))

(defgeneric render-input-val (type field item &key &allow-other-keys))

(defun render-input-val* (type field item)
  (let ((name (getf field :name)))
    (if (not (getf field :editable))
	(monkey-html-lisp:htm
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"	     
		  :value
		  (print-item-val 
		   type
		   field 
		   item)
		  :disabled "disabled"))
	(monkey-html-lisp:htm
	  (:input :class "form-control"
		  :id name
		  :name name 
		  :type "text"	     
		  :value
		  (print-item-val 
		   type
		   field 
		   item))))))

(defmethod render-input-val ((type (eql 'symbol)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'keyword)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'string)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'email)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'number)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'date)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'boolean)) field item &key &allow-other-keys)
  (let ((name (getf field :name))
	(print-val (item-val type field item)))
    
    (monkey-html-lisp:htm
      (:div :class "form-check" 
	    (if (not (getf field :editable))
		(monkey-html-lisp:htm (:div :class "form-check-label"
					    (:input
					     :class "form-check-input"
					     :type "checkbox"
					     :id name
					     :name name
					     :value (item-val 
						     type
						     field 
						     item)
					     :checked print-val
					     :aria-label "..."
					     :disabled "disabled")))
		(monkey-html-lisp:htm (:div :class "form-check-label"
					    (:input
					     :class "form-check-input"
					     :type "checkbox"
					     :id name
					     :name name
					     :value (item-val 
						     type
						     field 
						     item)
					     :checked print-val
					     :aria-label "..."))))))))

(defmethod render-input-val ((type (eql 'script)) field item &key &allow-other-keys)
  (let ((name (getf field :name)))
    (if (not (getf field :editable))
	(monkey-html-lisp:htm
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 50 :rows 10
	   :disabled "disabled"
	   (print-item-val 
	    type
	    field 
	    item)))
	(monkey-html-lisp:htm
	  (:textarea 
	   :class "form-control"
	   :id name
	   :name name :cols 50 :rows 10
	
	   (print-item-val 
	    type
	    field 
	    item))))))

(defmethod render-input-val ((type (eql 'list)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'list-item)) field item &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (full-type (cdr (getf field :db-type)))
	 (list (getf full-type :list)))

    (monkey-html-lisp:htm
      (:select :name name
       (dolist (option list)
	 (if (string-equal
	      (print-item-val type field item) (frmt "~S" option))
	     (monkey-html-lisp:htm
	       (:option :selected "" :value (frmt "~S" option) option))
	     (monkey-html-lisp:htm
	       (:option :value (frmt "~S" option) option))))))))

(defmethod render-input-val ((type (eql 'data-group)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'data-list)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defmethod render-input-val ((type (eql 'data-member)) field item 
			     &key &allow-other-keys)
;;  (break "~A ~A" item parent-item )
  (let* ((name (getf field :name))
	 (full-type (cdr (getf field :db-type)))
	 (data-spec (get-data-spec (getf full-type :data-spec)))
	 (list (fetch-all (collection-name data-spec) :result-type 'list)))

    (monkey-html-lisp:htm
      (:select :name name
       (dolist (option list)
	 (monkey-html-lisp:htm
	   (if (equalp (xdb2::id option) (xdb2::id item))
	     (monkey-html-lisp:htm
	       (:option :selected "" :value (frmt "~S" (xdb2::id option)) 
			(frmt "~A" (slot-value option (getf full-type :key-accessor)))))
	     (monkey-html-lisp:htm
	       (:option :value (frmt "~S" (xdb2::id option)) 
		    (frmt "~A" (slot-value option (getf full-type :key-accessor))))))))))))






(defun grid-js-render-form-values (renderer spec-name form-id 
				   &key widget-id action item-id )
  (let ((active-page (get-context-data-spec-attribute 
		      spec-name :active-page)))
    (js-render-form-values 
     renderer
     (or widget-id "grid-table")
     form-id
     (js-pair "data-spec"
	      (frmt "~S" spec-name))
     
     (js-pair "action" (or action ""))
     
     (js-pair "item-id" (or item-id
			    (get-context-data-spec-attribute 
			     spec-name :item-id)
			    ""))
    
     
     (js-pair "pages"
	      (or (parameter "pages") 10))
     (js-pair "page"
	      (or active-page 1))
     
    		      
     ))
  )

(defun grid-js-render (renderer spec-name &key widget-id action item-id)
  (let ((active-page (get-context-data-spec-attribute 
		      spec-name :active-page)))

    (js-render renderer
	       (or widget-id "grid-table")
	      
	       (js-pair "data-spec"
			(frmt "~S" spec-name))
	       (js-pair "action"
			(or action ""))
	       (js-pair "item-id" (or item-id ""))
	      
	       (js-pair "pages"
			(or (parameter "pages") 10))
	       (js-pair "page"
			(or active-page 1)))))



(defun render-expand-buttons (subs spec-name item)
;;  (break "subs ~A" subs)
  (if subs
      (if (equalp (ensure-parse-integer 
		   (get-context-data-spec-attribute spec-name
						    :expand-id)) 
		  (xdb2:id item))
	  (monkey-html-lisp:htm
	    (:button
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm active"				 
	     :aria-pressed "true"
	     :onclick (grid-js-render "cl-wfx:ajax-grid" spec-name
				      :action "unexpand"
				    ;;  :item-id (xdb2::id item)
				      )
	     "-"))
	  (monkey-html-lisp:htm
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm border-0"
	     :aria-pressed "false"
	     :onclick (grid-js-render "cl-wfx:ajax-grid" spec-name
				      :action "expand"
				      :item-id (xdb2::id item))
	     "+")))))


(defun render-grid-buttons (spec-name item)
  (let ((permissions (permissions (context-spec *context*))))
  ;;  (break "permissions ~A" (context-spec *context*))
    (dolist (permission permissions)
      (cond ((equalp permission :update)
	     (monkey-html-lisp:htm
	       
	       (:button ;;:tabindex -1 ;;when disabled
		:name "edit" :type "submit" 
		:class
		(if (and (parameter "item-id")
			 (string-equal 
			  (parameter "item-id") 
			  (frmt "~A" (xdb2:id item))))
		    "btn btn-outline-primary btn-sm active"
		    "btn btn-outline-primary btn-sm")
		:aria-pressed 
		(if (and (parameter "item-id")
			 (string-equal 
			  (parameter "item-id") 
			  (frmt "~A" (xdb2:id item))))
		    "true"
		    "false")
		:onclick 
		(grid-js-render "cl-wfx:ajax-grid" spec-name
				:action "edit"
				:item-id (xdb2::id item)
				
				)
		"Edit")))
	    ((equalp permission :delete)
	     (monkey-html-lisp:htm
	       (:button ;;:tabindex -1 ;;when disabled
		:name "edit" :type "submit" 
		:class (if (and (parameter "item-id")
				(string-equal 
				 (parameter "item-id") 
				 (frmt "~A" (xdb2:id item))))
			   "btn btn-outline-primary btn-sm active"
			   "btn btn-outline-primary btn-sm")
		:aria-pressed (if (and (parameter "item-id")
				       (string-equal 
					(parameter "item-id") 
					(frmt "~A" (xdb2:id item))))
				  "true"
				  "false")
		:onclick 
		(grid-js-render "cl-wfx:ajax-grid" spec-name
				:action "delete"
				:item-id (xdb2::id item)
				)
		"Delete")))))))


(defun render-grid-edit (spec-name fields item parent-item parent-spec sub-name)
  
  (set-context-data-spec-attribute spec-name :list-field-name sub-name)
  (set-context-data-spec-attribute spec-name :edit-item item)
  (set-context-data-spec-attribute spec-name :parent-item parent-item)
  (set-context-data-spec-attribute spec-name :parent-spec parent-spec)
  
  (set-context-data-spec-attribute (parameter "data-spec")
				   :item-id (xdb2::id item))
  
  (monkey-html-lisp:htm
    (:div :class "card" :id (string-downcase (frmt "grid-edit-~A"  spec-name))
	  (:div :class "card-header"
		(frmt "Editing... ~A" (string-capitalize spec-name)))
	  (:div :class "card-block"
		(:div :class "row" 
		      :id (frmt "~A" 
				spec-name)
		      (:div :class "col" 
			    
			    (dolist (field fields)
			      (let* ((name (getf field :name))
				     (label (getf field :label)))
				      
				(when (and (getf field :display) 
					   (and (not (equalp (field-data-type field)
							     'data-group))
						(not (equalp (field-data-type field)
							    'data-list))
					       ))
					
				  (monkey-html-lisp:htm					  
				    (:div :class (if (getf field :editable)
						     "form-group row"
						     "form-group row disabled")
					  (:label :for name 
						  :class "col-sm-2 col-form-label" 
						  (if label
						      label
						      (string-capitalize 
						       (substitute 
							#\Space  
							(character "-")  
							(format nil "~A" name) 
							:test #'equalp))))
					  (:div :class "col"
						(render-input-val 
						 (field-data-type field) 
						 field item :parent-item parent-item))))))))))
	  (:div :class "card-footer"
		(when (gethash :validation-errors (cache *context*))
		  (let ((errors (gethash :validation-errors (cache *context*))))
			  
		    (setf (gethash :validation-errors (cache *context*)) nil)
		    (setf (gethash :validation-error-item-id (cache *context*)) nil)
			  
		    (monkey-html-lisp:htm
		      (:div :class "row"
			    (:div :clas "col"
				  (frmt "Errors ~S"
					errors))))))
			 
		(:div :class "row"
		      (:div :class "col"
			    (:button ;;:tabindex -1 ;;when disabled
			     :name "save" 				   
			     :type "submit" 
			     :class "btn btn-outline-primary btn-sm"
			     :onclick 
			     (grid-js-render-form-values
			      "cl-wfx:ajax-grid" 
			      spec-name
			      (string-downcase  (frmt "grid-edit-~A"  spec-name))
			      :widget-id nil 
			      :action "save"
			      
			      )
			     "Save")
			    (:button ;;:tabindex -1 ;;when disabled
			     :name "cancel" 				   
			     :type "submit" 
			     :class "btn btn-outline-primary btn-sm float-right"
			     :onclick 
			     (grid-js-render "cl-wfx:ajax-grid" spec-name
					     :widget-id nil
					     :action "cancel"
					     )
			     "Cancel")))))))

(defun render-grid-col-filter (spec-name col-name)
  (monkey-html-lisp:htm
	(:input :class "w-100"
		:type "text" 
		:name (frmt "~A-filter" col-name) 

		:id (frmt "~A-filter" col-name)
		:value (or (parameter (frmt "~A-filter" col-name))
			   (get-context-data-spec-attribute 
			    spec-name (frmt "~A-filter" col-name))
			   "")
		:onkeydown
		;;fires ajax call on enter (13)
		(js-render-event-key 
		 (frmt "~A-filter" col-name)
		 13
		 "cl-wfx:ajax-grid"
		 "grid-table"
		 (js-pair "data-spec"
			  (frmt "~S" spec-name))
		 
		 
		 (js-pair "action" "grid-col-filter")))))

(defun render-grid-header (spec-name fields sub-p)
  (let ((subs))
    
    (dolist (field fields)
      (cond ((or (equalp (field-data-type field) 'data-group)
		 (equalp (field-data-type field) 'data-list))
	     (pushnew field subs))))
    
    (monkey-html-lisp:htm
      (:div :class "row"	
	    (when subs
	      (monkey-html-lisp:htm
		(:div :class "col-sm-1"
		      " ")))
	    
	    (dolist (field fields)
	      
	      (when (and (getf field :display) 
			 (and (not (equalp (field-data-type field)
					  'data-group))
			     (not (equalp (field-data-type field)
					  'data-list))))
		(let* ((name (getf field :name))
		       (label (getf field :label)))
		  
		  (monkey-html-lisp:htm
		    (:div :class "col"
			  (:div :class "row"
				
				(:h6 (if label
					 label
					 (string-capitalize 
					  (substitute #\Space  (character "-")  
						      (format nil "~A" name) 
						      :test #'equalp)))))
			  (unless sub-p
			    (monkey-html-lisp:htm
			      (:div :class "row"
				    (render-grid-col-filter spec-name name)
				    )))
			 
			  )
		    ))))
	    (unless sub-p
	      (monkey-html-lisp:htm	      
		(:div :class "col" ;;column spacing for buttons
		      (:div :class "row"	  
			    (:div :class "col"
				  (render-grid-search spec-name)
				  (render-grid-sizing spec-name))))))))))

(defun render-grid-data (spec-name page-items sub-level sub-name parent-item parent-spec)  
  (let ((sub-level-p (or
		   
		      (not (equalp spec-name 
				   (gethash :root-data-spec 
					    (cache *context*)))))))
    
    (when sub-level-p
      (parse-data-spec-for-grid spec-name))  
    
    (let ((fields (get-context-data-spec-attribute spec-name :data-fields))
	  (subs))
      
      (dolist (field (get-context-data-spec-attribute spec-name :data-fields))
	(cond ((or (equalp (field-data-type field) 'data-group)
		   (equalp (field-data-type field) 'data-list))
	       (pushnew field subs))))
      
      (dolist (item page-items)
	(monkey-html-lisp:htm
	  (:div :class "row"
		(monkey-html-lisp:htm
		  (if subs
		      (monkey-html-lisp:htm
			(:div :class "col-sm-1"
			      (render-expand-buttons subs spec-name item))))
		    
		  (dolist (field fields)
		    (when (and (getf field :display) 
			       (and (not (equalp (field-data-type field)
						 'data-group))
				    (not (equalp (field-data-type field)
						 'data-list))))
		      (monkey-html-lisp:htm 
			(:div :class "col"
				
			      (let ((val (print-item-val 
					  (field-data-type field) field item)))
				(if (> (length val) 100 )
				    (frmt "~A ..." (subseq val 0 100))
				    val)))))))

		(:div :class "col "
		      (:div :class "btn-group float-right"
			    (render-grid-buttons spec-name item ))))
	 
	  (if (and (or (and (equalp (parameter "action") "edit")
			    (parameter "item-id")) 
		       (gethash :validation-error-item-id
				(cache *context*)))
		   (string-equal (parameter "data-spec") (frmt "~S" spec-name))
		   (string-equal (or (parameter "item-id") 
				     (gethash :validation-error-item-id
					      (cache *context*)))
				 (frmt "~A" (xdb2:id item))))
				
	      (render-grid-edit spec-name fields item
				parent-item parent-spec sub-name))
	    
	  (when (equalp (ensure-parse-integer 
			 (get-context-data-spec-attribute spec-name
							  :expand-id)) 
			(xdb2:id item))
	      
	    (unless sub-level-p
	      (setf (gethash :root-item (cache *context*)) item))
	      
	    (dolist (sub subs)
	      (let* ((attributes (cdr (getf sub :db-type)))
		     (sub-data-spec (getf attributes :data-spec)))
		  
		(parse-data-spec-for-grid sub-data-spec)
		  
		(monkey-html-lisp:htm
		  (:div :class "row"
			(:div :class "col"
			      (:div :class "card"
				    (:h4 :class "card-title"
					 (frmt "~A" (string-capitalize
						     (getf sub :name))))
				    (:div :class "card-header"
					  (render-grid-header
					   sub-data-spec
					   (get-context-data-spec-attribute 
					    sub-data-spec :data-fields)
					     
					   t))
				    (:div :class "card-block"
					  (render-grid-data 
					   sub-data-spec
					   (item-val (field-data-type sub) sub item) 
					   (+ sub-level 1)
					   (getf sub :name)
					   item
					   spec-name
					   ))
				    (:div :class "card-footer"
					  (:div :class "row"	  
						(:div :class "col"
						      (:button ;;:tabindex -1 ;;when disabled
						       :name "new" :type "submit" 
						       :class "btn btn-outline-success"
							 
						       :aria-pressed "false"
							 
						       :onclick 
							 
						       (grid-js-render "cl-wfx:ajax-grid" 
								       spec-name
								       :action "new")
						       "+")))))))))))))
      
      (when (equalp (parameter "action") "new")
	(render-grid-edit spec-name fields 
			  (make-instance spec-name) 
			  parent-item
			  parent-spec
			  sub-name)))))

(defun render-grid-sizing (spec-name)
  (monkey-html-lisp:htm
    (:input :type "text" :name "pages" 
	    :size 2
	    :id "pages"
	    :value (or  (parameter "pages")
			(get-context-data-spec-attribute 
			 spec-name :show-pages-count)
			10)
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "pages"
	     13
	     "cl-wfx:ajax-grid"
	     "grid-table"
	     (js-pair "data-spec"
		      (frmt "~S" spec-name))

	     (js-pair
	      "grid-name" 
	      (frmt "~A" spec-name))
	     (js-pair "action" "grid-sizing")))))

(defun render-grid-search (spec-name)
  (monkey-html-lisp:htm
    (:input :type "text" 
	    :name "search" 	   
	    :id "search"
	    :value (or (parameter "search")
			     (get-context-data-spec-attribute 
			      spec-name :search)
			     "")
	    :onkeydown
	    ;;fires ajax call on enter (13)
	    (js-render-event-key 
	     "search"
	     13
	     "cl-wfx:ajax-grid"
	     "grid-table"
	     (js-pair "data-spec"
		      (frmt "~S" spec-name))
	     
	     (js-pair
	      "grid-name" 
	      (frmt "~A" 
		    (frmt "~A" spec-name)))
	     (js-pair "action" "grid-search")))))


(defun fetch-grid-page-data (spec-name items)
  
  (set-context-data-spec-attribute spec-name :data-count (length items))
   
 
  (set-context-data-spec-attribute spec-name :show-pages-count 
				   (if (not (empty-p (parameter "pages")))
				       (parse-integer (parameter "pages"))
				       (or (get-context-data-spec-attribute 
					    spec-name :show-pages-count)
					   10)))
    
  (set-context-data-spec-attribute spec-name :active-page
				   (if (not (empty-p (parameter "page")))
				       (parse-integer (parameter "page"))
				       (or (get-context-data-spec-attribute 
					    spec-name :active-page)
					   1)))


  (multiple-value-bind (page-count rem)
      (floor (get-context-data-spec-attribute spec-name :data-count) 
	     (get-context-data-spec-attribute spec-name :show-pages-count))
     
    (set-context-data-spec-attribute spec-name :page-count page-count)
    (set-context-data-spec-attribute spec-name :page-count-remaining rem))
    
  (set-context-data-spec-attribute 
   spec-name :start-page-count 
   (- (* (get-context-data-spec-attribute 
	  spec-name :active-page) 
	 (get-context-data-spec-attribute 
	  spec-name :show-pages-count)) 
      (get-context-data-spec-attribute 
       spec-name :show-pages-count)))
    
  (set-context-data-spec-attribute 
   spec-name :end-page-count 
   (if (< (* (get-context-data-spec-attribute 
	      spec-name :active-page)
	     (get-context-data-spec-attribute 
	      spec-name :show-pages-count)) 
	  (get-context-data-spec-attribute 
	   spec-name :data-count))
       (* (get-context-data-spec-attribute 
	   spec-name :active-page)
	  (get-context-data-spec-attribute 
	   spec-name :show-pages-count))))
  
  (when items
    (subseq items 
	    (get-context-data-spec-attribute spec-name :start-page-count) 
	    (get-context-data-spec-attribute spec-name :end-page-count))))

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

(defun filter-function (spec-name)
  (lambda (item)
		       
    (let ((found nil))
      (dolist (field (get-context-data-spec-attribute 
		      spec-name 
		      :filter-fields))
	(let ((filter-term 
	       (or
		(parameter 
		 (frmt "~A-filter" (getf field :name)))
		(get-context-data-spec-attribute 
		 spec-name (frmt "~A-filter" 
				 (getf field :name))))))
	  (when filter-term
			       
	    (when (getf field :db-type)
	      (when (or (equalp (field-data-type field) 'data-group)
			(equalp (field-data-type field) 'data-list))
		(dolist (sub-val (item-val (field-data-type field) field item))
		  (when sub-val
		    (let* ((full-type (cdr (getf field :db-type)))
			   (accessor (getf full-type :key-accessor))
			   (accessor-accessor (getf full-type :accessor-accessor))
			   (val (if (getf full-type :accessor-accessor)
				  (slot-value (slot-value sub-val accessor)
					      accessor-accessor))))
		      
		      (if (filter-found-p filter-term val)
			  (push t found)
			  (push nil found)
			  )))))
	      (let ((val (print-item-val 
			  (field-data-type field) field item)))
		(if (filter-found-p filter-term val)
		    (push t found)
		    (push nil found)))))))
			
						   
      (unless (found-nil-p found)
	item))))

(defun search-function (spec-name search-term)
  (lambda (item)
    
    (let ((found nil))
      (dolist (field (get-context-data-spec-attribute 
		      spec-name 
		      :data-fields))
	(when (getf field :db-type)
	  (when (or (equalp (field-data-type field) 'data-group)
		    (equalp (field-data-type field) 'data-list))
	    (dolist (sub-val (item-val (field-data-type field) field item))
		(when sub-val
		  (let* ((full-type (cdr (getf field :db-type)))
			 (accessor (getf full-type :key-accessor))	
			 (accessor-accessor (getf full-type :accessor-accessor))
			 (val (if (getf full-type :accessor-accessor)
				  (slot-value (slot-value sub-val accessor)
					      accessor-accessor))))
		    (when val
		      (when (search search-term 
				    val
				    :test #'string-equal)
			(unless found		       
			  
			  (setf found t))))))))
	  (let ((val (print-item-val 
		      (field-data-type field) field item)))
	    (when val
	      (when (search search-term 
			    val
			    :test #'string-equal)
		(unless found		       				     
		  (setf found t)))))))
      (when found
	item))))

(defun fetch-grid-data (spec-name)
  (let* ((items)
	 (collection-name (get-context-data-spec-attribute 
			   spec-name :collection-name))
	 (search-term (or (parameter "search") 
			  (get-context-data-spec-attribute 
			   spec-name :search)))
	 (search-p (not (empty-p search-term)))
	 (filter-p (get-context-data-spec-attribute 
				   spec-name :filter)))
    
    
    (unless (or search-p filter-p)
      (setf items (fetch-all collection-name
			     :result-type 'list)))
    
    (when (or search-p filter-p)
      
      (if (get-context-data-spec-attribute 
	   spec-name 
	   :filter-fields)
	  
	  (setf items
		(fetch-items  
		 collection-name
		 :test (filter-function spec-name)
		 :result-type 'list))
	  (setf items
		    (fetch-items  
		     collection-name
		     :test (search-function spec-name search-term)
		     :result-type 'list)))
  
      (when items
	(setf items
	      (items-from-items 
	       items
	       :test (search-function spec-name search-term)))))
    
    (fetch-grid-page-data spec-name items)))

(defun render-grid-paging (spec-name)
  
  (let ((active-page (get-context-data-spec-attribute 
		      spec-name :active-page))
	(how-many-rem (get-context-data-spec-attribute 
		       spec-name :page-count-remaining))
	(how-many-pages (get-context-data-spec-attribute 
		       spec-name :page-count)))
    
    (monkey-html-lisp:htm
      (:nav
       (:ul :class "pagination justify-content-end"
	    
	    (:li :class "page-item"
		 (:button ;;:tabindex -1 ;;when disabled
		  :name "page" :type "submit" 
		  :class (if (> active-page 1)
			     
			     "btn page-link"
			     "btn page-link disabled"
			     )
		  
		  :onclick 
		  (js-render "cl-wfx:ajax-grid"
			     "grid-table"
			     (js-pair "data-spec"
				      (frmt "~S" spec-name))
			     (js-pair "pages"
				      (or (parameter "pages") 10))
			     (js-pair "page"
				      (if (> active-page 1)
					  (- active-page 1)))
			     (js-pair "grid-name" 
				      (frmt "~A" 
					    spec-name))
			     (js-pair "action" "page-previous"))
		  "Previous"))
	    
	    (let ((real-page-count (if (>  how-many-rem 0)
				       (+ how-many-pages 1)
				       how-many-pages
				       )))
	      (dotimes (i real-page-count)
		(monkey-html-lisp:htm
		  (:li :class (if (equalp active-page (+ i 1))
				  "page-item active"
				  "page-item")
		       (:button 
			:name "page" :type "submit" 
			:class "btn page-link"
			
			:onclick 
			(js-render "cl-wfx:ajax-grid"
				   "grid-table"
				   (js-pair "data-spec"
					    (frmt "~S" spec-name))
				   (js-pair "pages"
					    (or (parameter "pages") 10))
				   (js-pair "page"
					    (+ i 1))
				   (js-pair "grid-name" 
					    (frmt
					     "~A" 
					     spec-name))
				   (js-pair "action" "page"))
			(+ i 1)))))
	      
	      (monkey-html-lisp:htm
		(:li :class "page-item"
		     (:button ;;:tabindex -1
		      :name "page" :type "submit" 
		      :class (if (< active-page real-page-count)
				 
				 "btn page-link"
				 "btn page-link disabled")
		      
		      :onclick 
		      (js-render "cl-wfx:ajax-grid"
				 "grid-table"
				 (js-pair "data-spec"
					  (frmt "~S" spec-name))
				 (js-pair "pages"
					  (or (parameter "pages") 10))
				 (js-pair "page"
					  (if (< active-page real-page-count)
					      (+ active-page 1)))
				 (js-pair "grid-name" 
					  (frmt
					   "~A" 
					   spec-name))
				 (js-pair "action" "page-next"))
		      "Next")))))))))



(defun render-grid (spec-name) 
  
  (parse-data-spec-for-grid spec-name)
  
  (setf (gethash :root-data-spec (cache *context*)) spec-name)
  
  (setf (gethash :root-item (cache *context*)) nil)
  
  (set-context-data-spec-attribute spec-name :list-field-name nil)
  
  (set-context-data-spec-attribute spec-name
				   :search (or (parameter "search")
					       (get-context-data-spec-attribute 
						spec-name :search)))
  (set-context-data-spec-attribute spec-name
				     :filter t)
  
  
  (when (equalp (parameter "action") "filter")
    (set-context-data-spec-attribute spec-name
				     :filter t))
  
  (when (equalp (parameter "action") "un-filter")
    (set-context-data-spec-attribute spec-name
				     :filter nil))

  (when (equalp (parameter "action") "grid-col-filter")
    (let ((fields (get-context-data-spec-attribute 
		      spec-name :filter-fields)))
      (dolist (field (get-context-data-spec-attribute 
		      spec-name :data-fields))
	(when (parameter (frmt "~A-filter" (getf field :name)))
	  
	  (pushnew field fields)
	  
	  
	  (set-context-data-spec-attribute 
	   spec-name (frmt "~A-filter" (getf field :name))
	   (parameter (frmt "~A-filter" (getf field :name))))))
      
      (set-context-data-spec-attribute 
	 spec-name :filter-fields fields)))
  
  
  (when (equalp (parameter "action") "expand")
    
    (set-context-data-spec-attribute (parameter "data-spec")
				     :expand-id (parameter "item-id")))
  
  (when (equalp (parameter "action") "unexpand")    
    (set-context-data-spec-attribute (parameter "data-spec")
				     :expand-id nil))
 
  (let ((page-items (fetch-grid-data spec-name)))
    
    (monkey-html-lisp:with-html	  
      (:div :class "card"
		  (:h4 :class "card-title"
			 (frmt "~A" (name (context-spec *context*)))
			 (:button :class "float-right"
				     :name "export" 
				     :type "submit" 
				     :class "btn btn-small btn-outline-success"
				     :aria-pressed "false"
				     :onclick 
				     (grid-js-render "cl-wfx:ajax-grid" 
						     spec-name
						     :action "export")
				     "Export")
			 (when (string-equal (frmt "~A" spec-name) "company")
			   (let ((campaigns (fetch-all "campaigns" :result-type 'list)))
			     (monkey-html-lisp:htm
			       (:button :class "float-right"
					:name "assign-campaign" 
					:type "submit" 
					:class "btn btn-small btn-outline-success"
					:aria-pressed "false"
					:onclick 
					(grid-js-render "cl-wfx:ajax-grid" 
							spec-name
							:action "assign-campain")
					"<>")
			       
			       (:select  :class "float-right"
				:name "campaign"
				(dolist (option campaigns)
				  (monkey-html-lisp:htm
				    (:option :value (frmt "~S" (xdb2::id option)) 
					     (frmt "~A" 
						   (slot-value 
						    option 
						    (intern "NAME" 'cl-bizhub)))))))
			       ))))
		  (:div :class "card-header"
			(render-grid-header spec-name
					    (get-context-data-spec-attribute 
					     spec-name 
					     :data-fields)
					    nil))
		  (:div :class "card-block"
			(render-grid-data spec-name page-items 0 nil nil nil))
	
		  (:div :class "card-footer"
			(:div :class "row"	  
			      (:div :class "col"
				    (:button ;;:tabindex -1 ;;when disabled
				     :name "expand" :type "submit" 
				     :class "btn btn-outline-success"
				  
				     :aria-pressed "false"
				
				     :onclick 
				     (grid-js-render "cl-wfx:ajax-grid" 
						     spec-name 
						     
						     :action "new")
				     "+")
				    (render-grid-paging spec-name)))))
      )))


(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (render-grid (gethash :root-data-spec (cache *context*))))


(defmethod action-handler ((action (eql :save)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (let* ((spec-name (read-symbol-from-string (parameter "data-spec")))
	 (fields (get-context-data-spec-attribute 
		  spec-name
		  :data-fields))
	 (parent-slot (get-context-data-spec-attribute 
				spec-name :list-field-name)))
    
    (setf (gethash :validation-errors (cache *context*)) nil)
    (setf (gethash :validation-error-item-id (cache *context*)) nil)
       
    (when fields
      (let ((item (get-context-data-spec-attribute 
			 spec-name
			 :edit-item))
	    (parent-item (get-context-data-spec-attribute 
			  spec-name
			  :parent-item)))
	
	(unless item
	  (setf item (make-instance spec-name)))
	
	(dolist (field fields)
	  (when (and (getf field :editable)
			 (getf field :db-type)
			 (and
			  (not (equalp (field-data-type field) 'data-group))
			  (not (equalp (field-data-type field) 'data-list))))
		(let ((valid (if (equalp (field-data-type field) 'list-item)
				 (validate-item-val (field-data-type field) 
						    field 
						    item 
						    (parameter (getf field :name)))
				 (list t nil))))
		  (unless (first valid)
		    (pushnew 
		     (list (getf field :name) (second valid))
		     (gethash :validation-errors (cache *context*))))
		  
		  (when (first valid)
		    (set-item-val (field-data-type field) field item 
				  (parameter (getf field :name)))))))
	
	;;TODO: is this still needed????
	;;Doing this to keep edit window open.
	(when (gethash :validation-errors (cache *context*))
	  (setf (gethash :validation-error-item-id
			 (cache *context*))
		(parameter "data-id")))
	
	(unless (gethash :validation-errors (cache *context*))
	  
	  (dolist (field fields)
	    (when (and (getf field :editable)
		       (getf field :db-type)
		       (and (not (equalp (field-data-type field) 'data-group))
			    (not (equalp (field-data-type field) 'data-list))))
		(set-item-val (field-data-type field) field item 
			      (parameter (getf field :name)))))
	 
	  ;;Append parent-slot only if new
	  (when (and parent-slot (not (xdb2:id item)))
	    
	    (setf (slot-value parent-item parent-slot)
		  (append (slot-value parent-item parent-slot)
			  (list item))))
	  
	  
	  (persist-data (gethash :root-item (cache *context*))
			    :collection-name (get-context-data-spec-attribute 
					      (gethash :root-data-spec (cache *context*))
					      :collection-name))
	  
	  (set-context-data-spec-attribute spec-name :parent-item nil)
	  (set-context-data-spec-attribute spec-name :edit-item nil)
	  (set-context-data-spec-attribute spec-name :item-id nil))))))


