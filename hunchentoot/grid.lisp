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



(defmethod print-item-val ((type (eql 'boolean)) field item &key &allow-other-keys)
  (if (item-val (field-data-type field) 
		field 
		item)
      "checked"
      ""))


(defmethod print-item-val ((type (eql 'list)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

(defmethod print-item-val ((type (eql 'data-group)) field item &key &allow-other-keys)
   (print-item-val-s* field item))

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
		   item
		  )
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
					     :disabled "disabled"
					     )))
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
					   
					     )))
		)))))

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

(defmethod render-input-val ((type (eql 'data-group)) field item &key &allow-other-keys)
  (render-input-val* type field item))

(defun render-grid-edit (spec-name fields item active-page)

  (monkey-html-lisp:htm
    (:div :class "card"
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
				      
				      (when (getf field :display)
					
					(monkey-html-lisp:htm
					  
					  (:div :class (if (getf field :editable)
							   "form-group row"
							   "form-group row disabled"
							   )
						(:label :for name :class "col-sm-2 col-form-label" 
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
						       field item))))))))))
		(:div :class "card-footer"
		      (:button ;;:tabindex -1 ;;when disabled
		       :name "save" 
		       
		       :type "submit" 
		       :class "btn btn-outline-primary btn-sm"
		       
		       :onclick 
		       (js-render-form-values "cl-wfx:ajax-grid"
					      "grid-table"
					      (frmt "~A" 
						    (gethash 'data-spec-name 
							     monkey-lisp::*sexp-cache*))
					      (js-pair "data-spec"
						       (frmt "~S" spec-name))
					      (js-pair "data-id"
						       (if (xdb2::id item)
							   (xdb2::id item)
							   "0"
							   ))
					      
					      (js-pair "pages"
						       (or (parameter "pages") 10))
					      (js-pair "page"
						       active-page)
					      (js-pair "action" "save")
					      
					      )
		       
		       
		       
		       "Save")))))

(defun render-grid-buttons (spec-name item permissions active-page
			    expand-row-spec expand-row-id grid-name)
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
	      (js-render "cl-wfx:ajax-grid"
			 "grid-table"
			 (js-pair "data-spec"
				  (frmt "~S" spec-name))
			 (js-pair "item-id"
				  (xdb2::id item))
			 
			 (js-pair "pages"
				  (or (parameter "pages") 10))
			 (js-pair "page"
				  active-page)
			 (js-pair "search"
				  (if (parameter "search")
				      (parameter "search")
				      ""))
			 (js-pair "grid-name" 
				  (frmt "~S" 
					(or grid-name spec-name)))
			 (js-pair "expand-row-id"
				  (if expand-row-id
				      (frmt "~A" expand-row-id)
				      ""
				      ))
			 (js-pair "expand-row-spec"
				  (if expand-row-spec
				      (frmt "~A" expand-row-spec)
				      ""
				      ))
			 (js-pair "action" "edit"))
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
	      (js-render "cl-wfx:grid"
			 "grid-table"
			 (js-pair "data-spec"
				  (frmt "~S" spec-name))
			 (js-pair "item-id"
				  (xdb2::id item))
			 
			 (js-pair "pages"
				  (or (parameter "pages") 10))
			 (js-pair "page"
				  active-page)
			 (js-pair "search"
				  (if (parameter "search")
				      (parameter "search")
				      ""))
			 (js-pair "grid-name" 
				  (frmt "~S" 
					(or grid-name spec-name)))
			 (js-pair "expand-row-id"
				  (if expand-row-id
				      (frmt "~A" expand-row-id)
				      ""
				      ))
			 (js-pair "action" "delete"))
	      "Delete"))))))


(defun render-expand-buttons (subs spec-name item active-page grid-name)
  (if subs
      (if (and (parameter "expand-row-id") (not (equalp (parameter "expand-row-id") ""))
	       (equalp (parse-integer (parameter "expand-row-id")) 
		       (xdb2:id item)))
	  (monkey-html-lisp:htm
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm active"
				 
	     :aria-pressed "true"
				 
	     :onclick 
	     (js-render "cl-wfx:ajax-grid"
			"grid-table"
			(js-pair "data-spec"
				 (frmt "~S" spec-name))
					   
			(js-pair "pages"
				 (or (parameter "pages") 10))
			(js-pair "page"
				 active-page)
			(js-pair "search"
				 (if (parameter "search")
				     (parameter "search")
				     ""))
			(js-pair "grid-name" 
				 (frmt "~S" (or grid-name spec-name))))
	     "-"))
	  (monkey-html-lisp:htm
	    (:button ;;:tabindex -1 ;;when disabled
	     :name "expand" :type "submit" 
	     :class "btn btn-outline-primary btn-sm  border-0"
				  
	     :aria-pressed "false"
				
	     :onclick 
	     (js-render "cl-wfx:ajax-grid"
			"grid-table"
			(js-pair "data-spec"
				 (frmt "~S" spec-name))
			(js-pair "expand-row-id"
				 (xdb2::id item))
			(js-pair "expand-row-spec"
				 (frmt "~S" spec-name))
			(js-pair "pages"
				 (or (parameter "pages") 10))
			(js-pair "page"
				 active-page)
			(js-pair "search"
				 (if (parameter "search")
				     (parameter "search")
				     ""))
			(js-pair "grid-name" 
				 (frmt "~S" (or grid-name spec-name))))
	     "+")
	    ))
      )
  )

(defun render-grid-data (spec-name page-data permissions active-page sub-level grid-name)
    
  (let ((data-spec (get-data-spec spec-name))
	(subs))

    (dolist (field (data-spec-fields data-spec))
      (cond ((equalp (field-data-type field) 'data-group)
	     (pushnew field subs))))
    
    (dolist (item page-data)
	(monkey-html-lisp:htm
	  (:div :class "row"
		(monkey-html-lisp:htm
		  
		  (:div :class "col-sm-1"
			(render-expand-buttons subs spec-name item active-page 
					       grid-name))
		  
		  (dolist (field (data-spec-fields data-spec))
		    (when (getf field :display)
		      (monkey-html-lisp:htm 
			
			(:div :class "col"
			      
			      (let ((val (print-item-val 
					  (field-data-type field) field item)))
				(if (> (length val) 100 )
				    (frmt "~A ..." (subseq val 0 100))
				    val)
				)
			      )))))
		
		
		(:div :class "col"
		      (render-grid-buttons 
		       spec-name item permissions active-page 
		       (parameter "expand-row-spec")
		       (parameter "expand-row-id")
		       grid-name)))


	  (if (and (parameter "item-id")
		   (string-equal (parameter "data-spec") (frmt "~S" spec-name))
		   (string-equal (parameter "item-id") (frmt "~A" (xdb2:id item))))
	      (render-grid-edit spec-name (data-spec-fields data-spec) item active-page))
	  
	
	  (when (and (parameter "expand-row-id") 
		     (not (equalp (parameter "expand-row-id") "")))
	    (when (equalp (parse-integer (parameter "expand-row-id")) (xdb2:id item))
	      (dolist (sub subs)
		(let* ((attributes (cdr (getf sub :db-type)))
		       (data-spec (get-data-spec (getf attributes :data-spec))))
		  (monkey-html-lisp:htm
		    (:div :class "row"
			  (:div :class "col"
				(:div :class "card"
				      (:h4 :class "card-title"
					   (frmt "~A" (string-capitalize
						       (getf sub :name)))
					   )
				      (:div :class "card-header"
					    (render-grid-header 
					     (data-spec-fields data-spec) 
					     (getf attributes :data-spec)
					     t))
				      (:div :clss "card-block"
					    (render-grid-data 
					     (getf attributes :data-spec) 
					     (item-val (field-data-type sub) sub item) 
					     permissions
					     active-page
					     (+ sub-level 1)
					     grid-name))))))))))
	  
	  
	  ))
    
    (when (parameter "new-item-id")
      
      (render-grid-edit spec-name (data-spec-fields data-spec) 
			(make-instance (name data-spec)) active-page))
    ))

(defun render-grid-paging (spec-name active-page how-many-rem how-many-pages)
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
					    (gethash 
					     'data-spec-name 
					     monkey-lisp::*sexp-cache*)))
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
					     (gethash 
					      'data-spec-name 
					      monkey-lisp::*sexp-cache*)))
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
					   (gethash 
					    'data-spec-name 
					    monkey-lisp::*sexp-cache*)))
				 (js-pair "action" "page-next"))
		      "Next"))))))))

(defun render-grid-sizing (spec-name)
  (monkey-html-lisp:htm
    (:input :type "text" :name "pages" 
	    :size 2
	    :id "pages"
	    :value (if (parameter "pages")
		       (parameter "pages")
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
	     (js-pair "page" "1")
	     (js-pair "search"
		      (if (parameter "search")
			  (parameter "search")
			  ""))
	     (js-pair
	      "grid-name" 
	      (frmt "~A" 
		    (gethash 
		     'data-spec-name 
		     monkey-lisp::*sexp-cache*)))
	     (js-pair "action" "grid-sizing")))))

(defun render-grid-search (spec-name)
  (monkey-html-lisp:htm
    (:input :type "text" 
	    :name "search" 	   
	    :id "search"
	    :value (if (parameter "search")
		       (parameter "search")
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
	     (js-pair "page" "1")
	     
	     (js-pair
	      "grid-name" 
	      (frmt "~A" 
		    (gethash 
		     'data-spec-name 
		     monkey-lisp::*sexp-cache*)))
	     (js-pair "action" "grid-search"))))
  )

(defun fetch-grid-data ()
  (if (or (not (parameter "search")) 
	  (< (length (string-trim (list #\Space)  (parameter "search"))) 1))
      
      (coerce  (fetch-all (gethash 'collection-name monkey-lisp::*sexp-cache*))
	       'list)
      (fetch-items  
       (gethash 'collection-name monkey-lisp::*sexp-cache*)
       :test (lambda (item)
	       
	       (let ((found nil))
		 (dolist (field (gethash 'fields monkey-lisp::*sexp-cache*))
		   (let ((val (print-item-val (field-data-type field) field item)))
		     
		     (when (search (parameter "search") 
				   val
				   :test #'string-equal)
		       (unless found
			 (setf found t)))))
		 (if found
		     item)))
       :result-type 'list
       )))

(defun render-grid-header (fields spec-name sub-p)
  (monkey-html-lisp:htm
    (:div :class "row"	
			      
	  (dolist (field fields)
				
	    (when (getf field :display)
	      (let* ((name (getf field :name))
		     (label (getf field :label)))
				    
		(monkey-html-lisp:htm
		  (:div :class "col"
			(:h6 (if label
				 label
				 (string-capitalize 
				  (substitute #\Space  (character "-")  
					      (format nil "~A" name) 
					      :test #'equalp)))))))))
	  (unless sub-p
	    (monkey-html-lisp:htm	      
	      (:div :class "col" ;;column spacing for buttons
		    (:div :class "row"	  
			  (:div :class "col"
				(render-grid-search spec-name)
				(render-grid-sizing spec-name)
				)))))))
  )

(defun render-grid (&key id from-ajax permissions)
  (declare (ignore id) (ignore from-ajax) )

  (let* ((data (fetch-grid-data))
	 (spec-name (gethash 'data-spec-name monkey-lisp::*sexp-cache*) )
	 (data-count (length data))	 
	 (how-many-pages nil)
	 (how-many-rem 0)
	 (active-page (if (parameter "page")
			  (parse-integer (parameter "page"))
			  1
			  ))
	 (pages (if (parameter "pages")
		    (parse-integer (parameter "pages"))
		    10))
	 (start-count)
	 (end-count))
   
    
    (multiple-value-bind (page-count rem)
	(floor data-count
	       pages)
     
      (setf how-many-pages page-count)
      (setf how-many-rem rem)
      
      (setf start-count (- (* active-page pages) pages))
      (setf end-count (if (< (* active-page pages) data-count)
			  (* active-page pages))))
    
    
    (monkey-html-lisp:with-html	       
      (:div :id "grid-table"
	    (:div :class "card"
		  
		  (:h4 :class "card-title"
		       (frmt "~A" (name (context-spec *context*))))
		  (:div :class "card-header"
			(render-grid-header (gethash 'fields monkey-lisp::*sexp-cache*)
					    spec-name nil))
		  (:div :class "card-block"
			(let ((page-data (subseq data start-count end-count )))
			  (render-grid-data spec-name page-data permissions active-page 0
					    spec-name
					    )))
		  
		  (:div :class "card-footer"
			(:div :class "row"	  
			      (:div :class "col"
				    (:button ;;:tabindex -1 ;;when disabled
				     :name "expand" :type "submit" 
				     :class "btn btn-outline-success"
				  
				     :aria-pressed "false"
				
				     :onclick 
				     (js-render "cl-wfx:ajax-grid"
						"grid-table"
						(js-pair "data-spec"
							 (frmt "~S" spec-name))
					
						(js-pair "pages"
							 (or (parameter "pages") 10))
						(js-pair "page"
							 active-page)
						(js-pair "search"
							 (if (parameter "search")
							     (parameter "search")
							     ""))
						(js-pair "new-item-id" "0")
						(js-pair
						 "grid-name" 
						 (frmt "~S" 
						       (or (read-symbol-from-string
							    (parameter "grid-name")) 
							   spec-name)))
						(js-pair "action" "new")
						)
				     "+")
				    (render-grid-paging spec-name active-page 
							how-many-rem how-many-pages)))))))))


(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let ((data-spec (get-data-spec 
		    (read-symbol-from-string (or (parameter "grid-name") 
						 (parameter "data-spec"))))))
   
    (monkey-lisp::monkey-lisp* 
     :processor-class 'cl-wfx::context-data-spec-processor
     :body
     (data-spec-script data-spec))))


(defmethod action-handler ((action (eql :save)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (let* ((data-spec (get-data-spec (read-symbol-from-string (parameter "data-spec"))))
	 (fields (data-spec-fields data-spec)))

    (when fields
      
      (let ((item (fetch-item (collection-name data-spec)
		  :test (lambda (item)
			  (equalp (parse-integer (parameter "data-id")) 
				  (xdb2::id item))))))
	(unless item
	  (setf item (make-instance (name data-spec))))
	
	(dolist (field fields)
	  (when (getf field :editable)
	    (when (getf field :db-type)
		(set-item-val (field-data-type field) field item 
			      (parameter (getf field :name))))))
	
	(persist-data item)))))


