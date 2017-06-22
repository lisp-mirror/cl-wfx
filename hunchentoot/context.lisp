(in-package :cl-wfx)

(defgeneric item-val (type item-def data)
  (:documentation "Retrieves and wrap for display data according to item definition."))


(defmethod item-val ((type (eql 'string)) item-def item)
  (let* ((name (getf item-def :name)))
    (slot-value item name)))

(defmethod item-val ((type (eql 'script)) item-def item)
  (let* ((name (getf item-def :name))
	(def (car (slot-value item name))))
    ;;(break "def ~A ~A" (second def) (third def))
    def))



(defclass context-data-spec-processor (monkey-lisp:processor)
  ()
  )

(monkey-lisp::define-monkey-macro shit-shat (&key id from-ajax)
  `(:div "No idea wtf?????????????????"
	 (:div ,id ,from-ajax)))

(monkey-lisp::define-monkey-macro render-page (&body body)
  
  `(:html
	"<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css\" integrity=\"sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ\" crossorigin=\"anonymous\">"
	
	"<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js\"></script>"
	"<link rel=\"stylesheet\" href=\"https://cdn.datatables.net/1.10.13/css/jquery.dataTables.min.css\">"
	"<script src=\"https://cdn.datatables.net/1.10.1/js/jquery.dataTables.min.js\"></script>"
	" <script src=\"https://cdnjs.cloudflare.com/ajax/libs/tether/1.2.0/js/tether.min.js\" integrity=\"sha384-Plbmg8JY28KFelvJVai01l8WyZzrYWG825m+cZ0eDDS1f7d/js6ikvy1+X+guPIB\" crossorigin=\"anonymous\"></script>"
	"<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js\" integrity=\"sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn\" crossorigin=\"anonymous\"></script>"
	
	"<script src=\"../web/codemirror/lib/codemirror.js\"></script>
<link rel=\"stylesheet\" href=\"../web/codemirror/lib/codemirror.css\">
<script src=\"../web/codemirror/mode/commonlisp/commonlisp.js\"></script>
<script src=\"../web/codemirror/addon/edit/closebrackets.js\"></script>
<script src=\"../web/codemirror/addon/edit/matchbrackets.js\"></script>
<script src=\"../web/cl-wfx.js\"></script>
<link rel=\"stylesheet\" href=\"../web/cl-wfx.css\">
<link href=https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/css/bootstrap-datepicker3.standalone.min.css' rel='stylesheet>
 <script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/js/bootstrap-datepicker.min.js></script>
"
	(:body 
	 (:input :type "hidden" :id "contextid" :value (context-id *context*))
	 (:div :class "container"
	       ,@body)
	 
	 (:input :type "hidden" :id "huh" :value "shit fuch")
	 (:div :id "shit-id"
	       "fuck " "fuck"
	       )
	 (:button :id "testbutton" 
		  :class "btn-submit btn"
		  :type "submit"
		  :onclick
		  (js-render "cl-wfx:shit-shat"
			     "shit-id"
			     (js-value "huh")
			     (js-pair "grid-name" "fuck-knows")
			     (js-pair "action" "copy-scripts")
			    
			     )
		  "Render Shit")

	 (:script ,(frmt	      
"function ajax_call(func, callback, args, widget_args) {

    var uri = '~Aajax/' + encodeURIComponent(func);
    var post_parameters = '&contextid=' + contextid.value;
    var i;
    if (args.length > 0) {
        uri += '?';
        for (i = 0; i < args.length; ++i) {
            if (i > 0)
                uri += '&';
            uri += 'arg' + i + '=' + encodeURIComponent(args[i]);
        }
    }

    if (widget_args && widget_args.length > 0) {
        for (i = 0; i < widget_args.length; ++i) {
            var arg = widget_args[i]
            post_parameters += '&' + encodeURIComponent(arg[0]) + '='
                + encodeURIComponent(arg[1]);
        }
    }

    fetchURI(uri, callback, post_parameters);

}" (site-url *system*))))))

(defun render-grid-edit (item)
  (monkey-html-lisp:htm
    (:div :class "card"
	  (:div :class "card-header"
		"Edit")
	  (:div :class "row"
				     
		(dolist (slot (gethash 'fields monkey-lisp::*sexp-cache*))
		  (let* ((name (getf slot :name))
			 (label (getf slot :label)))
					 
		    (monkey-html-lisp:htm
					   
		      (:div :class "col-sm-1"
			    (if label
				label
				(frmt "~A" name)))
		      (:div :class "col-sm-5"
			    (cond
			      ((equalp (getf slot :db-type) 'string)
			       (monkey-html-lisp:htm
				 (:input :name name :type "text"
					 :value
					 (frmt "~S" (item-val (getf slot :db-type) 
							      slot 
							      item)))))
			      ((equalp (getf slot :db-type) 'script)
			       (monkey-html-lisp:htm
				 (:textarea 
				  :name name :cols 50 :rows 5
				  (frmt "~S" (slot-value item name)))))
			      (t
			       (monkey-html-lisp:htm
				 (:textarea 
				  :name name :cols 50 :rows 5
				  (frmt "~S" (slot-value item name)))))))))))
	  (:div :class "card-footer"
		(:a :href "#" :class "card-link" "Save")))))

(defun render-grid-buttons (item permissions active-page)
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
				  "data-spec")
			 (js-pair "item-id"
				  (xdb2::id item))
			 (js-pair "pages"
				  (or (parameter "pages") 10))
			 (js-pair "page"
				  active-page)
			 (js-pair "grid-name" 
				  (frmt "~A" 
					(gethash 'data-spec-name 
						 monkey-lisp::*sexp-cache*)))
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
				  "Data Spec")
			 (js-pair "item-id"
				  (xdb2::id item))
			 (js-pair "pages"
				  (or (parameter "pages") 10))
			 (js-pair "page"
				  active-page)
			 (js-pair "grid-name" 
				  (frmt "~A" 
					(gethash 'data-spec-name 
						 monkey-lisp::*sexp-cache*)))
			 (js-pair "action" "delete"))
	      "Delete"))))))

(defun render-grid-data (page-data permissions active-page)
  (dolist (item page-data)
		(monkey-html-lisp:htm
		  (:div :class "row"
			(dolist (slot (gethash 'fields monkey-lisp::*sexp-cache*))
			
			  (let* ((name (getf slot :name)))
			    (monkey-html-lisp:htm 
			     
			      (:div :class "col"
				    (cond
				      ((getf slot :db-type)
				       
				       (let ((val (if (equalp (getf slot :db-type) :script)
						      (frmt "~S"
							    (item-val (getf slot :db-type) 
								      slot item))
						      (frmt "~A"
							    (item-val (getf slot :db-type) 
								      slot item))
						      )))
					 (if (> (length val) 50 )
					     (if (equalp (getf slot :db-type) :script)
						 (frmt "~S ..." (subseq val 0 50))
						 (frmt "~A ..." (subseq val 0 50)))
					     val)
					 ))
				      (t
				       (let ((val (frmt "~A"
							(slot-value item name))))
					 (if (> (length val) 50 )
					     (frmt "~A ..." (subseq val 0 50))
					     val)
					 )
				       ))))))
		
			(:div :class "col"
			      (render-grid-buttons item permissions active-page )
			      ))


		  (if (and (parameter "item-id")
			   (string-equal (parameter "item-id") (frmt "~A" (xdb2:id item))))
		      (render-grid-edit item)))))

(defun render-grid-paging (active-page how-many-rem how-many-pages)
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
				    "data-spec")
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
					  "data-spec")
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
					"data-spec")
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



(defun render-grid (&key id from-ajax permissions)
  (declare (ignore id) (ignore from-ajax) )
  (let* ((data (fetch-all (gethash 'collection-name monkey-lisp::*sexp-cache*)))
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
		  (:div :class "card-header"
			(:div :class "row"	
			      
			      (dolist (slot (gethash 'fields monkey-lisp::*sexp-cache*))
				
				(let* ((name (getf slot :name))
				       (label (getf slot :label)))
				  
				  (monkey-html-lisp:htm
				    (:div :class "col"
					  (:h6 (if label
						   label
						   (format nil "~A" name)))))))
			      
			      (:div :class "col" ;;column spacing for buttons
				    (:div :class "row"	  
					  (:div :class "col"
						(:input :type "text" :name "pages" :id "pages"
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
								  "data-spec")
				
							 (js-pair "page" "1")
							 (js-pair "grid-name" 
								  (frmt "~A" 
									(gethash 'data-spec-name 
										 monkey-lisp::*sexp-cache*)))
							 (js-pair "action" "grid-sizing"))))))))
		  
		  (let ((page-data (subseq data start-count end-count )))
		    (render-grid-data page-data permissions active-page))
		  
		  (:div :class "card-footer"
			(:div :class "row"	  
			      (:div :class "col"					 
				    (render-grid-paging active-page how-many-rem how-many-pages)))))))))




(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :name))
					  attribute-value)
  (setf (gethash 'data-spec-name monkey-lisp::*sexp-cache*) 
	attribute-value))


(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :collection-name))
					  attribute-value)
  ;;(break "collection-name")
  (setf (gethash 'collection-name monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:process-attribute ((processor context-data-spec-processor) 
					  (tag (eql :data-spec)) 
					  (attribute-tag (eql :data-fields))
					  attribute-value)
  
  (setf (gethash 'fields monkey-lisp::*sexp-cache*) attribute-value))

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						 attributes body)
  (render-grid :permissions '(:update :delete)))


(defun ajax-grid (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (let ((data-spec (get-data-spec (parameter "data-spec"))))
    (monkey-lisp::monkey-lisp* 
     :processor-class 'cl-wfx::context-data-spec-processor
     :body
     (first (script data-spec)))))

(defmethod setup-context ((module module) (spec context-spec) system)  
  (eval
   `(hunchentoot:define-easy-handler (,(alexandria:symbolicate 
					(string-upcase (id-string (name spec))) 
					'-page)  
				       :uri ,(frmt "~A~A/~A" (site-url system) 
						   (string-downcase 
						    (id-string (if module
								   (module-short module)
								   "sys"
								   )))
						   (if (url spec)
						       (url spec)
						       (string-downcase 
							(id-string (name spec)))
						       ))  
				       :allow-other-keys t) ,(args spec)
      
     ;; (break "~S~%~S~%~S" ,spec ,(get-data-spec (third (first (script spec)))) ',(third (first (script spec))))
      
      (let* ((shit (monkey-lisp:monkey-lisp 
		       (:processor-class cl-wfx::context-data-spec-processor)
		     ,(first (script (get-data-spec (third (first (script spec)))))))))
	
	(monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page shit))))))

