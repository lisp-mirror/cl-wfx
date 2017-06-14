(in-package :cl-wfx)

#|

(defmethod render ((renderer (eql 'shit)) &key &allow-other-keys)

  (with-html (cl-who:str "Display some shit"))
  )



(defun parse-slots-grid (sexp)
  (let ((slots))
    (dolist (slot sexp)
      (let ((name (first slot))
	    (attributes (cdr slot)))

	(setf slots (pushnew `(,name 
			       :initarg ,(getf attributes :initarg)
			       :accessor ,(getf attributes :accessor)
			       :initform ,(getf attributes :initform)
			       :key ,(getf attributes :key)
			       :db-type ,(getf attributes :db-type))
			     slots))))
    slots))

(defgeneric item-val (type item-def data)
  (:documentation "Retrieves and wrap for display data according to item definition."))


(defmethod item-val ((type (eql 'string)) item-def item)
  (let* ((name (car item-def)))
    (slot-value item name)))

(defmethod item-val ((type (eql 'script)) item-def item)
  (let* ((name (car item-def))
	(def (car (slot-value item name))))
    (frmt "~A ~A" (second def) (third def))))




(defmethod render ((renderer (eql 'grid)) &key (data-spec (parameter "data-spec")) 
					    class-def &allow-other-keys)

  (let* ((data-spec (if data-spec 
			(get-data-spec data-spec)
			(get-data-spec (parameter "data-spec"))
			))
	 (permissions (permissions (context-spec *context*)))
	 (class-def (or class-def (cdr (first (script data-spec)))))
	 (class-name (first class-def))
	 ;;	(classes (second body))
	 (slots (third class-def))
	 ;;	(class-options (cdr (cdr (cdr body))))
	 (data (fetch-data  class-name 
			    :test (lambda (item)
				    item)
			    :result-type 'list))
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
	 (end-count)

	 )
   
    
    (multiple-value-bind (page-count rem)
	(floor data-count
	       pages)
     
      (setf how-many-pages page-count)
      (setf how-many-rem rem)
      
      (setf start-count (- (* active-page pages) pages))
      (setf end-count (if (< (* active-page pages) data-count)
			  (* active-page pages))))
    
    (with-html
      
		       
      (:div :id "grid-table"
	    
	    (:div :class "row"	  
		  (:div :class "col"
			(:input :type "text" :name "pages" :id "pages"
				:value (if (parameter "pages")
					   (parameter "pages")
					   10)
				:onkeydown
				
				(js-render-event-key 
				 "pages"
				 13
					   
				 "cl-wfx:grid"
				 "grid-table"
				 (js-pair "data-spec"
					  "Data Spec")
				
				 (js-pair "page" "1")
				 (js-pair "grid-name" 
					  (frmt "~A" class-name))
				 (js-pair "action" "grid-sizing"))
				)))
	    
	    (:div :class "row table-active"		 	    
		  (dolist (slot slots)
		    (let* ((slot-attributes (cdr slot))
			   (name (car slot))
			   (label (getf slot-attributes :label)))
		      (cl-who:htm
		       (:div :class "col"
			     (:h6
			      (cl-who:str (if label
					      label
					      name)))))))
		  
		  )
			     
	    (let ((page-data (subseq data start-count end-count )))
	      (dolist (item page-data)
		(cl-who:htm
		 (:div :class "row"
		       (dolist (slot slots)
			 (let* ((slot-attributes (cdr slot))
				(name (car slot)))
			   (cl-who:htm 
			    ;;(break "~A" (getf slot-attributes :db-type))
			    (:div :class "col"
				  (cond
				    ((getf slot-attributes :db-type)
				     (cl-who:str 
				      (item-val (getf slot-attributes :db-type) slot item)))
				    (t
				     (cl-who:str (slot-value item name))))))))
		       (dolist (permission permissions)
			 (cond ((equalp permission :update)
				(cl-who:htm
				 (:div :class "col"
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
							    (frmt "~A" class-name))
						   (js-pair "action" "edit"))
					"Edit")))))))
		 (if (and (parameter "item-id")
			  (string-equal (parameter "item-id") (frmt "~A" (xdb2:id item))))
		     (cl-who:htm
		      (:div :class "row"
			    (dolist (slot slots)
			      (let* ((slot-attributes (cdr slot))
				    (name (car slot))
				    (label (getf slot-attributes :label))
				    )
				(cl-who:htm
				 (:div :class "col-sm-1"
				       (cl-who:str (if label
						       label
						       name))
				       )
				 (:div :class "col-sm-5"
				       (cond
					 ((equalp (getf slot-attributes :db-type) 'string)
					  (cl-who:htm
					   (:input :name name :type "text"
						   :value
						   (cl-who:str 
						    (item-val (getf slot-attributes 
								    :db-type) 
							      slot 
							      item)))))
					 ((equalp (getf slot-attributes :db-type) 'script)
					  (cl-who:htm
					   (:textarea :name name :cols 50 :rows 5
						   
						   (cl-who:str 
						    (slot-value item name)))))
					 (t
					  (cl-who:str (slot-value item name))))
				       )))
			      )
			    ))
		     )
		 )))
			     
			     

	    (:div :class "row"	  
		  (:div :class "col"
					 
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
				    (js-render "cl-wfx:grid"
					       "grid-table"
					       (js-pair "data-spec"
							"Data Spec")
					       (js-pair "pages"
							(or (parameter "pages") 10))
					       (js-pair "page"
							(cl-who:str
							 (if (> active-page 1)
							     (- active-page 1))))
					       (js-pair "grid-name" 
							(frmt "~A" class-name))
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
					 (js-render "cl-wfx:grid"
						    "grid-table"
						    (js-pair "data-spec"
							     "Data Spec")
						    (js-pair "pages"
							     (or (parameter "pages") 10))
						    (js-pair "page"
							     (cl-who:str (+ i 1)))
						    (js-pair "grid-name" 
							     (frmt "~A" class-name))
						    (js-pair "action" "page"))
					 (cl-who:str (+ i 1))))))
				
				(cl-who:htm
				 (:li :class "page-item"
				      (:button ;;:tabindex -1
				       :name "page" :type "submit" 
				       :class (if (< active-page real-page-count)
									  
						  "btn page-link"
						  "btn page-link disabled"
						  )
					 
				       :onclick 
				       (js-render "cl-wfx:grid"
						  "grid-table"
						  (js-pair "data-spec"
							   "Data Spec")
						  (js-pair "pages"
							   (or (parameter "pages") 10))
						  (js-pair "page"
							   (cl-who:str
							    (if (< active-page real-page-count)
								(+ active-page 1))))
						  (js-pair "grid-name" 
							   (frmt "~A" class-name))
						  (js-pair "action" "page-next"))
				       "Next"))))))))))))


(defmethod eval-sexp ((dialect (eql 'context-spec)) (tag (eql 'defclass)) 
		      &key attributes body)
  (declare (ignore dialect) (ignore attributes))

  (render 'grid :class-def body)

  )

(defmethod eval-sexp ((dialect (eql 'context-spec)) (tag (eql :data-spec)) 
		      &key attributes body)
  (declare (ignore tag) (ignore body))
  ;;TODO: Implement grid
  
  (let ((spec (get-data-spec (getf attributes :name))))
    
    (with-html-string
      (:div :class "container"
	    (:div :class "row"	  
		  (:div :class "col"
			(:h2 (cl-who:str (getf attributes :name)))))
	    
	    (if spec 
		(cl-who:str (read-sexp dialect (first (script spec))))
		(getf attributes :name))
	    
	    
	    
	    ))))
|#

#|
;;TODO: called by system (in init.lsip) to init permissions and ????
(defmethod setup-context ((context context) session)
    ;;TODO: Go look in old insite code what needs to go here? Page setup stuff?
 ;; (setf  (permissions context) nil)
  
 ;;  (setf (request-page-permissions context)        (setup-page-permissions 
  )


|#


(defclass context-data-spec-processor (monkey-lisp:processor)
  ())


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

	       (break "poes ~A" ,(format nil "~A" body))
	       ,@body
	       )
	 (:input :type "hidden" :id "huh" :value "shit fuch")
	 (:div :id "shit-id"
	       "fuck " "fuck"
	       )
	 (:button :id "testbutton" 
		  :class "btn-submit btn"
		  :type "submit"
		  :onclick
		  (js-render "cl-wfx:shit"
			     "shit-id"
			     (js-value "huh")
			     (js-pair "grid-name" "fuck-knows")
			     (js-pair "action" "copy-scripts")
			    
			     )
		  "Render Shit")
	 

	 (:script ,(frmt	      
		   "
function ajax_call(func, callback, args, widget_args) {
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

}

" (site-url *system*)))
	 
	 
	 ))
  
  )

(defmethod monkey-lisp:post-process-monkey-sexp ((processor context-data-spec-processor) 
						sexp
						(tag (eql :data-spec)) 
						 attributes body)
   
  (let ((name (getf attributes :name))
	(data-spec (get-data-spec (getf attributes :data-spec)))
	
	)
  (break "pous")
    
    (monkey-html-lisp:with-html
	      (:p "Holy fuck")
	       
	       (:div
		"yeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeha granma"
		)
	     )
   
    
   )
  )

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
      

      
      (let ((body (monkey-lisp:monkey-lisp 
		       (:processor-class cl-wfx::context-data-spec-processor)
		     ,@(script spec))))

	
	
	(monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page			
	   body)
	  )))))
