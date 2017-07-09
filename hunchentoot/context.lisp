(in-package :cl-wfx)

(monkey-lisp::define-monkey-macro shit-shat (&key id from-ajax)
  `(:div "No idea wtf?????????????????"
	 (:div ,id ,from-ajax)))


(defun render-login ()
  (monkey-html-lisp:htm
    (:div :class "row"
	  (:div :class "card col-5"
		(:img :class "card-image-top" :src "../sys/web/images/logo.png")
		
		(:div :class "card-block"
		      (:h4 :class "card-title"
			   "Login")
		      (:form :method "post"
			     :action ""
			     
			     (:input :type "hidden" :id "contextid" 
				     :value (context-id *context*))
			     (:div :class "form-group"
				   (:label :for "email" "Email")
				   (:input :type "email" 
					   :class "form-control"
					   :name "email"
					   :id "email"
					   :value ""))
			     (:div :class "form-group"
				   (:label :for "password" "Password")
				   (:input :type "password" 
					   :class "form-control"
					   :name "password"
					   :id "password"
					   :placeholder "Password"
					   :value ""))
			     (:button :name "action"
				      :class "btn btn-primary"
				      :type "submit"
				      :value "login"
				      "Login")))
		(:div :class "card-footer"
		      (gethash :login-error (cache *context*)))))))

(defun context-url (spec module)
  (let ((spec (or spec (get-context-spec (default-context *system*)))))
    (frmt "~A~A/~A" (site-url *system*) 
	  (string-downcase 
	   (id-string (if module
			  (module-short module)
			  "sys"
			  )))
	  (if (and spec (url spec))
	      (url spec)
	      (string-downcase 
	       (id-string (if spec
			      (name spec)
			      (default-context *system*))))))))

(defmethod on-success (user)
  
  (when (current-user)
;;    (break "Shit user")
    ;;(remhash  (sfx-session-id *sfx-session*) (sessions *sfx-system*))
    ;;(hunchentoot:remove-session *session*)
    )
  
  (let ((active-user (fetch-item "active-users"
				:test (lambda (doc)
					(equal (parameter "user") (user doc))))))
    (unless active-user
      (setf active-user (persist-data (make-instance 'active-user
						     :user user)
				      :license-code *sys-license-code*
				      :collection-name "active-users"
				      )))

    (setf (user *session*) active-user))
  
  ;;(init-user-session user)

;;  (log-login "Login" (email login) "Passed" "Login passed.")

  (hunchentoot:redirect (context-url nil nil)))

(defmethod on-failure ()
 ;; (log-login "Login" (get-val login 'email) "Failed" "User name or password incorrect.")
  (setf (gethash :login-error (cache *context*)) "User name or password incorrect.")
  )

(defun validate-user (email password)
  (let ((user (get-user email)))
    (unless (and user (check-password user password))      
      (setf user nil)
      ;;(setf (message widget) "Email or password incorrect")
      )
    user))

(defmethod action-handler ((action (eql :login)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
  (when (and (parameter "email") (parameter "password"))
    (let ((user (validate-user (parameter "email") (parameter "password"))))
      
        (if user
          (on-success user)
          (on-failure )))))


(defmethod action-handler ((action (eql :logout)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)
  
   
   (setf (contexts *session*) nil)
   (setf *session* nil)
   (break "???")
  (hunchentoot:remove-session hunchentoot:*session*)
  
  (hunchentoot:redirect (frmt "~Asys/login" (site-url *system*))))


(defun user-mods ()
  (fetch-items "modules"
	       :test
	       (lambda (doc)
		 (if (not (string-equal "System Admin" (module-name doc)))
		     doc))
	       :result-type 'list))

(defun mod-menu (mod)
  (if mod
      (menu-items (first (menu mod)))))

(defun system-menu ()
  (let ((sys-mod (fetch-item "modules"
			      :test (lambda (doc)
				      (string-equal "System Admin" (module-name doc))))))    
    (if sys-mod
	(menu-items (first (menu sys-mod))))))


(defun accessible-entity (entity accessible-entities)
  (if (find entity accessible-entities)
      (return-from accessible-entity entity)
      (dolist (entity (children entity))
	(if (find entity accessible-entities)
	    (return-from accessible-entity entity)
	    (accessible-entity entity accessible-entities))))
  )

(defun accessible-entity-roots (accessible-entities)
  (let ((roots))
    (dolist (entity (fetch-all "entities" :result-type 'list))
      (when (root-p entity)
	(when (accessible-entity entity accessible-entities)
	  (pushnew entity roots)
	  )))
    roots))

(defun render-entity-check (entity level accessible-entities)
  (monkey-html-lisp:htm
    (:div :class "row"
	  (:div :class "form-check"
		    
		(:div :class "form-check-label"
		      (dotimes (i level)
			(monkey-html-lisp:htm "&nbsp;"))
		      (if (find entity accessible-entities)
			  (if (find entity (current-entities (active-user)))
			      (monkey-html-lisp:htm
				(:input :class "form-check-input" :type "checkbox" 
					:name "tree-entity-id" 
					:value (xdb2::id entity)
					:checked ""))
			      (monkey-html-lisp:htm
				(:input :class "form-check-input" :type "checkbox" 
					:name "tree-entity-id" :value (xdb2::id entity))))
			  (monkey-html-lisp:htm
			    (:input :class "form-check-input" :type "checkbox" 
				    :disabled "")))
		      (name entity))))
    (if (children entity)
	(dolist (entity (children entity))
	  (render-entity-check entity 
			       (+ level 1) 
			       accessible-entities)))))

(defun render-entity-tree (accessible-entities) 
  
  (monkey-html-lisp:htm
    (:form
     (dolist (entity (accessible-entity-roots accessible-entities))
       ;; (break "~A ~A" entity (xdb2::id entity))
       (when (root-p entity)      
	 (render-entity-check entity 0 accessible-entities)))
     (:button
      :name "set-entities" 
      :type "submit" 
      :formmethod "post"
      :class "btn btn-outline-success"
      :aria-pressed "false"
      :value "set-entities"
      "Set Entities"))))

(defmethod action-handler ((action (eql :set-entities)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (current-entities (active-user)) nil)
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "tree-entity-id")
      
      (dolist (entity (accessible-entities* (current-user)))
	(if (string-equal (frmt "~A" (xdb2::id entity)) (cdr parameter))
	    (pushnew entity (current-entities (active-user))))))))

(defun render-licence-codes ()
  (dolist (code (license-codes (current-user)))
			    
    (monkey-html-lisp:htm
      
      (:div :class "row"
	    (:div :class "form-check"
		  
		  (:div :class "form-check-label"
			(if (find code (license-codes (active-user)) :test #'string-equal)
			    (monkey-html-lisp:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" 
				      :value code
				      :checked ""))
			    (monkey-html-lisp:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" :value code)))
			
			code)))))		      
  )

(defmethod action-handler ((action (eql :set-licenses)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (current-entities (active-user)) nil)
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "license-id")
      
      (pushnew (cdr parameter) (license-codes (active-user)))
   
      )))

(monkey-lisp::define-monkey-macro render-page (menu-p &body body)
  
  `(:html
	"<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css\" integrity=\"sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ\" crossorigin=\"anonymous\">"
	
	"<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js\"></script>"

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
	
	 (if ,(not menu-p)	 
	     (monkey-html-lisp:htm
	       (:div :class "container"
		     ,@body))
	     (monkey-html-lisp:htm
	       (:nav 
		:class "navbar sticky-top navbar-toggleable-md hidden-print"
		
		(if (current-user)
		    (monkey-html-lisp:htm 
		      (:button :class "navbar-toggler navbar-toggler-left"
			       :type "button btn-small"
			       :data-toggle "collapse"
			       :data-target "#menushit"
			       :aria-controls "menushit"
			       :aria-expanded "true"
			       :aria-label "Toggle menu"
			       (:span :class "navbar-toggler-icon"))))
		
		(:a :class "navbar-brand" :href "#" 
		    (:img :src "../sys/web/images/logo-small.png")
		    (system-name *system*))
		(:div :class "collapse navbar-collapse" :id "menushit"
		 (:span :class "navbar-text mr-auto"
			(frmt "Entities: ~A" (current-entities (active-user))))
		 
		 (:div :class "nav-item dropdown"
		       
		       (:a :class "nav-link dropdown-toggle" 
			   :href ""
			   :id "userDropdown" 
			   :data-toggle "dropdown" 
			   :aria-haspopup="true"
			   :aria-expanded "false" 
			   (if (current-user) 
			       (monkey-html-lisp:htm (email (current-user)))))
		       (:div :class "dropdown-menu":aria-labelledby "userDropdown"
			     
			     (let ((sys-mod 
					  (fetch-item "modules"
						      :test (lambda (doc)
							      (string-equal
							       "System Admin" 
							       (module-name doc))))))
				     
				     
				     (dolist (item (menu-items (first (menu sys-mod))))
				       
				       (let ((parameters))
					 
					 (dolist (param (context-parameters item))
					   
					   (setf parameters 
						 (if parameters
						     (frmt "~A&~A=~A" 
							   parameters
							   (parameter-name param)
							   (parameter-value param))
						     (frmt "~A=~A" 
							   (parameter-name param)
							   (parameter-value param)))))
					 
					 (monkey-html-lisp:htm
					   (:a :class "dropdown-item"
					       :href 
					       (if parameters
						   (frmt "~A?~A" 
							 (context-url 
							  (context-spec item)
							  sys-mod)
							 parameters)
						   (context-url 
						    (context-spec item)
						    sys-mod))
					       (item-name item)
					       
					       )))))))
		 
		 ))
	       (:nav :class "navbar"
		(if (current-user)
		    (monkey-html-lisp:htm 
		      (:button :class "navbar-toggler navbar-toggler-left"
			       :type "button btn-small"
			       :data-toggle "collapse"
			       :data-target "#exNavbarLeft"
			       :aria-controls "exNavbarLeft"
			       :aria-expanded "true"
			       :aria-label "Toggle application menu"
			       "&#9776;")))
		(if (current-user)
		    (monkey-html-lisp:htm
		      (:button :class "navbar-toggler navbar-toggler-right"
			       :type "button"
			       :data-toggle "collapse"
			       :data-target "#exNavbarRight"
			       :aria-controls "exNavbarRight"
			       :aria-expanded "false"
			       :aria-label "Toggle system menu"
			       "&#9776;")))
		
		)
	      
	       (:br "")
	       (:div :class "container-fluid"
		     
		     (:div :class "row"
			   (:div :class "collapse col-md-2 col-md-auto show hidden-print"
				 :id "exNavbarLeft"
				 (:nav :class "nav nav-pills flex-column"
				       (dolist (mod (user-mods))
					 (dolist (menu (menu mod))

					   (dolist (item (menu-items menu))
					     (monkey-html-lisp:htm
					       (:a :class 
						   "nav-link ~A"
						   :href (url 
							  (context-spec item))
						   (item-name item))))))))
		    
			   (:div  :class "col" :id "grid-table"
				  ,@body)
		    
			   (:div :class "collapse col-md-2 hidden-print " 
				 :id "exNavbarRight" :style "background-color:#FFFFFF"
				 (:form
				  (:div :class "row bg-faded"
					"Accessible License Codes")
				  (render-licence-codes)				 
				 
				  (:button
				   :name "set-licenses" 
				   :type "submit" 
				   :formmethod "post"
				   :class "btn btn-outline-success"
				   :aria-pressed "false"
				   :value "set-licenses"
				   "Set Licenses"))
			
				 (:div :class "row bg-faded"
				       "Accessible Entities")
				 (:div :class "row"
				       (render-entity-tree 
					(accessible-entities* (current-user))))
				 
				 )))))
	 
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

(defun check-user-access ()
  (unless (current-user)
	     
    (hunchentoot:redirect (frmt "~Asys/login" (site-url *system*)))
	     )
  )
(defmethod setup-context ((module module) (spec context-spec) system)  
  (eval
   `(hunchentoot:define-easy-handler 
	(,(alexandria:symbolicate 
	   (string-upcase (id-string (name spec))) 
	   '-page)  
	  :uri ,(if (url spec)
		    (url spec)
		    (frmt "~A~A/~A" (site-url system) 
			  (string-downcase 
			   (id-string (if module
					  (module-short module)
					  "sys"
					  )))
			  (string-downcase 
			   (id-string (name spec)))))  
	  :allow-other-keys t) ,(args spec)
      
      (check-user-access)
      (monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page t (render-grid ',(getf (cdr (context-spec-script spec)) :name)))))))

(defmethod setup-context-login ((module module) (spec context-spec) system)  
  (eval
   `(hunchentoot:define-easy-handler (,(alexandria:symbolicate 
					(string-upcase (id-string (name spec))) 
					'-page)  
				       :uri ,(frmt "~A~A/~A" (site-url system) 
						   (string-downcase 
						    (id-string (if module
								   (module-short module)
								   "sys")))
						   (if (url spec)
						       (url spec)
						       (string-downcase 
							(id-string (name spec)))))  
				       :allow-other-keys t) ,(args spec)
      
      (monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page nil
	   (render-login))))))

