(in-package :cl-wfx)


(defun render-login ()
  (with-html-string
    (:div :class "row"
	  (:div :class "card col-5"
		(:img :class "card-image-top" :src "../cor/web/images/logo.png")
		
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

  (let ((spec (or spec (get-context-spec (get-store-from-short-mod 
					  (digx module :module-short))
					 (default-context *system*)))))
    (frmt "~A~A/~A" (site-url *system*) 
	  (string-downcase 
	   (id-string (if module
			  (digx module :module-short)
			  "cor")))
	  (if (and spec (digx spec :url))
	      (digx spec :url)
	      (string-downcase 
	       (id-string (if spec
			      (digx spec :name)
			      (default-context *system*))))))))

(defmethod on-success (user)
  
  (when (current-user)
;;    (break "Shit user")
    ;;(remhash  (sfx-session-id *sfx-session*) (sessions *sfx-system*))
    ;;(hunchentoot:remove-session *session*)
    )
  
 ;; (break "~A" (core-collection "active-users"))
  
  (let ((active-user (fetch-item (core-collection "active-users")
				:test (lambda (item)
					(equal (parameter "user") (digx item :email))))))
    (unless active-user
      (setf active-user (persist-item (core-collection "active-users")
				      '(:email (digx item :email) 
					:selected-licenses nil
					:selected-entities nil))))

    (setf (user *session*) user)
    (setf (active-session-user *session*) active-user))
  
  ;;(init-user-session user)

;;  (log-login "Login" (email login) "Passed" "Login passed.")

  (hunchentoot:redirect (context-url nil *module*)))

(defmethod on-failure ()
 ;; (log-login "Login" (get-val login 'email) "Failed" "User name or password incorrect.")
  (setf (gethash :login-error (cache *context*)) "User name or password incorrect.")
  )

(defun validate-user (email password)
  (let ((user (get-user email)))
    (unless (and user (check-password user password))      
      (setf user nil))
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

  (hunchentoot:remove-session hunchentoot:*session*)
  
  (hunchentoot:redirect (frmt "~Acor/login" (site-url *system*))))


(defun user-mods ()
  (wfx-fetch-items "modules"
		   :test
		   (lambda (item)
		     (and (not (string-equal "Core" (digx item :name)))
			  item))
		   :result-type 'list))

(defun mod-menu (mod)
  (if mod
      (digx (first (digx mod :menu)) :menu-items)))

(defun system-menu ()
  (let ((sys-mod (fetch-item (core-collection "modules")
			      :test (lambda (item)
				      (string-equal "Core" (digx item :name))))))    
    (if sys-mod
	(digx (first (digx sys-mod :menu)) :menu-items))))


(defun accessible-entity (entity accessible-entities)
  (if (find entity accessible-entities)
      (return-from accessible-entity entity)
      (dolist (entity (digx entity :children))
	(if (find entity accessible-entities)
	    (return-from accessible-entity entity)
	    (accessible-entity entity accessible-entities))))
  )

(defun accessible-entity-roots (accessible-entities license-code)
  (let ((roots))
    (dolist (entity (fetch-items (license-collection license-code "entities")
				 :result-type 'list))
      (when (digx entity :root-p)
	(when (accessible-entity entity accessible-entities)
	  (pushnew entity roots)
	  )))
    roots))

(defun render-entity-check (entity level accessible-entities)
  (with-html-string
    (:div :class "row"
	  (:div :class "form-check"
		    
		(:div :class "form-check-label"
		      (dotimes (i level)
			(cl-who:htm "&nbsp;"))
		      (if (find entity accessible-entities)
			  (if (find entity (digx (active-user) :selected-entities))
			      (cl-who:htm
				(:input :class "form-check-input" :type "checkbox" 
					:name "tree-entity-id" 
					:value (item-hash entity)
					:checked ""))
			      (cl-who:htm
				(:input :class "form-check-input" :type "checkbox" 
					:name "tree-entity-id" :value (item-hash entity))))
			  (cl-who:htm
			    (:input :class "form-check-input" :type "checkbox" 
				    :disabled "")))
		      (name entity))))
    (if (digx entity :children)
	(dolist (entity (digx entity :children))
	  (render-entity-check entity 
			       (+ level 1) 
			       accessible-entities)))))

(defun render-entity-tree (license-code accessible-entities ) 
  
  (with-html-string
    (:form
     (dolist (entity (accessible-entity-roots license-code accessible-entities))
       ;; (break "~A ~A" entity (item-hash entity))
       (when (digx entity :root-p)      
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

  (setf (digx (active-user) :selected-entities) nil)
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "tree-entity-id")
      
      (dolist (entity (accessible-entities* (current-user)))
	(if (string-equal (frmt "~A" (item-hash entity)) (cdr parameter))
	    (pushnew entity (digx (active-user) :selected-entities)))))))

(defun render-licence-codes ()
  (dolist (code (digx (current-user) :license-codes))
			    
    (with-html-string
      
      (:div :class "row"
	    (:div :class "form-check"
		  
		  (:div :class "form-check-label"
			(if (find code (digx (active-user) :license-codes) 
				  :test #'string-equal)
			    (cl-who:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" 
				      :value code
				      :checked ""))
			    (cl-who:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" :value code)))
			
			code)))))		      
  )

(defmethod action-handler ((action (eql :set-licenses)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (digx (active-user) :selected-entities) nil)
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "license-id")
      (pushnew (cdr parameter) (digx (active-user) :license-codes)))))

(defun render-page (menu-p body)
  
  (with-html-string
    (:html
     (:head
      "<link rel=\"stylesheet\"
	href=\"../web/font-awesome-4.7.0/css/font-awesome.min.css\">"
      
      "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css\" integrity=\"sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ\" crossorigin=\"anonymous\">"
      
      "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js\"></script>"
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
      

      )
     
     
     (:body 
      (:input :type "hidden" :id "contextid" :value (context-id *context*))

      (if (not menu-p)	 
	  (cl-who:htm
	   (:div :class "container"
		 (cl-who:str body)))
	  (cl-who:htm
	   (:nav 
	    :class "navbar sticky-top navbar-toggleable-md hidden-print"
	    
	    (if (current-user)
		(cl-who:htm 
		 (:button :class "navbar-toggler navbar-toggler-left"
			  :type "button btn-small"
			  :data-toggle "collapse"
			  :data-target "#menushit"
			  :aria-controls "menushit"
			  :aria-expanded "true"
			  :aria-label "Toggle menu"
			  (:span :class "navbar-toggler-icon"))))
	    
	    (:a :class "navbar-brand" :href "#" 
		(:img :src "../cor/web/images/logo-small.png")
		(name *system*))
	    (:div :class "collapse navbar-collapse" :id "menushit"
		  (:span :class "navbar-text mr-auto"
			 (frmt "Entities: ~A" (digx (active-user) :selected-entities)))
		  
		  (:div :class "nav-item dropdown"
			
			(:a :class "nav-link dropdown-toggle" 
			    :href ""
			    :id "userDropdown" 
			    :data-toggle "dropdown" 
			    :aria-haspopup="true"
			    :aria-expanded "false" 
			    (if (current-user) 
				(cl-who:htm (digx (current-user) :email))))
			(:div :class "dropdown-menu":aria-labelledby "userDropdown"
			      
			      (let ((sys-mod 
				     (fetch-item (core-collection "modules")
						 :test (lambda (item)
							 (string-equal
							  "Core" 
							  (digx item :name))))))
				
			;;	(break "sys-mod ~A" sys-mod)
				(dolist (item (digx sys-mod :menu :menu-items))

				  (let ((parameters))
				    
				    (dolist (param (digx item :context-parameters))
				      
				      (setf parameters 
					    (if parameters
						(frmt "~A&~A=~A" 
						      parameters
						      (digx param :name)
						      (digx param :value))
						(frmt "~A=~A" 
						      (digx param :name)
						      (digx param :value)))))
				  
				    (cl-who:htm
				     (:a :class "dropdown-item"
					 :href 
					 (if parameters
					     (frmt "~A?~A" 
						   (context-url 
						    (digx item :context-spec)
						    sys-mod)
						   parameters)
					     (context-url 
					      (digx item :context-spec)
					      sys-mod))
					 (digx item :name))))))))))
	   (:nav :class "navbar"
		 (if (current-user)
		     (cl-who:htm 
		      (:button :class "navbar-toggler navbar-toggler-left"
			       :type "button btn-small"
			       :data-toggle "collapse"
			       :data-target "#exNavbarLeft"
			       :aria-controls "exNavbarLeft"
			       :aria-expanded "true"
			       :aria-label "Toggle application menu"
			       "&#9776;")))
		 (if (current-user)
		     (cl-who:htm
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
				     (break "user-mods ~A" mod)
				     (dolist (menu (digx mod :menu))

				       (dolist (item (digx menu :menu-items))
					 (cl-who:htm
					  (:a :class 
					      "nav-link ~A"
					      :href (url 
						     (digx item :context-spec))
					      (digx item :name))))))))
		       
		       (:div  :class "col" :id "grid-table"
			      body)
		       
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
				   (dolist (license-code (digx (active-user) 
								 :selected-licenses))
				       (cl-who:htm
					(render-entity-tree 
					 license-code
					 (accessible-entities* (current-user)))))))))))


      "<script>$(itemument).on('click', '.dropdown-item', function(){
       var selVal = $(this).children().first();
       var selText = $(this).text();
       $(this).parents('.dropdown').find('.dropdown-toggle').html(selText);
       $(this).parents('.dropdown').find('.selected-value').val($(selVal).val());
});</script>"
      
      (:script (frmt	      
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

}" (site-url *system*)))))))

(defun check-user-access ()
  (unless (current-user)
	     
    (hunchentoot:redirect (frmt "~Acor/login" (site-url *system*)))
	     )
  )
(defmethod setup-context ((module item) (context-spec item) (system hunch-system)  
			  &key &allow-other-keys) 
  
  (eval
   `(hunchentoot:define-easy-handler 
	(,(alexandria:symbolicate 
	   (string-upcase (id-string (digx context-spec :name))) 
	   '-page)  
	  :uri ,(if (digx context-spec :url)
		    (digx context-spec :url)
		    (frmt "~A~A/~A" (site-url system) 
			  (string-downcase 
			   (id-string (digx module :module-short)))
			  (string-downcase 
			   (id-string (digx context-spec :name)))))  
	  :allow-other-keys t) ,(digx context-spec :args)
      
      (check-user-access)
      (with-html
	"<!itemtype html>"
	(render-page t (render-grid 
			,(digx context-spec :name)
			,(getf
			  (find-collection-def *system* 
					       (digx context-spec :collection))
			  :data-type)))))))

(defmethod setup-context-login ((module item) (context-spec item) (system hunch-system)
				&key &allow-other-keys)  
  (eval
   `(hunchentoot:define-easy-handler (,(alexandria:symbolicate 
					(string-upcase 
					 (id-string (digx context-spec :args))) 
					'-page)  
				       :uri ,(frmt "~A~A/~A" (site-url system) 
						   (string-downcase 
						    (id-string 
						     (digx module :module-short)))
						   (if (digx context-spec :url)
						       (digx context-spec :url)
						       (string-downcase 
							(id-string 
							 (digx context-spec :name)))))  
				       :allow-other-keys t) ,(digx context-spec :args)
      (with-html
	"<!itemtype html>"
	(render-page nil (render-login))))))

