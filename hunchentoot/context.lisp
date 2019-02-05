(in-package :cl-wfx)

(defun render-login ()
  (with-html-string
    (:div :class "row"
	  (:div :class "card col-6"
		(:img :class "card-image-top"
		      :src  (frmt "~Acor/web/images/~A"
				  (site-url *system*)
				  (if (theme-element
				       (theme *system*) :login-image)
				      (theme-element
				       (theme *system*) :login-image)
				      "logo-login.png")))	 
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
		      (cl-who:str
		       (gethash :login-error (cache *context*))))))))

(defun context-url (spec-name)
  (frmt "~As-wfx?cs=~A" (site-url *system*) 
	  (string-downcase 
	   spec-name)))

(defun clear-hash-items (hash)
  (when hash
    (loop for key being the hash-keys of hash	 
       do (remhash key hash))))

(defmethod on-success (user)

  (when (current-user)
    (when *session*
      (clear-hash-items (cache *session*))
      (clear-hash-items (contexts *session*))
      (setf (user *session*) nil)))

  (dolist (license-code (available-licenses user))
    (init-license-universe *system* license-code))
  
  (let ((active-user (fetch-item (core-collection "active-users")
				 :test (lambda (item)				 
					 (equalp (parameter "email")
						 (digx item :email))))))
    (unless active-user      
      (setf active-user (persist-item (core-collection "active-users")
				      (list :email (digx user :email) 
					    :selected-licenses nil
					    :selected-entities nil))))
    (setf (user *session*) user)
    (setf (active-session-user *session*) active-user))
  
  ;;TODO: Handle default page other than cor pages....
  (hunchentoot:redirect (context-url (default-context *system*))))

(defmethod on-failure ()
  (setf (gethash :login-error (cache *context*))
	"User name or password incorrect."))

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
  (persist-item (core-collection "active-users") (active-user))
  (hunchentoot:remove-session hunchentoot:*session*)
  
  (hunchentoot:redirect (frmt "~As-wfx?cs=login" (site-url *system*))))

(defun user-mods ()
  (wfx-fetch-items "modules"
		   :test
		   (lambda (item)			    
		     (not (string-equal "Core" (digx item :name))))
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
	    (accessible-entity entity accessible-entities)))))

(defun accessible-entity-roots (accessible-entities license-code)
  (let ((roots))
    (dolist (entity (fetch-items (license-collection license-code "entities")
				 :result-type 'list))      
      (when (digx entity :root-p)	
	(when (accessible-entity entity accessible-entities)
	  (pushnew entity roots))))
    roots))

(defun render-entity-check (entity level accessible-entities)
  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(:div :class "form-check"
		      (:div :class "form-check-label"
			    (dotimes (i level)
			      (cl-who:htm "&nbsp;"))
			    
			    (if (find entity accessible-entities)
				(if (find (item-hash entity)
					  (digx (active-user)
						:selected-entities)
					  :test #'equalp)
				    (cl-who:htm
				     (:input :class "form-check-input"
					     :type "checkbox" 
					     :name "tree-entity-id" 
					     :value (item-hash entity)
					     :checked ""))
				    (cl-who:htm
				     (:input :class "form-check-input"
					     :type "checkbox" 
					     :name "tree-entity-id"
					     :value (item-hash entity))))
				(cl-who:htm
				 (:input :class "form-check-input"
					 :type "checkbox" 
					 :disabled "")))
			    (cl-who:str (getx entity :name))))))
    
    (if (digx entity :children)
	(dolist (entity (digx entity :children))
	  (cl-who:str (render-entity-check entity 
					   (+ level 1) 
					   accessible-entities))))))

(defun render-entity-tree (license-code accessible-entities )
  (with-html-string
    (:form
     (dolist (entity (accessible-entity-roots
		      accessible-entities license-code))

       (when (digx entity :root-p)
	   (cl-who:str (render-entity-check entity 0 accessible-entities))))

     (:div :class "row"
	   (:div :class "col"
		 (:button
		  :name "set-entities" 
		  :type "submit" 
		  :formmethod "post"
		  :class "btn btn-outline-success"
		  :aria-pressed "false"
		  :value "set-entities"
		  "Set Entities"))))))

(defmethod action-handler ((action (eql :set-entities)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (getf (item-values (active-user)) :selected-entities) nil)
 
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "tree-entity-id")
      (dolist (license-code (digx (active-user) :selected-licenses))

	
	(dolist (entity (available-entities license-code))
	  
	  (when (string-equal (frmt "~A" (item-hash entity)) (cdr parameter))
	    (pushnew (item-hash entity)
		     (getx (active-user) :selected-entities))
	    (persist-item (core-collection "active-users") (active-user))))))))

(defun render-licence-codes ()
  (with-html-string
    (dolist (code (available-licenses))    
      (cl-who:htm       
       (:div :class "row"
	     (:div :class "form-check"
		   (:div :class "form-check-label"
			 (if (find code (digx (active-user) :selected-licenses) 
				   :test #'string-equal)
			     (cl-who:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" 
				      :value code
				      :checked ""))
			     (cl-who:htm
			      (:input :class "form-check-input" :type "checkbox" 
				      :name "license-id" :value
				      (cl-who:str code))))
			 (cl-who:str code))))))))

(defun context-access-p (context)
  (let ((access-p))

   
    (when (and (active-user)
	       (digx (active-user) :selected-licenses))
      (cond ((getx (current-user) :super-user-p)
	     (setf access-p t))
	    (t (dolist (lic (digx (active-user)
				  :selected-licenses))
		 (when (license-user lic)
		   (dolist (permission
			     (getx (license-user lic)
				   :permissions))
		     
		     (when (equalp context
				   (digx permission
					 :context-spec))
		    
		       (setf access-p t))))))))
    access-p))

(defun render-user-admin-menu ()
  (with-html-string
    (:div :class "nav-item dropdown"	  
	  (:a :class
	      (concatenate
	       'string
	       "nav-link dropdown-toggle font-weight-bold "
	       (theme-element-attribute
		(theme *system*)
		:nav-toggler-right
		:text-color-class))
	      :href ""
	      :id "userDropdown" 
	      :data-toggle "dropdown" 
	      :aria-haspopup="true"
	      :aria-expanded "false" 
	      (if (current-user) 
		  (cl-who:str (cl-who:htm
			       (digx (current-user) :email)))))
	  
	  (:div :class "dropdown-menu"
		:aria-labelledby "userDropdown"
		(let ((sys-mod 
		       (fetch-item
			(core-collection "modules")
			:test (lambda (item)
				(string-equal
				 "Core" 
				 (digx item :name))))))

		  (dolist (menu (digx sys-mod :menu))
		    (when (equalp (digx menu :name) "System")
		      (dolist (item (digx menu :menu-items))
			(let ((parameters))
			  (dolist (param (digx item :context-parameters))
			    (setf parameters 
				  (frmt "~A&~A=~A" 
					    parameters
					    (digx param :name)
					    (digx param :value))))
			  
			  (when (or (equalp (digx item :name) "Logout")
				    (context-access-p
				     (digx item :context-spec)))
			    (cl-who:htm
			  
			     (:a :class "dropdown-item"
				 :href 
				 (if parameters
				     (frmt "~A&~A" 
					   (context-url 
					     (digx item :context-spec :name))
					   parameters)
				     (context-url 
				      (digx item :context-spec :name)))
				 (cl-who:str (digx item :name))))))))))))))

(defun render-header-display (text)
  (with-html-string
    (:span :class
	   (concatenate
	    'string
	    "navbar-text mr-auto font-weight-bold "
	    (tea
	     (theme *system*)
	     :nav-toggler-left
	     :text-color-class)			     )
	   	   
	   (cl-who:str
	    text))))

(defun data-menu (menu-items)
  (let ((items))
    (dolist (item menu-items)
   
      (when (and
	     (digx item :context-spec)
	     (context-access-p
	      (digx item :context-spec))
	     (item-collection (digx item :context-spec)
			      )
	     (not (getx (digx item :context-spec) :report)))
	(pushnew item items)))
    (reverse items)))

(defun report-menu (menu-items)
  (let ((items))
    (dolist (item menu-items)
      (when (and
	     (digx item :context-spec)
	     (context-access-p
	      (digx item :context-spec))
	     (getx (digx item :context-spec) :report))
	(pushnew item items)))
    (reverse items)))

(defun other-menu (menu-items)
  (let ((items))
    (dolist (item menu-items)
      (when (and
	     (digx item :context-spec)
	     (context-access-p
	      (digx item :context-spec))
	     (and (not (getx (digx item :context-spec) :report))
		  (not (getx (digx item :context-spec) :collection))))
	(pushnew item items)))
    (reverse items)))

(defun render-menu-item (item)
  (with-html-string

    
    (:a :class "btn btn-outline-light text-dark w-100 text-left"
	:role "button"
	    
	     :href (context-url
		    (digx item :context-spec :name))
	     (cl-who:str (digx item :name)))))

(defun render-left-user-menu ()
  (with-html-string
    (:ul :class "nav navpills flex-column"
	 :style "box-shadow: 0px 5px 10px;"
	  (dolist (mod (user-mods))
	   
	    (dolist (menu (digx mod :menu))
	      (cl-who:htm
	       (:a :class "nav-link bg-secondary text-light border border-light rounded"
		   :data-toggle "collapse"
		   :href "#data-menu"
		   (:strong "Data")))
	  
	      (cl-who:htm
	       (:div :id "data-menu" :class "expand"
		     (:ul :class "nav flex-column ml"
			  
			  (dolist (item (data-menu (digx menu :menu-items)))
			  
			    (cl-who:htm
			     (:div :class "btn-group "
				  
				   (:button
				    
				    :class "btn btn-light"
				    :name "filter-grid" 
				    
				    :data-toggle "collapse"
				    :href "#collapseFilter" 
				    :aria-expanded "false"
				    :aria-controls="collapseFilter"
				    :aria-pressed "false"
				    (:i
				    
				     :class (frmt "fa far ~A"
						   (digx item :context-spec
							 :icon))))
				   
				   (cl-who:str (render-menu-item item))))))))
	      (cl-who:htm
	       (:a :class "nav-link bg-secondary text-light border border-light rounded"
		   :data-toggle "collapse"
		   :href "#report-menu"
		   (:strong "Reports"))
	       (:div :id "report-menu" :class "collapse"
		     (:ul :class "nav flex-column ml"			 
			  (dolist (item (report-menu (digx menu :menu-items)))
			    (cl-who:htm
			     (:div :class "btn-group "
				  
				   (:button
				    
				    :class "btn btn-light"
				    :name "filter-grid" 
				    
				    :data-toggle "collapse"
				    :href "#collapseFilter" 
				    :aria-expanded "false"
				    :aria-controls="collapseFilter"
				    :aria-pressed "false"
				    (:i
				    
				     :class (frmt "fa far ~A"
						   (digx item :context-spec
							 :icon))))
				   
				   (cl-who:str (render-menu-item item))))
			    
			    ))))
	      (cl-who:htm
	       (:a :class "nav-link bg-secondary text-light border border-light rounded"
		   :data-toggle "collapse"
		   :href "#other-menu"
		   (:strong "Other"))
	       (:div :id "other-menu" :class "collapse"
		     (:ul :class "nav flex-column ml"			 
			  (dolist (item (other-menu (digx menu :menu-items)))
		 (cl-who:str (render-menu-item item)))))))))))

(defun render-license-dropdown (list &key (select-prompt "Select a License"))
  (let ((selected-value (or (parameter "license-select")
			    (car (getx (active-user) :selected-licenses))
			    select-prompt)))

    (with-html-string
      (:div :class "bt-group dropdown w-100"
	    (:input :type "hidden" :class "selected-value" 
		    :name "license-selected"
		    :value (or selected-value
			       ""))
	    (:button :class "btn dropdown-toggle w-100"
		     :type "button"
		     :data-toggle "dropdown"
		     :aria-haspopup "true" :aria-expanded "false"
		     (cl-who:str (or selected-value "")))
	    (:div :class "dropdown-menu w-100"
		  (dolist (option list)
		    (cl-who:htm
		     (:span :class "dropdown-item"
			    :onclick (js-render-form-values
				      "cl-wfx:ajax-license-select"
				      "entities-selection"
				      "license-selection"
				      (js-pair "action"
					       "license-select-action")
				      (js-pair "select-action-value"
					       option))
			     
			    (:input :id "select-action"
				    :type "hidden"
				    :value option)
			    (cl-who:str option)))))))))

(defun ajax-license-select (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))

  (unless (equalp (parameter "license-selected") "Select a License")
    ;;Set license for session
    (setf (getf (item-values (active-user)) :selected-licenses) nil)
    (setf (getx (active-user) :selected-entities) nil)
    
    (setf (getx (active-user) :selected-licenses)
	  (list (parameter "select-action-value")))

    (init-definitions (universe *system*)
		      :license (parameter "select-action-value")
		      (data-definitions *system*))
    
    (persist-item (core-collection "active-users") (active-user)))
  
  (with-html-string
    (:div :class "col"
	  (:div :class "row bg-light"
		(:div :class "col bg-light font-weight-bold"
		      "Accessible Entities"))

	  
	  
	  (dolist (license-code (digx (active-user) 
				      :selected-licenses))
	    (when (or (license-user license-code)
		      (getx (current-user) :super-user-p))	      
	      (cl-who:str
	       (render-entity-tree 
		license-code
		(available-entities license-code))))))))

(defmethod action-handler ((action (eql :set-licenses)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (setf (getf (item-values (active-user)) :selected-licenses) nil)
  (setf (getx (active-user) :selected-entities) nil)

  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "license-id")
      (pushnew (cdr parameter)
	       (getx (active-user) :selected-licenses)
	       :test #'string-equal)

      (init-definitions (universe *system*)
			:license (cdr parameter)
			(data-definitions *system*))
      
      (persist-item (core-collection "active-users") (active-user)))))

(defun render-right-menu ()
  (with-html-string
    (:div :class "nav navpills flex-column"
	  :style "box-shadow: 0px 5px 10px;"
	  (:a :class "nav-link bg-secondary text-light border border-light rounded w-100"
	      :data-toggle "collapse"
	      :href "#license-context"
	      (:strong "License Context"))
	  (:div
	   :id "license-context"
	   (:ul :class "nav flex-column"
		(:form
		 :id "license-selection"
		 (:div :class "col nav-item  bg-light font-weight-bold"
		       "Accessible Licenses")
		 (:div :class "row"
		       (:div :class "col"
			     (cl-who:str (render-license-dropdown
					   (available-licenses) ))))
		 (:div :class "row" :id "entities-selection"
		       (:div :class "col"
			     (:div :class "col bg-light font-weight-bold"
				   "Accessible Entities")
			     (let ((license-code
				    (car (getf (item-values (active-user))
					       :selected-licenses))))
			       (when license-code
				 (when (getf (item-values (active-user))
					     :selected-entities)
				   (cl-who:str
				    (render-entity-tree 
				     license-code
				     (available-entities license-code)))))))))))

	  (:a :class "nav-link bg-secondary text-light border border-light rounded w-100"
	      :data-toggle "collapse"
	      :href "#debug-info"
	      (:strong "Debug Info"))	  
	  (:div :id "debug-info" :class "collapse"
		(:ul :class "nav flex-column"
		     (:div
		      (cl-who:str (hunchentoot:post-parameters*))))))))

(defun theme-element (theme element)
  (dig theme element))

(defun theme-element-attribute (theme element attribute)
  (dig theme element attribute))

(defun tea (theme element attribute &optional default)
  (let ((attribute 
	 (theme-element-attribute theme element attribute)))
    (if attribute
	attribute
	default)))


(defun theme-style (theme element)
  (let ((style-string ""))
    (dolist (style (dig theme element :style))
      (setf style-string
       (concatenate 'string style-string
		    (frmt "~A: ~A;" (first style) (second style)))))
    style-string))

(defun ts (theme element &optional default)
  (let ((style
	 (theme-style theme element)))
    (if (not (empty-p style))
	style
	default)))

(defgeneric page-css (system &key &allow-other-keys))

(defmethod page-css ((system hunch-system) &key &allow-other-keys)
  )

(defmethod page-css :around ((system hunch-system) &key &allow-other-keys)
  
  (with-html-string
 
    (:link :rel "stylesheet"
	   :href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"
	   :integrity "sha384-Smlep5jCw/wG7hdkwQ/Z5nLIefveQRIY9nfy6xoR1uRYBtpZgI6339F5dgvm/e9B"
	   :crossorigin "anonymous")

    (:link :rel "stylesheet"
	   :href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/css/fileinput.min.css"
	   :media "all"
	   :type "text/css")

    (:link :rel "stylesheet"
	   :href "https://use.fontawesome.com/releases/v5.6.0/css/all.css"
	   :integrity "sha384-aOkxzJ5uQz7WBObEZcHvV5JvRW3TUc2rNPA7pe3AwnsUohiw1Vj2Rgx2KSOkF5+h"
	   :crossorigin "anonymous")

  
    (:link :rel "stylesheet"
	   :href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/css/bootstrap-datepicker3.standalone.min.css")
    
       #|
      (cl-who:str (frmt  "<script src=\"~Aweb/codemirror/lib/codemirror.js\"></script>" (site-url *system*)))
	
      (cl-who:str
      (frmt  "<link rel=\"stylesheet\" href=\"~Aweb/codemirror/lib/codemirror.css\">" (site-url *system*)))
	
      (cl-who:str
      (frmt  "<script src=\"~Aweb/codemirror/mode/commonlisp/commonlisp.js\"></script>
" (site-url *system*)))
	
      (cl-who:str
      (frmt  "<script src=\"~Aweb/codemirror/addon/edit/closebrackets.js\"></script>
" (site-url *system*)))
	
      (cl-who:str
      (frmt  "<script src=\"~Aweb/codemirror/addon/edit/matchbrackets.js\"></script>
" (site-url *system*)))
      |#
    

    (:link :rel "stylesheet"
	   :href (frmt "~Aweb/cl-wfx.css" (site-url system)))

    (cl-who:str (call-next-method))
   )
  )




(defgeneric page-header-js (system &key &allow-other-keys))

(defmethod page-header-js ((system hunch-system) &key &allow-other-keys)
  )


(defmethod page-header-js :around ((system hunch-system) &key &allow-other-keys)
  (with-html-string
    (:script
      :src "https://code.jquery.com/jquery-3.3.1.min.js"
      :integrity "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8="
      :crossorigin "anonymous")
    
    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
     :ingegrity "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
     :crossorigin "anonymous")
    
    (:script
     :src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.min.js"
     :ingegrity "sha384-o+RDsa0aLu++PJvFqy8fFScvbHFLtbvScb8AjopnFD+iEQ7wo/CG0xlczd+2O/em"
     :crossorigin "anonymous")
    
    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/js/fileinput.min.js")
    
    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/themes/fa/theme.min.js")
    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/js/bootstrap-datepicker.min.js")
    
    (:script
     :src (frmt "~Aweb/cl-wfx.js" (site-url system)))

    (cl-who:str (call-next-method))
    ))



(defgeneric page-footer-js (system &key &allow-other-keys))

(defmethod page-footer-js ((system hunch-system) &key &allow-other-keys)
 )

(defmethod page-footer-js :around ((system hunch-system) &key &allow-other-keys)

  (with-html-string
    (:script :type "text/javascript"
     (cl-who:str
      "prep_dropdowns();"))
    
    (:script :type "text/javascript"
     (cl-who:str
      "prep_auto_completes();"))
    
    (:script :type "text/javascript"
	     (cl-who:str 
	      "//prep_codemiror();"
	      )
	     )
   
    (:script :type "text/javascript"
	     (cl-who:str
	      "prep_file_upload();"))


    (:script :type "text/javascript"
	     (cl-who:str
	      "prep_expands();"))

    
    (:script :type "text/javascript"
	     (cl-who:str
	      (frmt	      
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
            var arg = widget_args[i];
            post_parameters += '&' + encodeURIComponent(arg[0]) + '='
                + encodeURIComponent(arg[1]);
        }
    }

    fetchURI(uri, callback, post_parameters);

}" (site-url system))))

    (cl-who:str (call-next-method))
    ))


(defgeneric render-page (system body &key menu-p header-css header-js
				       footer-js
				       &allow-other-keys))


(defmethod render-page ((system hunch-system) body
			&key menu-p header-css header-js
			  footer-js
			  &allow-other-keys)
  (with-html-string
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport"
	     :content "width=device-width, initial-scale=1, shrink-to-fit=no")

      (cl-who:str (page-css system))
      (cl-who:str header-css)
      (cl-who:str (page-header-js system))
      (cl-who:str header-js))
     
     
     (:body
      
	(:input :type "hidden" :id "contextid" :value (context-id *context*))

	(if (not menu-p)	 
	    (cl-who:htm
	     (:div :class "container"
		   (cl-who:str body)))
	    (cl-who:htm
	     (unless (active-user)
	       (hunchentoot:redirect (frmt "~Acor/login" (site-url system))))
	     
	     (:nav 
	      :class "navbar sticky-top d-print-none justify-content-between bg-white"
	      :style
	      (if (tea (theme system) :main-nav-bar :bg-image)
		(frmt "background-image: url(~Acor/web/images/~A);~A"
		      (site-url system)
		      (tea (theme system) :main-nav-bar :bg-image)
		      (ts (theme system) :main-nav-bg-image))
		(ts (theme system) :main-nav-bar
		    "background-size: cover;box-shadow: 0px 1px 2px"))
	      
	      (if (current-user)
		  (cl-who:htm 
		   (:button :class
			    (concatenate
			     'string
			     "navbar-toggler navbar-toggler-left "
			     (tea
			      (theme system)
			      :nav-toggler-left
			      :text-color-class))
			    
			    :type "button btn-small"
			    :data-toggle "collapse"
			    :data-target "#exNavbarLeft"
			    :aria-controls "exNavbarLeft"
			    :aria-expanded "true"
			    :aria-label "Toggle application menu"
			    "&#9776;")))
	      
	      (:a :class "navbar-brand" :href "#" 
		  (:img
		   :style (ts (theme system) :main-nav-logo
			      "height:50px;")
		   :src (frmt "~Acor/web/images/~A"
			      (site-url system)
			      (tea
			       (theme system)
			       :main-nav-logo
			       :src)))
		  (name system))

	      (cl-who:str (render-header-display ""))
	      
	      (cl-who:str (render-user-admin-menu))
	      
	      (if (current-user)
		  (cl-who:htm
		   (:button :class
			    (concatenate
			     'string
			     "navbar-toggler navbar-toggler-right "
			     (theme-element-attribute
			      (theme system)
			      :nav-toggler-right
			      :text-color-class))
			    :type "button"
			    :data-toggle "collapse"
			    :data-target "#exNavbarRight"
			    :aria-controls "exNavbarRight"
			    :aria-expanded "false"
			    :aria-label "Toggle system menu"
			    "&#9776;"))))
	     
	     (:div
	      :class "container-fluid"
	      (:div :class "row"
		    (:div
		     :class "collapse show d-print-none "
		     :id "exNavbarLeft"
		     (:br)
		     (cl-who:str (render-left-user-menu)))
		    
		    (:div :class "col"
			 (:br)
			 (cl-who:str body))
		    (:div
		     :class "collapse col-md-2 d-print-none " 
		     :id "exNavbarRight" :style "background-color:#FFFFFF"
		     (:br)
		     (cl-who:str (render-right-menu)))))))


	(cl-who:str (page-footer-js system))
	(cl-who:str footer-js)



	))))

(defun check-user-access ()
  (unless (current-user)
    (hunchentoot:redirect (frmt "~As-wfx?cs=login" (site-url *system*)))))


(defun render-repl ()
  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(:div :class "card"
		      
		      (:div :class "card-block"
			    (:h4 :class "card-title"
				 "Run Script")
			    (:form :method "post"
				   :action ""			       
				   (:input :type "hidden" :id "contextid" 
					   :value (context-id *context*))
				   (:div :class "form-group"
					 (:label :for "script" "Script")
					 (:textarea
					  :class "form-control wfx-script"
					  :rows 20
					  :name "script"
					  :id "script"
					  (cl-who:str
					   (or (parameter "script") ""))))
				   
				   (:button :name "action"
					    :class "btn btn-primary"
					    :type "submit"
					    :value "eval-repl"
					    "run")))
		      (:div :class "card-footer"
			    (cl-who:str
			     (gethash :repl-result (cache *context*)))))))))

(defparameter *script-functions*
  (list 'cl-wfx:frmt
	'cl-wfx::wfx-fetch-context-item
	'cl-wfx::wfx-fetch-context-items
	'cl-wfx:with-html
	'cl-wfx:with-html-string
	'cl-wfx::render-report
	'cl-wfx::parameter
	'cl-who:htm
	'cl-who:str
	'cl-who:esc
	'cl-naive-store:getx))

(defun script-eval-safe (script)
  (let* ((sandbox-impl::*allowed-extra-symbols*
	    *script-functions*)
	  (script-result (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))

      (with-output-to-string (s script-result)
	(let ((sandbox::*msg-value-prefix* "")
	      (sandbox::*msg-error-prefix* "")
	      (sandbox::*msg-value-formatter* "~{~S~^<br/> ~}")
	      (sandbox::*msg-no-value-message* "Nil"))
	  
	  (sandbox::read-eval-print script  s)
	  script-result))))

(defun script-eval (script)  
  (handler-case      
      (list (eval script) nil)
    (error (c)
       (list nil c))))

(defmethod action-handler ((action (eql :eval-repl)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (when (and (parameter "action") (parameter "script")
	     (not (empty-p (parameter "script"))))
    (let ((result (script-eval
		   (read-no-eval
		     (parameter "script")))))
      
      (setf (gethash :repl-result (cache *context*))	   
	    (or (first result) (second result)) ))))


(defun render-set-password ()
  (with-html-string
    (:div :class "row"
	  (:div :class "col"
		(:div :class "card"
		      (:div :class "card-block"
			    (:h4 :class "card-title"
				 "Add New User")
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
					     :value (or (parameter "email") "")))
				   
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
					    :value "set-password"
					    "Set Password"))
			    (:div :class "card-footer"
				  (cl-who:str
				   (gethash :user-not-found-error
					    (cache *context*))))))))))

(defmethod action-handler ((action (eql :set-password)) 
			   (context context) 
			   (request hunch-request)
			   &key &allow-other-keys)

  (when (and (parameter "email") (parameter "password"))
    (let ((user (get-user (parameter "email"))))
      (if user
          (and (change-user user (parameter "password"))
	       (setf (gethash :user-not-found-error (cache *context*))
		     "Password Set!"))
          (setf (gethash :user-not-found-error (cache *context*))
		"User not found!")))))



(defun handle-file (post-parameter)
  (when (and post-parameter (listp post-parameter))
    (destructuring-bind (path filename content-type)
        post-parameter
      (declare (ignore content-type))
      
      ;; strip directory info send by Windows browsers
      (when (search "Windows" (hunchentoot:user-agent) :test #'char-equal)
        (setf filename (ppcre:regex-replace ".*\\\\" filename "")))
     
      (let ((server-path (string-downcase
			  (frmt "~A/~A/~A/files/tmp/~A/~A/"
				(location (universe *system*))
				(parameter "license")
				(parameter "collection")
				(parameter "datatype")
				(parameter "field")))))

	(ensure-directories-exist server-path)

	(fad:copy-file path
		       (merge-pathnames
			(sanitize-file-name filename)
			server-path)
		       :overwrite t)))))


(defgeneric setup-file-upload (system &key &allow-other-keys))

(defmethod setup-file-upload ((system hunch-system)  &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
	(,(intern (frmt "file-upload-~A" (name system))) 
	 :uri ,(frmt "~Acor/file-upload" (site-url system))  
	 :allow-other-keys t)
	nil      
      (when (and (boundp 'hunchentoot:*request*)
				 (hunchentoot:get-parameter "datatype"))
			(handle-file (hunchentoot:post-parameter "file_data")
				     ))
      "{}")))


(defmethod custom-render-context ((system hunch-system)
				 context-spec
				 &key &allow-other-keys)
  (warn "Implement custom-render-context")
  )

(defgeneric render-context (system context-spec-name &key &allow-other-keys))

(defmethod render-context ((system hunch-system) context-spec-name
			    &key &allow-other-keys)

  
  (let ((context-spec (get-context-spec-x context-spec-name)))
    
    (cond ((not context-spec)
	   (check-user-access)
	   (with-html-string
	     "<!doctype html>"
	     (cl-who:str (render-page system
				      "Context not defined."
				      :menu-p t)))
	   )
	  ((not (empty-p (getx context-spec :collection)))
	   (check-user-access)
	   
	   (with-html-string
	     "<!doctype html>"
	     (cl-who:str
	      (render-page system
			   (if (context-access-p context-spec)
			       (render-grid 
				(getx context-spec :collection))
			       "Access Denied")
			   :menu-p t))))
	  ((not (empty-p (getx context-spec :report)))
	   (check-user-access)
	   (with-html-string
	     "<!doctype html>"
	     (cl-who:str
	      (render-page system
			   (if (context-access-p context-spec)
			       (render-report
				:html 
				(digx context-spec :report))
			       "Access Denied"
			       )
			   :menu-p t))))
	  ((string-equal (getx context-spec :name) "repl")
	   (check-user-access)
	   (with-html-string
	       "<!doctype html>"
	       (cl-who:str
		(render-page system
			     (if (context-access-p context-spec)
				 (render-repl)
				 "Access Denied")
			     :menu-p t))))

	  
	  ((string-equal (getx context-spec :name) "file-upload")

	   (when (and (boundp 'hunchentoot:*request*)
				 (hunchentoot:get-parameter "datatype"))
			(handle-file (hunchentoot:post-parameter "file_data"))))
	  ((string-equal (getx context-spec :name) "set password")
	   (with-html-string
	     "<!doctype html>"	
	     (cl-who:str (render-page system
				      (render-set-password)
				      :menu-p t))))
	  ((string-equal (getx context-spec :name) "login")
	   (with-html-string
	     
	     (cl-who:str (render-page system
				      (render-login)
				      :menu-p nil))))
	  ((not (empty-p (getx context-spec :renderer)))
	   
	   (script-eval
		   (read-no-eval
		    (getx context-spec :renderer))))
	  (t
	   (custom-render-context system context-spec)
	   ))
    )
  )

(defmethod setup-context-system ((system hunch-system)  
				 &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler 
	(,(alexandria:symbolicate 
	   (string-upcase (id-string (name system))) 
	   '-dynamic-easy-handler)  
	  :uri ,(frmt "~As-wfx" (site-url system))  
	  :allow-other-keys t) 
      nil
      
      (with-html-string
	(cl-who:str (render-context *system* (parameter "cs")))))))
