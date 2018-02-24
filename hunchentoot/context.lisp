(in-package :cl-wfx)

(defun render-login ()
  (with-html-string
      (:div :class "row"
	    (:div :class "card col-5"
		  (:img :class "card-image-top"
			:src  (frmt "~Acor/web/images/logo-login.png"
				    (site-url *system*)))		  
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
			(cl-who:str (gethash :login-error (cache *context*))))))))



(defun context-url (spec module)
  (let* ((short (if spec
		   (digx module :module-short)
		   "sys"))
	(spec (or spec (get-context-spec (get-store-from-short-mod 
					  short)
					 (default-context *system*)))))
    
    (frmt "~A~A/~A" (site-url *system*) 
	  (string-downcase 
	   (id-string short))
	  (if (and spec (digx spec :url))
	      (digx spec :url)
	      (string-downcase 
	       (id-string (if spec
			      (digx spec :name)
			      (default-context *system*))))))))

(defun clear-hash-items (hash)
  (when hash
    (loop for key being the hash-keys of hash	 
       do (remhash key hash))))

(defmethod on-success (user)

  (when (current-user)
    (when *session*
      (clear-hash-items (cache *session*))
      (clear-hash-items (contexts *session*))
       (setf (user *session*) nil))
    ;;(hunchentoot:remove-session *session*)
    )
  
  (dolist (license-code (getx user :license-codes))
    (init-license-universe *system* license-code))
  
  ;; (break "~A" (core-collection "active-users"))
  
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
  
  ;;(init-user-session user)

  ;;  (log-login "Login" (email login) "Passed" "Login passed.")

  ;;TODO: Handle default page other than cor pages....
  (hunchentoot:redirect (context-url nil *module*)))

(defmethod on-failure ()
  ;; (log-login "Login" (get-val login 'email) "Failed" "User name or password incorrect.")
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
  
  (hunchentoot:redirect (frmt "~Acor/login" (site-url *system*))))


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
			     (:input :class "form-check-input" :type "checkbox" 
				     :disabled "")))
			(cl-who:str (getx entity :name)))))
      
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

  (setf (getf (item-values (active-user)) :selected-entities) nil)
 
  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "tree-entity-id")
     
      (dolist (license-code (digx (active-user) :selected-licenses))
	
	(dolist (entity (digx (license-user license-code)
			      :accessible-entities))	  
	  (when (string-equal (frmt "~A" (item-hash entity)) (cdr parameter))
	    (pushnew (item-hash entity)
		     (getx (active-user) :selected-entities))
	    (persist-item (core-collection "active-users") (active-user))))))))

(defun render-licence-codes ()
  (with-html-string
    (dolist (code (digx (current-user) :license-codes))    
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
      
      (persist-item (core-collection "active-users") (active-user))
      )))

(defun context-access-p (context)
  (let ((access-p))

    (when (and (active-user) (digx (active-user) :selected-licenses))
     ;; (break "~A" (current-user))
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
		       
		       (setf access-p t)))))))
      )
    access-p))

(defun render-user-admin-menu ()
  (with-html-string
    (:div :class "nav-item dropdown"	  
	  (:a :class "nav-link dropdown-toggle" 
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
		  
		 ; (break "sys-mod ~A" (digx sys-mod :menu))
		  (dolist (menu (digx sys-mod :menu))
		    (when (equalp (digx menu :name) "System")
		      
		      (dolist (item (digx menu :menu-items))

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
			  
			  (when (or (equalp (digx item :name) "Logout")
				    (context-access-p
				     (digx item :context-spec)))
			    
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
				 
				 (cl-who:str (digx item :name))))))))))))))

(defun render-entity-display ()
  (with-html-string
    (:span :class "navbar-text mr-auto"	   
	   (cl-who:str
	    (frmt
	     "Entities: ~A"
	     (when (active-user)
	       (let ((entities))
		 (dolist (license-code (digx (active-user)
					     :selected-licenses))
		   
		   (dolist (entity (digx (license-user license-code)
					 :accessible-entities))
		     (dolist (selected (getx (active-user)
					     :selected-entities))
		       (when (equalp (item-hash entity)
				     selected)
			 (if (not entities)
			     (setf entities (getx entity :name))
			     (setf entities
				   (frmt "~A|~A"
					 entities
					 (getx entity :name) )))))))
		 entities)))))))

(defun data-menu (menu-items)
  (let ((items))
    (dolist (item menu-items)
      (when (and
	     (digx item :context-spec)
	     (context-access-p
	      (digx item :context-spec))
	     (getx (digx item :context-spec) :collection))
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

(defun render-menu-item (mod item)
  (with-html-string
   (:a :class "nav-item nav-link bg-light"
       :href (context-url
	      (digx item :context-spec)
	      mod)
       (cl-who:str (digx item :name)))))

(defun render-left-user-menu ()
  (with-html-string
    (:nav :class "nav flex-column"
	  (dolist (mod (user-mods))
	   
	    (dolist (menu (digx mod :menu))
	      (cl-who:htm
	       (:span :class "nav-item "
		      (:strong "Data")))
	      (dolist (item (data-menu (digx menu :menu-items)))
		(cl-who:str (render-menu-item mod item)))
	      (cl-who:htm
	       (:span :class "nav-item"
		      (:strong "Reports")))
	      (dolist (item (report-menu (digx menu :menu-items)))
		(cl-who:str (render-menu-item mod item)))
	      (cl-who:htm
	       (:span :class "nav-item"
		      (:strong "Other")))
	      (dolist (item (other-menu (digx menu :menu-items)))
		(cl-who:str (render-menu-item mod item)))
	      )
	    ))
    ))

(defun render-right-menu ()
  (with-html-string

    (:form
      (:div :class "row bg-faded"
	    "Accessible License Codes")
      (cl-who:str (render-licence-codes))				 
      
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
	     (when (license-user license-code)
	       (cl-who:str
		(render-entity-tree 
		 license-code
		 (getx (license-user license-code)
		       :accessible-entities))))))
    ))

(defun render-page (menu-p body)
  
  (with-html-string
      (:html
       (:head

	(cl-who:str
	 (frmt  "<link rel=\"stylesheet\" href=\"~Aweb/font-awesome-4.7.0/css/font-awesome.min.css\">
" (site-url *system*)))

	"<script  src=\"https://code.jquery.com/jquery-3.2.1.min.js\" integrity=\"sha256-hwg4gsxgFZhOsEEamdOYGBf13FyQuiTwlAQgxVSNgt4=\"  crossorigin=\"anonymous\"></script>"
	
	"<script src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js\" integrity=\"sha384-vFJXuSJphROIrBnz7yo7oB41mKfc8JzQZiCq4NCceLEaO4IHwicKwpJf9c9IpFgh\" crossorigin=\"anonymous\"></script>
"
	"<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css\" integrity=\"sha384-PsH8R72JQ3SOdhVi3uxftmaW6Vc51MKb0q5P2rRUpPvrszuE4W1povHYgTpBfshb\" crossorigin=\"anonymous\">"

	"

<link href=\"https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.5/css/fileinput.min.css\" media=\"all\" rel=\"stylesheet\" type=\"text/css\" />

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.5/js/fileinput.min.js\"></script>

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.5/themes/fa/theme.min.js\"></script>

<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js\" integrity=\"sha384-alpBpkh1PFOepccYVYDB4do5UnbKysX5WZXm3XxPqe5iKTfUKjNkCk9SaVuEZflJ\" crossorigin=\"anonymous\"></script>"

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
	(cl-who:str
	 (frmt  "<script src=\"~Aweb/cl-wfx.js\"></script>
" (site-url *system*)))

	(cl-who:str
	 (frmt  "<link rel=\"stylesheet\" href=\"~Aweb/cl-wfx.css\">
" (site-url *system*)))	
	"<link href=https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/css/bootstrap-datepicker3.standalone.min.css' rel='stylesheet>
 <script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/js/bootstrap-datepicker.min.js></script>
")
       
       
       (:body 
	(:input :type "hidden" :id "contextid" :value (context-id *context*))

	(if (not menu-p)	 
	    (cl-who:htm
	     (:div :class "container"
		   (cl-who:str body)))
	    (cl-who:htm
	     (unless (active-user)
	       (hunchentoot:redirect (frmt "~Acor/login" (site-url *system*))))
	     (:nav 
	      :class "navbar sticky-top hidden-print justify-content-between bg-white"

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
	      
	      (:a :class "navbar-brand" :href "#" 
		  (:img :src (frmt "~Acor/web/images/logo-small.png"
				   (site-url *system*)))
		  (name *system*))

	      (cl-who:str (render-entity-display))
	      
	      (cl-who:str (render-user-admin-menu))
	      
	      (if (current-user)
		  (cl-who:htm
		   (:button :class "navbar-toggler navbar-toggler-right"
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
		     :class "collapse col-md-2 col-md-auto show hidden-print"
		     :id "exNavbarLeft"
		     
		     (cl-who:str (render-left-user-menu)))

		    (:div :class "col"
		     (cl-who:str body))
		    (:div
		     :class "collapse col-md-2 hidden-print " 
		     :id "exNavbarRight" :style "background-color:#FFFFFF"
		     (cl-who:str (render-right-menu)))))))


	"<script>$(document).on('click', '.dropdown-item', function(){
       var selVal = $(this).children().first();
       var selText = $(this).text();
       $(this).parents('.dropdown').find('.dropdown-toggle').html($.trim(selText));
       $(this).parents('.dropdown').find('.selected-value').val($(selVal).val());
});</script>"

	"<script>$(document).on('click', '.auto-complete-item', function(){
       var selVal = $(this).children().first();
       var selText = $(this).text();

       $(this).parents('.auto-complete').find('.auto-complete-text').val($.trim(selText));
       $(this).parents('.auto-complete').find('.selected-value').val($(selVal).val());
       $(this).parents('.auto-complete').find('.auto-list').empty();
});
</script>"
#|
	(:script :type "text/javascript"
	 (cl-who:str
	  "$(document).ready(function() {
        $('.wfx-script').each(function(i,textarea) {
               
     	  	editor = CodeMirror.fromTextArea(textarea, {
                lineNumbers: true,
                smartIndent: true,
          	autoCloseBrackets: true,
 		showTrailingSpace: true,
                matchBrackets: true,
          	    mode: \"text/x-common-lisp\"});
          editor.display.wrapper.style.fontSize = \"12px\";
          editor.refresh();
function updateTextArea() {
    editor.save();
}
myEditor.on('change', updateTextArea);
});});")
	 )
|#

	(:script :type "text/javascript"
	 (cl-who:str
	  "$(document).ready(function() {
              $(\".file-upload\").each(function (i,file) {

                 file.fileinput({
                 uploadUrl: \"/cor/file-upload\",
                 uploadAsync: false,
                 theme: \"fa\",
                  initialPreviewAsData: true,
                 initialPreview: [$(\"#init\" + file.id).val() ],
                 maxFileCount: 1})});
            });")
	 )

	(:script :type "text/javascript"
	 (cl-who:str
	  "function gridSelectAll() {

              $(\".grid-selection\").each(function (i,checkbox) {

                checkbox.checked = $(\"#grid-select-all\").is(\":checked\");
                })
             };
            ")
	 )
	
	(:script :type "text/javascript"
	 (cl-who:str (frmt	      
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

}" (site-url *system*))))))))

(defun check-user-access ()
  (unless (current-user)
    
    (hunchentoot:redirect (frmt "~Acor/login" (site-url *system*)))))

(defmethod setup-context ((module item) (context-spec item)
			  (system hunch-system)  
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
      (with-html-string
	  "<!itemtype html>"
	(cl-who:str (render-page t (render-grid 
				    ,(digx context-spec :collection))))))))

(defmethod setup-context-report ((module item) (context-spec item)
				 (system hunch-system)  
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
      (with-html-string
	  "<!itemtype html>"
	  (cl-who:str (render-page t (render-report
				      :html 
				      ,(digx context-spec :report))))))))


(defgeneric setup-context-login (module context-spec system
				 &key &allow-other-keys))

(defmethod setup-context-login ((module item) (context-spec item)
				(system hunch-system)
				&key &allow-other-keys)  

  (eval
   `(hunchentoot:define-easy-handler
	(,(alexandria:symbolicate 
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
      (with-html-string
	  "<!itemtype html>"
	  (cl-who:str (render-page nil (render-login)))))))

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

(defgeneric setup-context-repl (system &key &allow-other-keys))

(defmethod setup-context-repl ((system hunch-system)  &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
	(repl 
	  :uri ,(frmt "~Acor/repl" (site-url system))  
	  :allow-other-keys t)
	nil
      (with-html-string
	"<!itemtype html>"
	
	(cl-who:str (render-page t (render-repl)))))))


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
					    (cache *context*)))))
		      )))))

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

(defgeneric setup-context-set-password (system &key &allow-other-keys))

(defmethod setup-context-set-password ((system hunch-system)  &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
	(setup-password
	  :uri ,(frmt "~Acor/set-password" (site-url *system*))  
	  :allow-other-keys t)
	nil
      (with-html-string
	"<!itemtype html>"	
	(cl-who:str (render-page t (render-set-password)))))))

(defparameter *unsecure-upload-dir* "/home/phil/Temp/shit/")

(defun handle-file (post-parameter)
  ;;(ht-log :info "Handling file upload with params: '~A'." post-parameter)

 ;; (break "???")
  (when (and post-parameter (listp post-parameter))
    ;; (break "You got here with: ~A." post-parameter)
    (destructuring-bind (path filename content-type)
        post-parameter
      (declare (ignore content-type))
      
      ;; strip directory info send by Windows browsers
      (when (search "Windows" (hunchentoot:user-agent) :test #'char-equal)
        (setf filename (ppcre:regex-replace ".*\\\\" filename "")))
     
      ;;(break "~A" path)
      (let ((server-path (string-downcase
			  (frmt "~A/~A/~A/files/tmp/~A/~A/"
				(location (universe *system*))
				(parameter "license")
				(parameter "collection")
				(parameter "datatype")
				(parameter "field")))))

	(ensure-directories-exist server-path)


	(fad:copy-file path
		       (merge-pathnames (string-downcase
					 (replace-all filename "_" "-"))
					  server-path)
		       :overwrite t)))
    ))


(defgeneric setup-file-upload (system &key &allow-other-keys))

(defmethod setup-file-upload ((system hunch-system)  &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
	(file-upload 
	 :uri ,(frmt "~Acor/file-upload" (site-url system))  
	 :allow-other-keys t)
	nil      
      (when (and (boundp 'hunchentoot:*request*)
				 (hunchentoot:get-parameter "datatype"))
			(handle-file (hunchentoot:post-parameter "file_data")
				     ))
      "{}")))

#|
(hunchentoot:define-easy-handler (upload-file :uri "/cor/file-upload") ()
 ;; (break "~A~%~A"  (hunchentoot:post-parameters*)	 (hunchentoot:get-parameters*))
  (let ((uploaded (when (and (boundp 'hunchentoot:*request*)
			      (hunchentoot:get-parameter "datatype"))
		     (handle-file (hunchentoot:post-parameter "file_data")
				  )))))
  
  "{}")

|#
