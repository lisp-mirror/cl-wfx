(in-package :cl-wfx)


(monkey-lisp::define-monkey-macro shit-shat (&key id from-ajax)
  `(:div "No idea wtf?????????????????"
	 (:div ,id ,from-ajax)))


(defun render-login ()
  (monkey-html-lisp:htm
    (:div :class "card"

     (:img :class "card-image-top" :src "/images/insite-logo.png")
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
				"Login"
				)))
     (:div :class "card-footer"
	   (gethash :login-error (cache *context*))
	   )
     )))



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
						:user user))))

    (setf (user *session*) active-user))
  
  ;;(init-user-session user)

;;  (log-login "Login" (email login) "Passed" "Login passed.")

  (hunchentoot:redirect "/cl-wfx/sys/data-specs" ;;(default-context *system*)
   ))

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
      
      (let* ((shit (monkey-lisp:monkey-lisp 
		       (:processor-class cl-wfx::context-data-spec-processor)
		     ,(data-spec-script (get-data-spec (getf 
							(cdr (context-spec-script spec))
							:name))))))
	(monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page shit))))))

(defmethod setup-context-login ((module module) (spec context-spec) system)  
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
      
      (monkey-html-lisp:with-html
	  "<!doctype html>"
	  (render-page 
	   (render-login))))))

