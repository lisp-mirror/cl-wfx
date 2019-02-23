(in-package :cl-wfx)

(defmethod parameter* (parameter (request hunch-request))
  (or (hunchentoot:post-parameter parameter (request-object request))
      (hunchentoot:get-parameter parameter (request-object request))))

(defmethod bad-request ((request hunch-request))
  (setf (hunchentoot:return-code hunchentoot:*reply*)
	hunchentoot:+http-bad-request+)
  (hunchentoot:abort-request-handler))


(defmethod request-context ((request hunch-request) &key &allow-other-keys)
  (let* ((split
	    (split-sequence:split-sequence
	     #\/
	     (hunchentoot:script-name (request-object request))))
	   (sys (second split))

	   (context-name (parameter "cs"))
	   (context-spec (get-context-spec-x context-name)))
      (declare (ignore sys))

      (start-context *session* context-spec
		       :id (or (parameter "i") (parameter "contextid"))
		       :request request)))

(defgeneric action-handler (action context request
				&key &allow-other-keys))




(defmethod process-sys-request ((system hunch-system)
				(context context) 
				(request hunch-request)
				&key &allow-other-keys)


  (let ((allowed-actions (list "save" "delete" "login" "logout"
			       "assign-campaign" "select-action" "grid-select-action"
			       "add-selection" "eval-repl" "set-password"
			       "item-action"))
	(action-parameters (list "wfxaction" "set-entities")))

    (when (action-parameter-allowed-values system)
      (setf allowed-actions (append allowed-actions (action-parameter-allowed-values system))))

    (when (action-parameters system)
      (setf action-parameters (append action-parameters (action-parameters system))))

    
    (dolist (action-parameter action-parameters)
      (when (parameter action-parameter)
	(if (string-equal action-parameter "wfxaction")
	    (progn
	    
	      (when (find (parameter action-parameter) allowed-actions :test 'string-equal)
		
		(action-handler (intern (string-upcase (parameter action-parameter)) :keyword)
				context
				request)))
	    (action-handler (intern (string-upcase (parameter action-parameter)) :keyword)
		    context
		    request)

	    )))
     ))

(defun ajax-request-p (request)
  (alexandria:starts-with-subseq 
   (ajax-url (hunchentoot:request-acceptor request))
   (hunchentoot:script-name request)))

(defmethod system-request ((acceptor hunch-system) (request hunch-request) 
			   &key &allow-other-keys)
  (process-sys-request acceptor *context* request))

(defun dont-process (system request)
  (let ((script-name (hunchentoot:script-name request))
	(default-exclusions
	 (list ".js"
	       ".css"
	       ".jpg"
	       ".png"
	       ".gif"
	       ".ico"
	       ".woff"
	       ".woff2"
	       ".ttf"
	       "file-upload")))

    (dolist (exclusion (concatenate 'list
				     default-exclusions
				     (request-exclusions system)))
      (when (search exclusion script-name)
	(return-from dont-process t)))
    nil))

(defmethod hunchentoot:handle-request :around ((acceptor hunch-system) request)
  (with-debugging
    (let ((*request* (make-instance 'hunch-request :request-object request))
	  (*system* acceptor)
	  (dont (dont-process acceptor request))
	  (*current-theme* (theme acceptor)))
      (declare (special *current-theme*))
      (hunchentoot:start-session)

      (if (not dont)           
	  (let* ((*session* (start-session acceptor))
		 (*context* (request-context *request*))
		 ;;TODO: is this the right way to get module
		 )
	    (declare (special *system* *context* *session* *request*))
	
	    (unless *context*
	      (bad-request *request*))
	    (when *context*
	      (system-request acceptor *request*))	 
	 
	    (call-next-method))
	  (progn
	    (call-next-method))))))

(defvar *widget-parameters*)

(defun defer-js (format-arguments &rest args)
  (when format-arguments
    (push (apply #'format nil format-arguments args)
          (getf *widget-parameters* :javascript-defer))))

(defun defer-js-function (code)
  (push code (getf *widget-parameters* :javascript-defer-function)))

(defun deferred-js ()
  (format nil "$(document).ready(function(){~{~a;~}});~{~a~}"
          (getf *widget-parameters* :javascript-defer)
          (getf *widget-parameters* :javascript-defer-function )))

(defun js-link (&rest code)
  (frmt "javascript:{~{~a~^,~}}" code))

(defun js-pair (key value)
  (frmt "[~s, ~s]" key value))

(defun js-value (id)
  (frmt "[~s, document.getElementById(~s).value]"
            id id))

(defun js-value-name (name)
  (frmt "[~s, document.getElementsByName(~s).value]"
            name name))

(defun js-context ()
  "[\"contextid\", contextid]")

(defun js-checkbox-value (name)
  (frmt "[~s, $(\"#~a\").prop(\"checked\")]"
            name name))

(defun find-request-context (&optional (id (parameter "contextid")) (error t))
  (or (gethash id (contexts *session*))
      (and error
	   (bad-request *request*))))

(defun load-default-ajax (system)
  
  (define-ajax system cl-ajax-render (script-name renderer id)
    (declare (ignore script-name))
    (setf (hunchentoot:content-type*) "text/json")
 
	(let ((*context* (find-request-context)))
	  (declare (special *context*))

	  
	  
	  (with-debugging

	      (when renderer
		
		(json:encode-json-to-string
		 (list (render-to-string* renderer 
					  :id id :from-ajax t)
		      
		       )))))))

(defmethod load-context-specs :after ((system hunch-system)
				      &key &allow-other-keys)
  (load-default-ajax system))


