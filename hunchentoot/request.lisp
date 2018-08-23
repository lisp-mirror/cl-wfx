(in-package :cl-wfx)

(defmethod parameter* (parameter (request hunch-request))
  (or (hunchentoot:post-parameter parameter (request-object request))
      (hunchentoot:get-parameter parameter (request-object request))))

(defmethod bad-request ((request hunch-request))
  (setf (hunchentoot:return-code hunchentoot:*reply*)
	hunchentoot:+http-bad-request+)
  (hunchentoot:abort-request-handler))


(defun ajax-request-p (request)
  (alexandria:starts-with-subseq 
   (ajax-url (hunchentoot:request-acceptor request))
                                 (hunchentoot:script-name request)))

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

  ;;TODO:: How to register actions? Contexs spec permissions?

  (dolist (trigger (action-handler-triggers system))
    (let ((handler (eval trigger)))
      (when handler
	(action-handler handler context request))))
  
  
    (cond ((find (parameter "action") 
		 (list "save" "delete" "login" "logout"
		       "assign-campaign" "select-action" "grid-select-action"
		       "add-selection" "eval-repl" "set-password"
		       "item-action") 
		 :test #'string-equal)
	   ;;TODO: why do we check against a list of actions and not just fire
	   ;;the action
	   (action-handler (intern (string-upcase (parameter "action")) :keyword)
			   context
			   request))
	  ((parameter "set-licenses")
	   ;;TODO: this is not used any more an ajax call ajax-license-select
	   ;;handles this ... check to delete
	   (break "~A" (hunchentoot:post-parameters*))
	   (action-handler :set-licenses
			   context
			   request))
	  ((parameter "set-entities")	   
	   (action-handler :set-entities
			   context
			   request))))

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
;;  (nid-ajax system)
;;  (rid-ajax system)
;;  (table-ajax system)
;;  (grid-test-ajax system)
;;  (grid-depend-ajax system)
  
  (define-ajax system cl-ajax-render (script-name renderer id)
    (declare (ignore script-name))
    (setf (hunchentoot:content-type*) "text/json")
 
	;;(setf (slot-value *request* 'script-name) script-name)
	(let ((*context* (find-request-context)))
	    (declare (special *context*))
	    (with-debugging

	      ;;(map-cache #'synq-cache *sfx-context*)
	      ;;  (map-dom #'synq-data)
	      ;;(map-cache-events *sfx-context*)

	      (when renderer
		  ;;  (enable-notifications)
		(json:encode-json-to-string
		 (list (render-to-string* renderer 
					  :id id :from-ajax t)
		       ;;(deferred-js)
		       )))))))

(defmethod load-context-specs :after ((system hunch-system)
				      &key &allow-other-keys)
  (load-default-ajax system))


