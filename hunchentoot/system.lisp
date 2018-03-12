(in-package :cl-wfx)

(defclass hunch-system (system hunchentoot:easy-acceptor)
  ((site-url :initarg :site-url
             :initform (error "site-url should be provided")
             :accessor site-url
	     :documentation	     
	     "Not the same as hunchentoot acceptor address! 
Must be of the form  /my-system/, it is used to 
differentiate systems in hunchentoot parlance 
because hunchentoot does not have vhosts by default.")
   (ajax-processor :initarg :ajax-processora
		   :accessor ajax-processor
		   :initform nil)
   (ajax-url :initarg :ajax-url           
             :accessor ajax-url
	     :initform nil
	     :documentation "Must be of the form /my-system/ajax/")
   (image-processor :initarg :image-processor
		    :accessor image-processor
		    :initform nil)   
   (image-url :initarg :image-url
	      :accessor image-url
	      :initform "/images")
   (default-context :initarg :default-context
	      :accessor default-context
	      :initform nil)
   (request-exclusions
    :initarg :request-exclusions
    :accessor request-exclusions
    :initform nil
    :documentation "Hunchentoot scripts/pages not to be processed by request handler. Does partial matching of these with search against script-name.")
   (action-handler-triggers
    :initarg :action-handler-triggers
    :accessor action-handler-triggers
    :initform nil
    :documentation "Must return handler to use based on logic applied to parameters.")
   (theme :initarg :theme
	  :accessor theme
	  :initform nil)))

(defmacro define-ajax (system name lambda-list &body body)
   `(ht-simple-ajax:defun-ajax ,name ,lambda-list ((ajax-processor ,system))
       ,@body))

;;TODO: Does this have to be a function because it gets passed to
;;the prefix-dispatcher or can we make it a method and specialize on system??
(defun ajax-call-lisp-function (system processor)
  "This is called from hunchentoot on each ajax request. It parses the
   parameters from the http request, calls the lisp function and returns
   the response."
  
  (let* ((*system* system)
	 (fn-name (string-trim
		   "/"
		   (subseq (hunchentoot:script-name* hunchentoot:*request*)
			   (length (ht-simple-ajax::server-uri processor)))))
         (fn (gethash fn-name (ht-simple-ajax::lisp-fns processor)))
         (args (mapcar #'cdr
		       (hunchentoot:get-parameters* hunchentoot:*request*))))
    (unless fn
      (error "Error in call-lisp-function: no such function: ~A" fn-name))

    (setf (hunchentoot:reply-external-format*) 
	  (hunchentoot:reply-external-format processor))
    (setf (hunchentoot:content-type*) (hunchentoot:content-type processor))
    (hunchentoot:no-cache)
    
    ;;TODO: Theming ... (theme site)..(theme system)
    
    (let ((*current-theme* (theme system)))
      (declare (special *current-theme*))
      (apply fn args))))

;;TODO: The only reason why system is passed to ajax-call-lisp-function is for the theme???
(defun create-ajax-dispatcher (system processor)
  "Creates a hunchentoot dispatcher for an ajax processor"
  (hunchentoot:create-prefix-dispatcher (ht-simple-ajax::server-uri processor)
                            (lambda () (ajax-call-lisp-function system processor))))


(defmethod start-sys ((system hunch-system) &key &allow-other-keys)
  (assert-ends-with-/ (site-url system))
    
  (hunchentoot:start system)


  (let* ((ajax-processor
	  (make-instance 'ht-simple-ajax:ajax-processor
			 :server-uri (frmt "~Aajax" (site-url system))))
	 (ajax-prefix-dispatcher
	  (create-ajax-dispatcher system ajax-processor))

	 (file-dispatcher-sys-web
	  (hunchentoot:create-folder-dispatcher-and-handler
	   (frmt "~Aweb/" (site-url system)) 
	   (system-web-folder system)))
	 
	 (file-dispatcher-web
	  (hunchentoot:create-folder-dispatcher-and-handler
	   (frmt "~Acor/web/" (site-url system)) 
	   (web-folder system))))
    
    (pushnew ajax-prefix-dispatcher hunchentoot:*dispatch-table*)
    (pushnew file-dispatcher-web hunchentoot:*dispatch-table*)
    (pushnew file-dispatcher-sys-web hunchentoot:*dispatch-table*)
      
    (setf (ajax-processor system) ajax-processor))
  system)

