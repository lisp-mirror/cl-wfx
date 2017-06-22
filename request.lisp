(in-package :cl-wfx)

(defvar *request* nil)

(defclass request ()
  ((parameters :initarg :parameters
	       :accessor parameters))
  (:documentation "System request object."))

(defgeneric bad-request (request))

(defgeneric parameter* (parameter request))
(defgeneric parameter (parameter))

(defmethod parameter (parameter)
  (parameter* parameter *request*))

(defmethod parameter* (parameter (request request))
  (getf (parameters request) parameter))



(defgeneric request-context (request &key &allow-other-keys))

(defmethod request-context ((request request) &key &allow-other-keys))

(defgeneric process-sys-request (processor request &key &allow-other-keys)
  (:documentation "Processes a system request using the passed processor."))

#|
(defmethod process-sys-request (context request &key &allow-other-keys)
  (break "process-sys-request needs to be implemented"))
|#

(defgeneric system-request (system request &key &allow-other-keys)
  (:documentation "Processes system requests."))

;;TODO: is this the right way to get module
(defmethod system-request ((system system) (request request) &key &allow-other-keys)
  (let* ((*request* request)
	 (*system* system)
	 (*session* (start-session system))
	 (*context* (request-context request))
	 (*module* (module *context*))
	 )
    (declare (special *system* *context* *session* *request*))
    (break "system-request")
    (process-sys-request *context* request )

    ))

(defvar *context-id-lock* (bordeaux-threads:make-lock))

(defun generate-context-id ()
  (declare (optimize speed))
  (format nil "~:@(~36r~)" (random (expt 2 32))))

(defun generate-new-context (module session name)
  
  (let* ((contexts (contexts session))
	 (context-spec (data-from-items (contexts module)
				       :test (lambda (item)
					    ;;TODO: split the id-string comaparison out in own method
					    
					       (or (string-equal name (name item))
						   (string-equal name 
								 (string-downcase 
								  (id-string (name item))))
						   ))))
	 
	 (context (make-instance 'context 
				 :module module
				 :context-spec context-spec)))
    
    ;;TODO: figure this shit out! NID and RID
    (unless context-spec
     ;; (break "~A ~A ~A" context-spec name (type-of name))
      
      )
    
    
    (bordeaux-threads:with-lock-held (*context-id-lock*)
      (loop for id = (generate-context-id)
	 unless (gethash id contexts)
	 do (setf (context-id context) id
		  (gethash id contexts) context)
	 return id))
    context))

(defmethod start-context ((module module) (session session) context-name  &key id request)
  (let* ((id (or id (parameter* "contextid" request)))
	 (context (if id
		     (or (gethash id (contexts session))
			 (bad-request request))
		     (generate-new-context module session context-name))))
    (setf (session context) session)
    (init-context context session)
    context))
