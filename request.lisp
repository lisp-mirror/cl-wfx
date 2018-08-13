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

(defgeneric process-sys-request (system processor request &key &allow-other-keys)
  (:documentation "Processes a system request using the passed processor."))

(defgeneric system-request (system request &key &allow-other-keys)
  (:documentation "Processes system requests."))

;;TODO: is this the right way to get module
(defmethod system-request ((system system) (request request)
			   &key &allow-other-keys)
  (let* ((*request* request)
	 (*system* system)
	 (*session* (start-session system))
	 (*context* (request-context request)))
    
    (declare (special *system* *context* *session* *request*))
    (process-sys-request system *context* request )))

(defvar *context-id-lock* (bordeaux-threads:make-lock))

(defun generate-context-id ()
  (declare (optimize speed))
  (format nil "~:@(~36r~)" (random (expt 2 32))))

(defun generate-new-context (session context-spec)
  (let* ((contexts (contexts session))
	 (context (make-instance 'context 
				 :context-spec context-spec)))

    (bordeaux-threads:with-lock-held (*context-id-lock*)
      (loop for id = (generate-context-id)
	 unless (gethash id contexts)
	 do (setf (context-id context) id
		  (gethash id contexts) context)
	 return id))
    context))

(defmethod start-context ((session session) context-spec  &key id request)
  (let* ((id (or id (parameter* "contextid" request)))
	 (context (if id
		     (or (gethash id (contexts session))
			 (bad-request request))
		     (generate-new-context session context-spec))))
    (setf (session context) session)
    (init-context context session)
    context))
