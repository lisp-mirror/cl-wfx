(in-package :cl-wfx)

(defclass context ()
  ((context-id :initarg :context-id
	       :accessor context-id
	       :initform (random 10810))
   (context-spec :initarg :context-spec
		       :accessor context-spec
		       :initform nil)
   (session :initarg :session
	    :accessor session
	    :initform nil)
   (url :initarg :url
	:accessor url
	:initform nil)
   (cache :initarg :cache
	     :accessor cache
	     :initform (make-hash-table :test #'equalp)))
  (:documentation "An instance of a context within the current user session."))

(defgeneric context-parameter (parameter))

(defmethod context-parameter (parameter)
  (if *context*
      (gethash parameter (cache *context*))))

(defmethod (setf context-parameter) (value parameter)
  (if *context*
      (setf (gethash parameter (cache *context*)) value)))

(defgeneric start-context (session context-name &key &allow-other-keys)
  (:documentation "Creates a context instance."))

(defgeneric setup-context (context-spec system  &key &allow-other-keys)
  (:documentation "To be used to setup a context instance.."))

(defgeneric init-context (context session )
  (:documentation "To be used to setup a context instance.."))

;;TODO: called by system (in init.lsip) to init permissions and ????
(defmethod init-context ((context context) session)
    ;;TODO: Go look in old insite code what needs to go here? Page setup stuff?
 ;; (setf  (permissions contexat) nil)
  
 ;;  (setf (request-page-permissions context)        (setup-page-permissions context))
  )


(defun context-log (object)
  (when *context*
    (setf (gethash :debug-log (cache *context*))	   
	  (push object (gethash :context-log (cache *context*))) )))

(defun clear-context-log ()
  (setf (gethash :context-log (cache *context*)) nil))
