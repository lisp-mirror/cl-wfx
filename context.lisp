(in-package :cl-wfx)

(defclass context ()
  ((context-id :initarg :context-id
	       :accessor context-id
	       :initform (random 10810))
   (module :initarg :module
	   :accessor module
	   :initform nil)
   (context-spec :initarg :context-spec
		       :accessor context-spec
		       :initform nil)
   (session :initarg :session
	    :accessor session
	    :initform nil)
   (url :initarg :url
	:accessor url
	:initform nil)
   (new-p :initarg :new-p
	  :accessor new-p
	  :initform t)
   (reuse-p :initarg :reuse-p
	    :accessor reuse-p
	    :initform nil)
   (widget-sequence :initarg :widget-sequence
		    :accessor widget-sequence
		    :initform nil)
   (cache :initarg :cache
	     :accessor cache
	     :initform (make-hash-table :test #'equalp)))
  (:documentation "An instance of a context within the current user session."))

(defgeneric start-context (module session context-name &key &allow-other-keys)
  (:documentation "Creates a context instance."))

(defgeneric setup-context (module context-spec system)
  (:documentation "To be used to setup a context instance.."))

(defgeneric init-context (context session )
  (:documentation "To be used to setup a context instance.."))

;;TODO: called by system (in init.lsip) to init permissions and ????
(defmethod init-context ((context context) session)
    ;;TODO: Go look in old insite code what needs to go here? Page setup stuff?
 ;; (setf  (permissions contexat) nil)
  
 ;;  (setf (request-page-permissions context)        (setup-page-permissions context))
  )

