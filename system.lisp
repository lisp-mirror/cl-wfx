(in-package :cl-wfx)

(defclass system ()
  ((name :initarg :name
		:accessor name
		:initform nil
		:documentation "Name of the system")  
   (system-status :initarg :system-status
		  :accessor system-status
		  :initform nil
		  :documentation		  
		  ":initialized :stopped :started :locked")
   (system-folder :initarg :system-folder
		  :accessor system-folder
		  :initform "/system/"
		  :documentation		  
		  "Path where files generated by the system should be stored.") 
   (web-folder :initarg :web-folder
	       :accessor web-folder
	       :initform "/web/")
   
   (system-web-folder :initarg :system-web-folder
		      :accessor system-web-folder
		      :initform "/web/")
   (system-email :initarg :system-email
	       :accessor system-email
	       :initform nil
	       :documentation "Used to send system mails, like password reset codes.")
   (universe :initarg :universe
	     :accessor universe
	     :initform (make-instance 'wfx-universe :location "~/data-universe/")
	     :documentation "") 
   (data-definitions :initarg :data-definitions
		     :accessor data-definitions
		     :initform nil)
   (sessions :initarg :sessions
	     :accessor sessions
	     :initform (make-hash-table :test #'equalp))
   (debug-errors-p :initarg :debug-errors-p
		   :accessor debug-errors-p
		   :initform nil
		   :documentation "To put the system into debug mode while running.")
   (logging-p :initarg :logging-p
              :initform nil
              :accessor logging-p
	      :documentation "Indicate if logging should happen at run time."))
  (:documentation   
"This is an conceptual framework to help package
the functionality of an application. Most systems
with a ui have the same basic structure/needs."))

(defmacro with-system (system &body body)
  :documentation "Makes *system* available."
  `(let ((*system* ,system))
     ,@body))

(defgeneric init-core-universe (system &key &allow-other-keys)
  (:documentation "Initialize data storage for system.
Notes:
This is called on initialize-instance :after for cl-wfx core."))

(defgeneric init-system-universe (system &key &allow-other-keys)
  (:documentation "Initialize data storage for system.
Notes:
This is called on initialize-instance :after for system."))

(defgeneric init-license-universe (system licenses &key &allow-other-keys)
  (:documentation "Initialize data storage for licenses."))


(defgeneric init-system (system &key &allow-other-keys)
  (:documentation "Method to be used to implement system initialization tasks.
Notes:
This is called on initialize-instance :after for system."))

(defmethod  init-system ((system system) &key &allow-other-keys)
  (unless (system-folder system)
    (setf (system-folder system) (format nil "~~/~A" (id-string (name system))))
    (ensure-directories-exist (system-folder system))))

(defmethod initialize-instance :after ((system system) &rest args)  
  (declare (ignore args))
  :documentation "Dont mess with this just use init-system and init-sys-universe effectively."
  (let ((*system* system))
    (init-system system)  
    (init-core-universe system))
  (setf (system-status system) :initialized))

(defgeneric start-sys (system &key &allow-other-keys)
  (:documentation "Method to be used to implement system startup tasks.
Notes:
Must be called by the framework implementer to get a system going.
Calls in :after in order:

 load-context-specs
 load-modules"))

(defgeneric load-modules (system &key &allow-other-keys))

(defgeneric load-context-specs (system &key &allow-other-keys))


(defgeneric ensure-core-user (system &key &allow-other-keys))

(defgeneric ensure-system-user (system &key &allow-other-keys))

(defgeneric ensure-demo-license (system &key &allow-other-keys))

(defmethod start-sys :around ((system system) &key &allow-other-keys)
  (when (or (not (system-status system)) 
	    (equalp (system-status system) :initialized) 
	    (equalp (system-status system) :stopped) )

    (call-next-method)))

(defmethod start-sys ((system system) &key &allow-other-keys)
)

(defmethod start-sys :after ((system system) &key &allow-other-keys)
  (let ((*system* system))
    (declare (special *system*))

    (init-system-universe system)
    
    (ensure-core-user system)
    (ensure-system-user system)
    (ensure-demo-license system)
  
    (load-context-specs system)
    (load-modules system)

    (setf (system-status system) :started)

    system
    ))


(defgeneric stop-sys (system &key &allow-other-keys)
  (:documentation "Method to be used to implement system cleanup tasks.
Notes:
Calls tear-down-data to clean up data."))

(defmethod stop-sys ((system system) &key &allow-other-keys))

(defmethod tear-down-universe ((universe universe) &key &allow-other-keys))

(defmethod stop-sys :before ((system system) &key &allow-other-keys)
  (tear-down-universe (universe system)))

(defmethod stop-sys :after ((system system) &key &allow-other-keys)
  (setf (system-status system) :started))

;;TODO: How to force lock, how to stop users or more by system lock so that only admin can perform tasks on the system?"
(defgeneric lock (system &key &allow-other-keys)
  (:documentation "Implement system lock tasks."))

(defmethod lock ((system system) &key &allow-other-keys))

(defmethod lock :after ((system system) &key &allow-other-keys)
  (setf (system-status system) :locked))

(defgeneric unlock (system &key &allow-other-keys)
  (:documentation "Implement system unlock tasks."))

(defmethod unlock ((system system) &key &allow-other-keys))

(defmethod unlock :after ((system system) &key &allow-other-keys)
  (setf (system-status system) :started))



