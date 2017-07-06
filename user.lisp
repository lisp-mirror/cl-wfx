(in-package :cl-wfx)

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec 
   :name user-preference
   :label "User Preference"
   :data-fields
   ((:name name 
	   :initarg :name
	   :accessor name)
    (:name preference 
	   :initarg :preference
	   :accessor preference))
   :metaclass xdb2:storable-versioned-class
   (:documentation
    "User preference, used to store user specific UI and 
system settings.")))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec 
   :name user-profile
   :label "User Profile"
   :data-fields
   ((:name name 
	   :initarg :name
	   :initform nil
	   :accessor name
	   :db-type string
	   :display t
	   :editable t) 
      (:name context-permissions 
	     :initarg :context-permissions
	     :accessor context-permissions
	     :initform nil
	     :db-type (data-group :data-spec user-permission)
	     ))
   :metaclass xdb2:storable-versioned-class
   :collection-name "user-profiles"
   :collection-type :license
   :default-initargs (:top-level t)
   (:documentation
      "Predetermined user settings used to set up users according 
to role or some other criteria."))
  
  )

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  
  (:data-spec 
   :name user-permission
   :label "User Permission"
   :data-fields
   ((:name context-spec 
	   :initarg :context-spec
	   :accessor context-spec
	   :initform nil
	   :db-type (data-member context-spec 
				 :key-accessor context-name)
	   :key t)
      (:name permissions 
	     :initarg :permissions
       :accessor permissions
       :initform nil
       :db-type (list permission)))
   :metaclass xdb2:storable-versioned-class)
  )

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  
  (:data-spec 
   :name user
   :label "User"
   :super-classes (license-doc)
   :data-fields
   ((:name email 
	   :initarg :email
	     :accessor email
	     :key t
	     :db-type email
	     :display t
	     :editable t
	     :documentation	     
	     "User email address used as unique identifier for a user, 
must be valid email to enable confirmation.")
      (:name password 
       :initarg :password
		:accessor password
		:db-type number
		)
      (:name salt 
	     :initarg :salt
	    :accessor salt
	    :db-type string
	    )
      (:name permissions 
	     :initarg :permissions
		   :accessor permissions
		   :initform nil
		   :db-type (list user-permission)
		   :display t
		   :editable t
		   :documentation "Context permissions for the user.")
      (:name accessible-entities 
	     :initarg :accessible-entities
			   :accessor accessible-entities
			   :initform nil
			   :db-type 
			   (data-tree entity 
				      :tree-func #'(lambda (info)
						     (if (license 
							  (value-info-row info))
							 (license-entities 
							  (license 
							   (value-info-row info)))))
				      :selected (or 
						 (current-entities *session*)
						 (current-entities (active-user)))
				      :child-func children
				      :value-func entity-name
				      :check-func check-entity-access))
      (:name preferences 
	     :initarg :preferences
		   :accessor preferences
		   :initform nil
		   :db-type (list user-preference)
		   :display t
		   :editable t
		   ) 
      (:name super-user-p 
	     :initarg :super-user-p
		    :accessor super-user-p
		    :initform nil
		    :documentation		    
		    "If t none of the permission security applies to the user.")
      (:name status 
	     :initarg :status
	      :accessor :status
	      :initform nil
	      :db-type (values :active :suspended :locked :disabled)
	      :documentation "Active, Suspended, Locked Out, Disabled"))
   :metaclass xdb2:storable-versioned-class
   :collection-name "users"
   :collection-type :system
   :default-initargs (:top-level t)
 
   (:documentation
      "User with enough attributes to implement basic login and ui security."))
  )

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec 
   :name user-action
   :label "User Action"
   :data-fields
   ((:name user 
	  :initarg :user
	    :accessor :user
	    :initform nil)
      ;;  (action)
      ;;  (action-reversal)
      ;;  (date-time)
      )
   :metaclass xdb2:storable-versioned-class
   :collection-name "user-actions"
   :collection-type :license
   :default-initargs (:top-level t)
  )
  
  )

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  ;;TODO: Is this not taken care of by sessions? No because it has to be persisted on a user level!
  ;;TODO: How to define actions?
  (:data-spec
   :name active-user
   :label "Active User"
   :data-fields
   ((:name user :initarg :user
	   :accessor user
	   :initform nil)
    (:name action-history 
	   :initarg :action-history
	   :accessor action-history
	   :initform nil
	   :documentation "Actions by user since logon.")
    (:name entities 
	   :initarg :entities
	   :accessor current-entities
	   :initform nil)
    (:name current-action 
	   :initarg :current-action
	   :accessor current-action
	   :initform nil
	   :documentation "What is the user doing now?")
    (:name system-state 
	   :initarg :system-state
	   :accessor system-state
	   :initform nil
	   :documentation "Temporary settings like date selection range?"))
   :metaclass xdb2:storable-versioned-class
   :collection-name "active-users"
   :collection-type :license
   :default-initargs (:top-level t)
   
   (:documentation "Register active users to be able to see what is going on 
and possibly use for logging? Can be used to reapply state when 
a user logs in again.")))

(defvar *password-salt-length* 10)
(defvar *min-passwrod-length* 5)

(defun generate-salt (&key (length *password-salt-length*))
  (map-into (make-array length :element-type '(unsigned-byte 8))
            (lambda () (random 256))))

(defun hash-sequence (sequence &key (digest :sha256))
  (ironclad:digest-sequence digest sequence))

(defun hash-password (password salt)
  (hash-sequence
   (concatenate '(simple-array (unsigned-byte 8) (*))
                (sb-ext:string-to-octets password)
                salt)))

(defun make-password (password)
  (let* ((salt (generate-salt))
         (password (hash-password password salt)))
    (values password salt)))

(defgeneric check-password (user password)
  (:documentation "Check password given against user stored password."))

(defmethod check-password ((user user) password)
  (equalp
   (password user)
   (hash-password password (salt user))))

(defun make-user (email password &key
				   license
				   permissions
				   accessible-entities
				   super-user-p)
  (multiple-value-bind (password salt)
      (make-password password)
    (make-instance 'user 
		   :license license
		   :email email
		   :password password
		   :salt salt
		   :permissions permissions
		   :accessible-entities accessible-entities
		   :super-user-p super-user-p)))

(defun change-user (user new-password &key )
  (when new-password
    (setf (values (password user) (salt user))
          (make-password new-password))))

(defun get-user (email)  
  (fetch-item "users"
	     :test (lambda (doc)
		     (string-equal email (email doc)))))

(defun find-users (criteria)
  (if criteria
      (fetch-items      
      "users"
       :test  criteria)
      (system-data-items 'user)))

(defparameter *user* nil)
(defparameter *license* nil)

(defmacro with-sys-user (system &body body)
  `(let* ((*system* ,system)	 
	  (*user* (get-user "admin@cl-wfx.com"))
	  (*license* (license *user*))
	  (*session* 
	   (make-instance 'session
			  :user (make-instance 
				 'cl-wfx::active-user
				 :user *user*))))
     (when *user*
       ,@body)))

(defmacro with-user (system user &body body)
  `(let* ((*system* ,system)	 
	  (*user* (get-user ,user))
	  (*license* (license *user*))
	  (*session* 
	   (make-instance 'session
			  :user (make-instance 
				 'cl-wfx::active-user
				 :user *user*))))
     (when *user*
       ,@body)))

;;TODO: implement permissions from wfx permissins.lisp

(defgeneric match-entities (user entities))

(defmethod match-entities ((doc user) entities)
  (intersection (accessible-entities doc) entities))

(defun relevant-entities ()
  (or (current-entities *session*)
      (let ((items (fetch-all* (data *system*) 'entitiy)))
	(if (and items (not (listp items)))
	    (coerce items 'list)
	    items))))

#|
(defun setup-permissions (user)
  (let ((hash (make-hash-table :test #'equal)))
    (cond ((super-user-p user)
           (loop with entities = (coerce (entities) 'list)
                 for (page . subs) in (permissions *context*)
                 for perm = (alexandria:ensure-gethash page hash
                                                       (make-permission page
                                                                        (loop for sub in subs
                                                                              collect (cons sub entities))))
                 do
                 (setf (permission-entities perm) entities)))
          ((per-entity-permissions user)
           (loop for (entity . permissions) in (permissions user)
                 do
                 (loop for (page . sub) in permissions
                       for perm = (alexandria:ensure-gethash page hash (make-permission page))
                       do
                       (push entity (permission-entities perm))
                       (loop for sub in sub
                             do
                             (push entity (alexandria:assoc-value (permission-sub-permissions perm)
                                                                  sub :test #'equal))))))
          (t
           (loop for (page . sub) in (permissions user)
                 for perm = (make-permission page (mapcar #'alexandria:ensure-list sub))
                 do (setf (gethash page hash) perm))))
    hash))




(defun update-user-permissions ()
  (let* ((current *current-permissions*)
         (time (current-permissions-time current))
         (user (current-user)))
    (when (or (>= (effective-date user) time)
              (>= (modified (entities-collection)) time))
      (setf (current-permissions-permissions current) (setup-permissions user)
            (current-permissions-time current) (get-universal-time))
      (multiple-value-bind (context found) (session-value 'context)
        (when found
          (setf (session-value 'context)
                (remove-if-not #'get-entity-by-id context)))))))

(defun entity-context ()
  (and (current-user)
       (multiple-value-bind (context found) (session-value 'context)
         (if found
             context
             (setf (context)
                   (remove-if-not #'get-entity-by-id
                                  (last-context (current-user))))))))


;;TODO: Make sure user is set on session when login success

(defgeneric setup-context-permissions (context))

(defmethod setup-context-permissions ((context context))
  (when (and context (session context) (user (session context)))
    (update-user-permissions)
    (let ((perm (gethash (context-name context)
                         (permissions (user (session context))))))
      (cond ((not perm)
             nil)
            ((permission-entities perm)
             (let ((entities (mapcar #'alexandria:ensure-list (permission-entities perm))))
               (loop for (sub . sub-entities) in (sub-permissions perm)
                     do (loop for entity in sub-entities
                              do
                              (push sub (alexandria:assoc-value entities entity))))
               (loop for entry in entities
                     for (entity) = entry
                     when (member (xid entity) (entity-context))
                     collect entry)))
            (t
             (let ((sub (mapcar #'car (sub-permissions perm))))
               (loop for xid in (entity-context)
                     for entity = (get-entity-by-id xid)
                     collect (cons entity sub))))))))


|#
