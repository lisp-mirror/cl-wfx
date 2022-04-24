(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "user-permission"
     :label "User Permission"

     :elements ((:name :context-spec
		 :label "Context Spec"
		 :concrete-type (:type :document
				 :complex-type :collection
				 :document-type "context-spec"
				 :collection "context-specs"
				 :accessor :name)
		 :key-p t
		 :attributes (:display t :editable t))
		(:name :permissions
		 :label "Permissions"
		 :concrete-type (:type :keyword
				 :complex-type :value-string-list
				 :delimiter " ")
		 :attributes (:display t :editable t))))
    :destinations (:core :license))

   (:document-type
    (:name "document-type-permission"
     :label "Data-Type Permission"

     :elements ((:name :type-name
		 :label "Type Name"
		 :concrete-type :string
		 :key-p t
		 :attributes
		 (:display t :editable t))
		(:name :permissions
		 :label "Permissions"
		 :concrete-type (:type :keyword
				 :complex-type :value-string-list
				 :delimiter " ")
		 :attributes (:display t :editable t))))
    :destinations (:core :license))

   (:document-type
    (:name "user-profile"
     :label "User Profile"

     :elements ((:name :name
		 :label "Name"
		 :concrete-type :string
		 :key-p t
		 :attributes
		 (:display t :editable t))
		(:name :context-permissions
		 :label "Context Permissions"
		 :key nil
		 :concrete-type (:type :list
				 :complex-type :list-objects
				 :document-type "user-permission"
				 :accessor (:context-spec :name))
		 :attributes (:display t :editable t))
		(:name :document-type-permissions
		 :label "Data Type Permissions"
		 :key nil
		 :concrete-type (:type :list
				 :complex-type :list-objects
				 :document-type "document-type-permission"
				 :accessor (:name))
		 :attributes (:display t :editable t)))
     :documentation "Predetermined user settings used to set up users according to role or some other criteria.")
    :destinations (:core :license))

   (:collection
    (:name "user-profiles"
     :label "User Profiles"
     :document-type "user-profile")
    :destinations (:core :license))

   (:document-type
    (:name "user-preference"
     :label "User Preference"

     :elements ((:name :name
		 :label "name"
		 :concrete-type :string
		 :key-p t
		 :attributes (:display t :editable t))
		(:name :preference
		 :label "Preference"
		 :key nil
		 :concrete-type :string
		 :attributes (:display t :editable t)))
     :documentation "")
    :destinations (:core))

   (:document-type
    (:name
     "user"
     :label "User"

     :elements
     (
      (:name :email
       :label "Email"
       :key-p t
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation "User email address used as unique identifier for a user, must be valid email to enable confirmation.")
      (:name :title
       :label "Title"
       :concrete-type (:type :keyword
		       :complex-type :value-list
		       :elements (:mr
				  :mrs
				  :miss
				  :prof
				  :dr))
       :attributes (:display t :editable t)
       :documentation "")
      (:name :name
       :label "Name"
       :key nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :phone-no
       :label "Phone No"
       :key nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :password
       :label "Password"
       :concrete-type :number
       :attributes nil)
      (:name :salt
       :label "Salt"
       :concrete-type :String
       :attributes nil)
      (:name :license-codes
       :label "License Codes"
       :concrete-type (:type :string
		       :complex-type :value-string-list
		       :delimiter ";")
       :attributes (:display t :editable t))
      (:name :permissions
       :label "Permissions"
       :key nil
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "user-permission"
		       :accessor (:context-spec :name))
       :attributes (:display t :editable t))
      (:name :preferences
       :label "Preferences"
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "user-preference"
		       :accessor :name)
       :attributes (:display t :editable t))
      (:name :super-user-p
       :label "Is Super User"
       :concrete-type :boolean
       :attributes (:display t :editable t)
       :documentation "If t none of the permission security applies to the user.")
      (:name :status
       :label "Status"
       :concrete-type (:type :keyword
		       :complex-type :value-list
		       :elements (:active :suspended :locked :disabled))
       :attributes (:display t :editable t)
       :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:core))

   (:collection
    (:name "users"
     :label "Users"
     :document-type "user")
    :destinations (:core)
    :access (:stores
	     (:core
	      (:user-levels
	       (:core (:update :delete :lookup))
	       (:system (:update :delete :lookup))))))

   (:document-type
    (:name
     "license-user"
     :label "License User"

     :elements
     ((:name :email
       :label "Email"
       :key-p t
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation "User email address used as unique identifier for a user, must be valid email to enable confirmation.")
      (:name :permissions
       :label "Permissions"
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "user-permission"
		       :accessor (:context-spec :name))
       :attributes (:display t :editable t))
      (:name :document-type-permissions
       :label "Data Type Permissions"
       :key nil
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "document-type-permission"
		       :accessor (:name))
       :attributes (:display t :editable t))
      (:name :accessible-entities
       :label "Accessible Entities"
       :concrete-type (:type :list
		       :complex-type :collection-objects
		       :document-type"entity"
		       :collection "entities"
		       :accessor :name)
       :attributes (:display t :editable t))
      (:name :status
       :label "Status"
       :concrete-type (:type :keyword
		       :complex-type :value-list
		       :elements (:active :suspended :locked :disabled))
       :attributes (:display t :editable t)
       :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:license))

   (:collection
    (:name "license-users"
     :label "License Users"
     :document-type "license-user")
    :destinations (:license)
    :access
    (:stores
     (:license
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))

   (:document-type
    (:name
     "active-user"
     :label "Active User"

     :elements
     ((:name :email
       :label "Mail"
       :key-p t
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation
       "User email address used as unique identifier for a user,
must be valid email to enable confirmation.")
      (:name :selected-licenses
       :label "Selected Licenses"
       :key nil
       :concrete-type (:type :string
		       :complex-type :value-string-list
		       :delimiter ";"
		       :accessor :code)
       :attributes (:display t :editable t))
      (:name :selected-entities
       :label "Selected Entities"
       :key nil
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "entity"
		       :accessor :name)
       :attributes (:display t :editable t))))
    :destinations (:core))

   (:collection
    (:name "active-users"
     :label "Active Users"
     :document-type "active-user")
    :destinations (:core)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))))))))

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

(defmethod check-password ((user document) password)
  (equalp
   (getx user :password)
   (hash-password password (getx user :salt))))

(defun make-user (email password &key name phone-no license-codes super-user-p)
  (multiple-value-bind (password salt)
      (make-password password)
    (persist-document (core-collection "users")
		      (list :license-codes license-codes
			    :email email
			    :name name
			    :phone-no phone-no
			    :password password
			    :salt salt
			    :super-user-p super-user-p))))

(defun change-user (user new-password &key)
  (when new-password
    (setf (values (getx user :password) (getx user :salt))
          (make-password new-password))))

(defun get-user (email)
  (query-document
   (core-collection "users")
   :query (lambda (document)
	    (string-equal email (getx document :email)))))

(defun get-license-user (license-code email)
  (query-document
   (license-collection license-code "license-users")
   :query (lambda (document)
	    (string-equal email (getx document :email)))))

(defparameter *user* nil)

(defun add-user (email password &key name licenses contexts exclude-document-types
				  entities super-user-p permissions)
  (let ((user (get-user email))
	(context-permissions)
	(document-type-permissions))

    (when user
      (multiple-value-bind (password salt)
	  (make-password password)
	(setf (getx user :password) password)
	(setf (getx user :salt) salt))
      (setf (getx user :name) name)
      (setf (getx user :license-codes) licenses)
      (persist-document (document-collection user) user))

    (unless user
      (setf user (cl-wfx::make-user email password
				    :name name
				    :license-codes licenses
				    :super-user-p super-user-p)))
    (dolist (context contexts)
      (push
       (make-document
	:document-type "user-permission"
	:elements
	(list :context-spec
	      context
	      :permissions (or permissions '(:update :delete :search))))
       context-permissions))

    (dolist (document-type exclude-document-types)
      (push
       (make-document
	:document-type "document-type-permission"
	:elements
	(list :type-name
	      document-type
	      :permissions permissions))
       document-type-permissions))

    (dolist (code licenses)
      (unless (find code (getx user :license-codes) :test #'equalp)
	(setf (getx user :license-codes)
	      (append (getx user :license-codes)
		      (list code))))

      (persist-document
       (document-collection user)
       user)

      (let ((lic-user (get-license-user code email)))

	(when lic-user
	  (setf (getx lic-user :permissions) context-permissions)
	  (setf (getx lic-user :accessible-entities) entities)
	  (setf (getx lic-user :document-type-permissions) document-type-permissions)

	  (persist-document (license-collection code "license-users") lic-user))

	(unless lic-user
	  (persist-document
	   (license-collection code "license-users")
	   (list
	    :email email
	    :permissions context-permissions
	    :document-type-permissions document-type-permissions
	    :accessible-entities entities
	    :status :active)))))

    user))

(defgeneric ensure-user (system email password &key &allow-other-keys))

(defmethod ensure-user ((system system) email password
			&key name licenses super-user-p &allow-other-keys)
  (or (get-user email)
      (make-user email password
		 :name name
		 :license-codes licenses
		 :super-user-p super-user-p)))

(defmethod ensure-core-user ((system system) &key &allow-other-keys)
  (ensure-user system
	       "admin@cl-wfx.com"
	       "admin"
	       :name "Core Admin"
	       :super-user-p t))

(defmethod ensure-system-user ((system system) &key &allow-other-keys)
  (ensure-user system
	       (frmt "admin@~A.com" (name system))
	       "admin"
	       :name "System Admin"
	       :licenses (or (get-license-codes) (list "000000"))
	       :super-user-p t))

(defmacro with-core-user (system &body body)
  `(let* ((*system* ,system)
	  (*user* (get-user "admin@cl-wfx.com"))
	  (*session*
	    (make-instance 'session
			   :user (make-document
				  :document-type "user"
				  :elements (list :email "admin@cl-wfx.com"
						  :selected-licenses nil
						  :selected-entities nil)))))
     (when *user*
       ,@body)))

(defmacro with-user (system user &body body)
  `(let* ((*system* ,system)
	  (*user* (get-user ,user))
	  (*session*
	    (make-instance 'session
			   :user (make-document
				  :document-type "user"
				  :elements (list :email ,user
						  :selected-licenses nil
						  :selected-entities nil)))))
     (when *user*
       ,@body)))

;;TODO: implement permissions from wfx permissions.lisp

(defgeneric match-entities (user license-code entities))

(defmethod match-entities ((user document) license-code entities)
  (intersection (available-entities license-code) entities))

(defun user-context-permission-p (context permission)
  (let ((permission-p)
	(lic-user (get-license-user
		   (first (getx (active-user) :selected-licenses))
		   (getx (active-user) :email))))

    (if (getx (current-user) :super-user-p)
	(setf permission-p t)

	(dolist (permissionx (getx lic-user :permissions))

	  (when (and (getx permissionx :context-spec)
		     (equalp (getx (getx permissionx :context-spec) :name)
			     context))

	    (unless permission-p
	      (setf permission-p
		    (find permission
			  (getx permissionx :permissions)
			  :test #'equalp))))))
    permission-p))

(defun validate-user (email password)
  (let ((user (get-user email)))
    (unless (and user (check-password user password))
      (setf user nil))
    user))
