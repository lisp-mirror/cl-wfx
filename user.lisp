(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "user-permission"
     :label "User Permission"
     :top-level-p nil
     :fields ((:name :context-spec
		     :label "Context Spec"
		     :db-type (:type :item
				     :complex-type :collection
				     :data-type "context-spec"
				     :collection "context-specs"
				     :accessor :name)
		     :key-p t
		     :attributes (:display t :editable t)) 
	      (:name :permissions 
		     :label "Permissions"
		     :db-type (:type :keyword
				     :complex-type :value-string-list
				     :delimiter " ")
		     :attributes (:display t :editable t))))
    :destinations (:core))
   
   (:data-type
    (:name "user-profile"
     :label "User Profile"
     :top-level-p t
     :fields ((:name :name
		     :label "Name"
		     :db-type :string
		     :key-p t
		     :attributes 
		     (:display t :editable t)) 
	      (:name :context-permissions 
		     :label "Context Permissions"
		     :key nil
		     :db-type (:type :list
				     :complex-type :list-items
				     :data-type "user-permission"
				     :accessor (:context-spec :name))
		     :attributes (:display t :editable t)))
     :documentation "Predetermined user settings used to set up users according to role or some other criteria.")
    :destinations (:core :license))
 
   (:collection
    (:name "user-profiles"
     :label "User Profiles"
     :data-type "user-profile")
    :destinations (:core :license))
   
   (:data-type
    (:name "user-preference"
     :label "User Preference"
      :top-level-p nil
     :fields ((:name :name 
		     :label "name"
		     :db-type :string
		     :key-p t
		     :attributes (:display t :editable t)) 
	      (:name :preference
		     :label "Preference"
		     :key nil
		     :db-type :string
		     :attributes (:display t :editable t)))
     :documentation "")
    :destinations (:core))
   
   (:data-type
    (:name
     "user"
     :label "User"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Email"
	     :key-p t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation "User email address used as unique identifier for a user, must be valid email to enable confirmation.")
      (:name :name
	     :label "Name"
	     :key nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :phone-no
	     :label "Phone No"
	     :key nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :password 
	     :label "Password"
	     :db-type :number
	     :attributes nil)
      (:name :salt 
	     :label "Salt"
	     :db-type :String
	     :attributes nil)
      (:name :license-codes 
	     :label "License Codes"
	     :db-type (:type :string
			     :complex-type :value-string-list
			     :delimiter ";" )
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key nil
	     :db-type (:type :list
			     :complex-type :list-items
			     :data-type "user-permission"
			     :accessor (:context-spec :name))
	     :attributes (:display t :editable t))     
      (:name :preferences
	     :label "Preferences"
	     :db-type (:type :list
			     :complex-type :list-items
			     :data-type "user-preference"
			     :accessor :name)			  
	     :attributes (:display t :editable t))      
      (:name :super-user-p
	     :label "Is Super User"
	     :db-type :boolean
	     :attributes (:display t :editable t)
	     :documentation "If t none of the permission security applies to the user.")
      (:name :status
	     :label "Status"
	     :db-type (:type :keyword
			     :complex-type :value-list
			     :values (:active :suspended :locked :disabled))
	     :attributes (:display t :editable t)
	     :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:core))
   
   (:collection
    (:name "users"
     :label "Users"
     :data-type "user")
    :destinations (:core)
    :access (:stores
	     (:core
	      (:user-levels
	       (:core (:update :delete :lookup))
	       (:system (:update :delete :lookup))))))
   
   (:data-type
    (:name
     "license-user"
     :label "License User"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Email"
	     :key-p t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation "User email address used as unique identifier for a user, must be valid email to enable confirmation.")
      (:name :permissions 
	     :label "Permissions"
	     :db-type (:type :list
			     :complex-type :list-items
			     :data-type "user-permission"
			     :accessor (:context-spec :name))
	     :attributes (:display t :editable t))     
      (:name :accessible-entities 
	     :label "Accessible Entities"
	     :db-type (:type :list
			     :complex-type :collection-items
			     :data-type"entity"
			     :collection "entities"
			     :accessor :name)
	     :attributes (:display t :editable t))
      (:name :status
	     :label "Status"
	     :db-type (:type :keyword
			     :complex-type :value-list
			     :values (:active :suspended :locked :disabled))
	     :attributes (:display t :editable t)
	     :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:core :license))
 
   (:collection
    (:name "license-users"
     :label "License Users"
     :data-type "license-user")
    :destinations (:license)
    :access
    (:stores		  
      (:license
       (:user-levels
	(:core (:update :delete :lookup))
	(:system (:update :delete :lookup))
	(:license (:update :delete :lookup))))))
   
   (:data-type
    (:name
     "active-user"
     :label "Active User"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Mail"
	     :key-p t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation	     
	     "User email address used as unique identifier for a user, 
must be valid email to enable confirmation.")      
      (:name :selected-licenses 
	     :label "Selected Licenses"
	     :key nil
	     :db-type (:type :string
			     :complex-type :value-string-list
			     :delimiter ";"
			     :accessor :code)
	     :attributes (:display t :editable t))      
      (:name :selected-entities 
	     :label "Selected Entities"
	     :key nil
	     :db-type (:type :list
			     :complex-type :list-items
			     :data-type "entity"
			     :accessor :name)
	     :attributes (:display t :editable t))))
    :destinations (:core))
   
   (:collection
    (:name "active-users"
     :label "Active Users"
     :data-type "active-user")
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

(defmethod check-password ((user item) password)
  (equalp
   (getx user :password)
   (hash-password password (getx user :salt))))

(defun make-user (email password &key name phone-no license-codes super-user-p)
  (multiple-value-bind (password salt)
      (make-password password)
    (persist-item (core-collection "users") 
		  (list :license-codes license-codes
			:email email
			:name name
			:phone-no phone-no
			:password password
			:salt salt
			:super-user-p super-user-p))))

(defun change-user (user new-password &key )
  (when new-password
    (setf (values (getx user :password) (getx user :salt))
          (make-password new-password))))

(defun get-user (email) 
  (fetch-item
   (core-collection "users")
   :test (lambda (item)
	   (string-equal email (getx item :email)))))

(defun get-license-user (license-code email) 
  (fetch-item
   (license-collection license-code "license-users")
   :test (lambda (item)
	   (string-equal email (getx item :email)))))


(defparameter *user* nil)

(defun add-user (email password &key name licenses contexts
				  entities super-user-p permissions)
  (let ((user (get-user email))
	(permissionsx))
    
    (when user
      (multiple-value-bind (password salt)
	  (make-password password)
	(setf (getx user :password) password)
	(setf (getx user :salt) salt))
      (setf (getx user :name) name)
      (setf (getx user :license-codes) licenses)
      (persist user))
    
    (unless user
      (setf user (cl-wfx::make-user email password
				    :name name
				    :license-codes licenses
				    :super-user-p super-user-p)))
    
    (dolist (context contexts)	    
	    (push
	     (make-item
	      :data-type "user-permission"
	      :values
	      (list :context-spec
		    context
		    :permissions (or permissions '(:update :delete :search))))
	     permissionsx))
    
    (dolist (code licenses)
      (unless (find code (getx user :license-codes) :test #'equalp)
	(setf (getx user :license-codes)
	      (append (getx user :license-codes)
		      (list code)))
	(persist-item
	   (item-collection user)
	   user))

      (let ((lic-user (get-license-user code email)))

	(when lic-user
	  (setf (getx lic-user :permissions) permissionsx)
	  (setf (getx lic-user :accessible-entities) entities)
	  (persist-item (license-collection code "license-users") lic-user))
	
	(unless lic-user	  
	  (persist-item
	   (license-collection code "license-users")
	   (list
	    :email email
	    :permissions permissionsx
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
	       :super-user t))

(defmethod ensure-system-user ((system system) &key &allow-other-keys)
  (ensure-user system 
	       (frmt "admin@~A.com" (name system)) 
	       "admin"
	       :name "System Admin"
	       :licenses (list "000000")
	       :super-user t))

(defmacro with-core-user (system &body body)
  `(let* ((*system* ,system)	 
	  (*user* (get-user "admin@cl-wfx.com"))	
	  (*session* 
	   (make-instance 'session
			  :user (make-item
				 :data-type "user"
				 :values (list :email "admin@cl-wfx.com" 
					       :selected-licenses nil
					       :selected-entities nil)))))
     (when *user*
       ,@body)))


(defmacro with-user (system user &body body)
  `(let* ((*system* ,system)	 
	  (*user* (get-user ,user))	
	  (*session* 
	   (make-instance 'session
			  :user (make-item
				 :data-type "user"
				 :values (list :email ,user 
					       :selected-licenses nil
					       :selected-entities nil)))))
     (when *user*
       ,@body)))

;;TODO: implement permissions from wfx permissions.lisp

(defgeneric match-entities (user entities))

(defmethod match-entities ((user item) entities)
  (intersection (getx user :accessible-entities) entities))



(defun user-context-permission-p (context permission)
  (let ((permission-p)
	(lic-user (get-license-user
		   (first (getx (active-user) :selected-licenses))
		   (getx (active-user) :email))))
   
    (if (getx (current-user) :super-user-p)
	(setf permission-p t)
	
	(dolist (permissionx (getx lic-user :permissions))
	
	  (when (equalp (getx (getx permissionx :context-spec) :name) context)
	  
	    (unless permission-p
	      (setf permission-p
		    (find permission
			  (getx (getx permissionx :context-spec) :permissions)
			  :test #'equalp))))))
    permission-p)
  )
