(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "user-permission"
     :label "User Permission"
     :top-level-p nil
     :fields ((:name :context-spec
		     :label "Context Spec"
		     :db-type (:type :item
				     :data-type "context-spec"
				     :collection "context-specs"
				     :key-accessor :name)
		     :key t
		     :attributes (:display t :editable t)) 
	      (:name :permissions 
		     :label "Permissions"
		     :db-type (:type :list
				     :list-type :keyword
				     :delimiter " ")
		     :attibutes (:display t :editable t))))
    :destinations (:core))
   
   (:data-type
    (:name "user-profile"
     :label "User Profile"
     :top-level-p t
     :fields ((:name :name
		     :label "Name"
		     :db-type :string
		     :key t
		     :attributes 
		     (:display t :editable t)) 
	      (:name :context-permissions 
		     :label "Context Permissions"
		     :key nil
		     :db-type (:type :list
				     :list-type :item
				     :data-type "user-permission"
				     :key-accessor :context-spec
				     :accessor-accossor :name)
		     :attibutes (:display t :editable t)))
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
		     :key t
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
	     :key t
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
	     :db-type (:type :list 
			     :list-type :string
			     :delimiter ";" )
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key nil
	     :db-type (:type :list
			     :list-type :item
			     :data-type "user-permission"
			     :key-accessor :context-spec
			     :accessor-accossor :name)
	     :attibutes (:display t :editable t))     
      (:name :accessible-entities 
	     :label "Accessible Entities"
	     :key nil
	     :db-type (:type :list
			     :list-type :item 
			     :data-type"entity"
			     :collection "entities"
			     :key-accessor :name)
	     :attibutes (:display t :editable t))
      (:name :preferences
	     :label "Preferences"
	     :db-type (:type :list
			     :list-type :item
			     :data-type "user-preference"
			     :key-accessor :name)			  
	     :attributes (:display t :editable t))      
      (:name :super-user-p
	     :label "Is Super User"
	     :db-type :boolean
	     :attributes nil
	     :documentation "If t none of the permission security applies to the user.")
      (:name :status
	     :label "Status"
	     :db-type (:type :list
			     :list-type :keyword
			     :list-values (:active :suspended :locked :disabled))
	     :attributes (:display t :editable t)
	     :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:core))
   
   (:collection
    (:name "users"
     :label "Users"
     :name-space "core"
     :data-type "user")
    :destinations (:core))
   
   (:data-type
    (:name
     "license-user"
     :label "License User"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Email"
	     :key t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation "User email address used as unique identifier for a user, must be valid email to enable confirmation.")
      (:name :permissions 
	     :label "Permissions"
	     :key nil
	     :db-type (:type :list
			     :list-type :item
			     :data-type "user-permission"
			     :key-accessor :context-spec
			     :accessor-accossor :name)
	     :attibutes (:display t :editable t))     
      (:name :accessible-entities 
	     :label "Accessible Entities"
	     :key nil
	     :db-type (:type :list
			     :list-type :item 
			     :data-type"entity"
			     :collection "entities"
			     :key-accessor :name)
	     :attibutes (:display t :editable t))
      (:name :status
	     :label "Status"
	     :db-type (:type :list
			     :list-type :keyword
			     :list-values (:active :suspended :locked :disabled))
	     :attributes (:display t :editable t)
	     :documentation "Active, Suspended, Locked Out, Disabled"))
     :documentation "User with enough attributes to implement basic login and ui security.")
    :destinations (:core :license))
 
   (:collection
    (:name "license-users"
     :label "License Users"
     :data-type "user")
    :destinations (:license))
   
   (:data-type
    (:name
     "active-user"
     :label "Active User"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Mail"
	     :key t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation	     
	     "User email address used as unique identifier for a user, 
must be valid email to enable confirmation.")      
      (:name :selected-licenses 
	     :label "Selected Licenses"
	     :key nil
	     :db-type (:type :list
			     :list-type "license"
			     :collection "licenses"
			     :key-accessor :code)
	     :attibutes (:display t :editable t))      
      (:name :selected-entities 
	     :label "Selected Entities"
	     :key nil
	     :db-type (:type :list
			     :list-type "entity"
			     :collection "entities"
			     :key-accessor :name)
	     :attibutes (:display t :editable t))))
    :destinations (:core))))

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
    (persist-item (system-collection "users") (list :license-codes license-codes
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


(defparameter *user* nil)

(defgeneric ensure-user (system email password &key &allow-other-keys))

(defmethod ensure-user ((system system) email password 
			&key name super-user-p &allow-other-keys)
  (or (get-user email)
      (make-user email password
		 :name name
		 :super-user-p super-user-p)))

(defmethod ensure-core-user ((system system) &key &allow-other-keys)
  (ensure-user "admin@cl-wfx.com" 
	       :name "Core Admin"
	       :super-user t))

(defmethod ensure-system-user ((system system) &key &allow-other-keys)
  (ensure-user (frmt "admin@~A.com" (name system)) 
	       :name "System Admin"
	       :super-user t))

(defmacro with-core-user (system &body body)
  `(let* ((*system* ,system)	 
	  (*user* (get-user "admin@cl-wfx.com"))	
	  (*session* 
	   (make-instance 'session
			  :user (make-item 
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
				 :values (list :email ,user 
					       :selected-licenses nil
					       :selected-entities nil)))))
     (when *user*
       ,@body)))

;;TODO: implement permissions from wfx permissions.lisp

(defgeneric match-entities (user entities))


(defun accessible-entities* (user)
  (let ((license-user (fetch-item "license-users"
				  :test
				  (lambda (item)
				    (string-equal (getx item :email)
						  (getx user :email))))))
    (getx license-user :accessible-entities)))

(defmethod match-entities ((user item) entities)
  (intersection (accessible-entities* user) entities))
