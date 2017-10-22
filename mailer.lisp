(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name
     "email-account"
     :label "Email Account"
     :top-level-p t
     :fields
     ((:name :email
	     :label "Email"
	     :key-p t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation	     
	     "")
      (:name :host
	     :label "Host"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :port
	     :label "port"
	     :db-type :integer
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :ssl
	     :label "SSL"
	     :db-type :boolean
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :user-name
	     :label "User Name"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :password
	     :label "password"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")))
    :destinations ( :license))
   
   (:collection
    (:name "email-accounts"
     :label "Email Accounts"
     :data-type "email-account")
    :destinations (:license)
    :access
    (:stores
     (:core
      (:user-levels
      ;; (:core (:update :delete :lookup))
     ;;  (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))

   (:data-type
    (:name
     "entity-email-account"
     :label "Entity Email Account"
     :top-level-p t
     :fields
     ((:name :entity
	     :label "Entity"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "entity"
			     :collection "entities"
			     :accessor (:name))
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :email
	     :label "Email"
	     :key-p t
	     :db-type :email
	     :attributes (:display t :editable t)
	     :documentation	     
	     "")
      (:name :host
	     :label "Host"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :port
	     :label "port"
	     :db-type :integer
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :ssl
	     :label "SSL"
	     :db-type :boolean
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :user-name
	     :label "User Name"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :password
	     :label "password"
	     :db-type :string
	     :attributes (:display t :editable t)
	     :documentation "")))
    :destinations ( :license))

   (:collection
	(:name "entity-email-accounts"
	 :label "Entity Email Accounts"
	 :data-type "entity-email-account"
;;	 :bucket-keys (:entity)
	 )
	:destinations (:license))

   ))

(setf cl-smtp::*debug* t)

(defun send-mail (mail-account to from subject message html-message)
  
  (handler-case
      (progn
	(cl-smtp:send-email (getx mail-account :host)
			    (or from (getx mail-account :email))
			    (list to)
			    subject
			    message
			    :html-message (if (stringp html-message)
					      html-message
					      (eval html-message))
			    :port (getx mail-account :port)
			    :ssl (getx mail-account :ssl)
			    :reply-to (or from (getx mail-account :email))
			    :authentication
			    (list :login (getx mail-account :user-name)
				  (getx mail-account :password))
			    :extra-headers
			    (list (cons "Return-Receipt-To"
					(list (getx mail-account :email)))
				  (cons "Disposition-Notification-To"
					(list (getx mail-account :email)))))
	nil)
    (error (c)      
      (princ-to-string c))))
