(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name
     "mail-account"
     :label "Mail Account"
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
      (:name :host
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
	     :db-type :password
	     :attributes (:display t :editable t)
	     :documentation "")))
    :destinations ( :license))
   
   (:collection
    (:name "mail-accounts"
     :label "Mail Accounts"
     :data-type "mail-account")
    :destinations (:core)
    :access
    (:stores
     (:core
      (:user-levels
      ;; (:core (:update :delete :lookup))
     ;;  (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))))

(setf cl-smtp::*debug* t)
(defun send-mail (mail-account to from subject message html-message)

  
  (handler-case
      (progn
	(cl-smtp:send-email (getx mail-account :host)
			    (or from (getx mail-account :email))
			    (list to)
			    subject
			    message
			    :html-message html-message
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
