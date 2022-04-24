(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name
     "email-log"
     :label "Email Log"
     :elements
     ((:name :date-stamp
       :label "Date Stamp"
       :key-p t
       :concrete-type :date-time
       :attributes (:display t :editable t)
       :documentation "")
      (:name :email-account
       :label "Email Account"
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :email-template
       :label "Email Template"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :to
       :label "To"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :status
       :label "Status"
       :key-p t
       :concrete-type (:type :keyword
		       :complex-type :value-list
		       :elements (:sent
				  :error))
       :attributes (:display t :editable t)
       :documentation "")
      (:name :error
       :label "Error"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")))
    :destinations (:system :license))

   (:collection
    (:name "email-logs"
     :label "Email Logs"
     :document-type "email-log")
    :destinations (:system :license)
    :access
    (:stores
     (:core
      (:user-levels
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))

   (:document-type
    (:name
     "email-template"
     :label "Email Template"

     :elements
     ((:name :description
       :label "Description"
       :key-p t
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")
      (:name :email-account
       :label "Email Account"
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "email-account"
		       :collection "email-accounts"
		       :accessor (:email))
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :email-script
       :label "Email Script"
       :concrete-type :lambda
       :attributes (:display t :editable t)
       :documentation "")))
    :destinations (:system :license))

   (:collection
    (:name "email-templates"
     :label "Email Templates"
     :document-type "email-template")
    :destinations (:system :license)
    :access
    (:stores
     (:core
      (:user-levels
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))

   (:document-type
    (:name
     "email-account"
     :label "Email Account"

     :elements
     ((:name :email
       :label "Email"
       :key-p t
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :host
       :label "Host"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")
      (:name :port
       :label "port"
       :concrete-type :integer
       :attributes (:display t :editable t)
       :documentation "")
      (:name :ssl
       :label "SSL"
       :concrete-type :boolean
       :attributes (:display t :editable t)
       :documentation "")
      (:name :user-name
       :label "User Name"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")
      (:name :password
       :label "password"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")))
    :destinations (:system :license))

   (:collection
    (:name "email-accounts"
     :label "Email Accounts"
     :document-type "email-account")
    :destinations (:system :license)
    :access
    (:stores
     (:core
      (:user-levels
       ;; (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))

   (:document-type
    (:name
     "entity-email-account"
     :label "Entity Email Account"

     :elements
     ((:name :entity
       :label "Entity"
       :key-p t
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "entity"
		       :collection "entities"
		       :accessor (:name))
       :attributes (:display t :editable t)
       :documentation "")
      (:name :email
       :label "Email"
       :key-p t
       :concrete-type :email
       :attributes (:display t :editable t)
       :documentation
       "")
      (:name :host
       :label "Host"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")
      (:name :port
       :label "port"
       :concrete-type :integer
       :attributes (:display t :editable t)
       :documentation "")
      (:name :ssl
       :label "SSL"
       :concrete-type :boolean
       :attributes (:display t :editable t)
       :documentation "")
      (:name :user-name
       :label "User Name"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")
      (:name :password
       :label "password"
       :concrete-type :string
       :attributes (:display t :editable t)
       :documentation "")))
    :destinations (:license))

   (:collection
    (:name "entity-email-accounts"
     :label "Entity Email Accounts"
     :document-type "entity-email-account")
    :destinations (:license))))

(setf cl-smtp::*debug* t)

(defvar *mail-data* nil)

(defun get-entity-email-account (email)
  (wfx-query-document
   (wfx-get-collection "entity-email-accounts")
   :query (lambda (document)
	    (string-equal (getx document :email) email))))

(defun get-email-account (email)
  (wfx-query-document
   (wfx-get-collection "email-accounts")
   :query (lambda (document)
	    (string-equal (getx document :email) email))))

(defun send-mail (mail-account to from subject message html-message
		  &key data cc bcc reply-to extra-headers display-name attachments
		    bubble-errors-p)

  (let ((*mail-data* data))
    (handler-case
	(progn
	  (cl-smtp:send-email (getx mail-account :host)
			      (or from (getx mail-account :email))
			      (list to)
			      (ppcre:regex-replace-all
			       #\Newline subject
			       (coerce '(#\Return #\Newline) 'string))
			      (ppcre:regex-replace-all
			       #\Newline (if (stringp message)
					     message
					     (eval% html-message))
			       (coerce '(#\Return #\Newline) 'string))
			      :cc cc
			      :bcc bcc
			      :html-message (ppcre:regex-replace-all
					     #\Newline (if (stringp html-message)
							   html-message
							   (eval% html-message))
					     (coerce '(#\Return #\Newline) 'string))
			      :display-name display-name
			      :attachments attachments
			      :port (getx mail-account :port)
			      :ssl (getx mail-account :ssl)
			      :reply-to (or reply-to from (getx mail-account :email))
			      :authentication
			      (list :login (getx mail-account :user-name)
				    (getx mail-account :password))
			      :extra-headers
			      (if extra-headers
				  extra-headers
				  (list (cons "Return-Receipt-To"
					      (list (getx mail-account :email)))
					(cons "Disposition-Notification-To"
					      (list (getx mail-account :email))))))

	  nil)
      (error (c)
	(break "~A" c)
	(if bubble-errors-p
	    (error c)
	    (princ-to-string c))))))

(defparameter *email-account* nil)
(defparameter *email-template* nil)

(defun get-email-template (description)
  (query-document
   (wfx-get-collection "email-templates")
   :query (lambda (document)
	    (string-equal (getx document :description) description))))

(defun send-template-mail-by-description (description &key data)
  (let ((template (get-email-template description)))
    (send-template-mail template :data data)))

(defun log-email (email-account email-template to status error)
  (let ((collection (wfx-get-collection "email-logs")))
    (persist-document
     collection
     (make-document :collection collection
		    :document-type "email-log"
		    :elements (list :date-stamp (local-time:now)
				    :email-account email-account
				    :email-template email-template
				    :to to
				    :status status
				    :error error))
     :collection (wfx-get-collection "email-logs"))))

(defun send-template-mail (template &key data)
  (let ((*email-account* (getx template :email-account))
	(*email-template* template)
	(*mail-data* data)
	(email))

    (when (and *email-account* (getx template :email-script))
      (setf email (eval% (getx template :email-script)))
      (when email
	(handler-case
	    (progn
	      (send-mail
	       *email-account*
	       (getx email :to)
	       (getx email :from)
	       (getx email :subject)
	       (getx email :message)
	       (getx email :html-message)
	       :cc (getx email :cc)
	       :bcc (getx email :bcc)
	       :reply-to (getx email :reply-to)
	       :display-name (getx email :display-name)
	       :data data)

	      (log-email (getx template :email-account) (getx template :description)
			 (getx email :to) :sent  nil))
	  (error (c)

	    (log-email (getx template :email-account) (getx template :description)
		       (getx email :to) :error  c)))))))
