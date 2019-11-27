(in-package :cl-wfx)

(defclass locale ()
  ((language :initarg :language
	     :accessor language
	     :initform :english-us)
   (currency :initarg :language
	     :accessor currency
	     :initform :$)
   (date-format :initarg date-format
		:accessor date-format
		:initform "dd MMM yyyy"))
  (:documentation "Locale info for a session to use."))

(defclass session ()
  ((session-id :initarg :session-id
	       :accessor session-id
	       :initform (random 10810))
   (system :initarg :system
	   :accessor system
	   :initform nil)
   (locale :initarg :locale
	   :accessor locale
	   :initform nil)
   (user :initarg :user
	 :accessor user
	 :initform nil
	 :documentation "user")
   (active-user :initarg :active-user
		:accessor active-session-user
		:initform nil
	 :documentation "active-user")
   (contexts :initarg :contexts
	     :accessor contexts
	     :initform (make-hash-table :test #'equalp))
   (entities :initarg :entities
	     :accessor current-entities
	     :initform nil)
   (cache :initarg :cache
	     :accessor cache
	     :initform (make-hash-table :test #'equalp)))
  (:documentation "Sessions are used to handle displaying of ui independantly for different users. A session starts life before a user login so user is added to the session afterwards."))


(defgeneric session-parameter (parameter))

(defmethod session-parameter (parameter)
  (if *session*
      (gethash parameter (cache *session*))))

(defmethod (setf session-parameter) (value parameter)
  (if *session*
      (setf (gethash parameter (cache *session*)) value)))

(defgeneric locale-language (locale))

(defmethod locale-language ((locale locale))
  (if locale
      (language locale)
      :english))

(defparameter *language-translations* (make-hash-table :test 'equalp)
  "Hashtable of word phrase translations")

(defclass translation ()
  ((language :initarg :language
	     :accessor language
	     :initform nil)
   (phrase :initarg :phrase
	   :accessor phrase
	   :initform nil)))

(defun tlp (phrase)
  (let* ((locale-language (locale (session *context*)))
	 (translation-table (gethash phrase *language-translations*))
	 (translation (if translation-table (gethash locale-language translation-table))))
    (if translation
	(phrase translation)
	phrase)))


(defgeneric setup-session (system session)
  (:documentation "Override if special setup needed."))

(defmethod setup-session ((system system) session)
  "Override if special setup needed.")

(defgeneric start-session (system &key &allow-other-keys)
  (:documentation "Starts a session for the ui."))

(defmethod start-session ((system system) &key)

  (let ((session (gethash (hunchentoot:session-id hunchentoot:*session*) 
						    (sessions system))))
    (unless session
      (setf session (make-instance 'session :session-id
				   (hunchentoot:session-id hunchentoot:*session*)))
      (setf (gethash (hunchentoot:session-id hunchentoot:*session*)
		     (sessions system)) session))
    (setf (system session) system)
    session))

(defun current-user ()
  (if (and (boundp '*session*) *session*) 
      (user *session*)))

(defun license-user (license-code)
  (if (and (boundp '*session*) *session*)   
      (if *session*
	  (get-license-user license-code 
			    (getx (user *session*) :email)))))

(defun active-user ()
  (if (and (boundp '*session*) *session*)   
      (if *session*
	  (active-session-user *session*))))



