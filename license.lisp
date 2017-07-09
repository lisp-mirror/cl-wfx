(in-package :cl-wfx)

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name license-code
   :label "License Codes"
   :data-fields
   ((:name license-code
	   :initarg :license-code
	   :accessor license-code
	   :initform nil
	   :db-type string
	   :key t
	   :display t
	   :editable t)
    (:name user-names
	   :initarg :user-names
	   :accessor user-names
	   :initform nil
	   :db-type (list :type 'string)
	   :display t
	   :editable t))

   :metaclass xdb2:storable-versioned-class
   :collection-name "license-codes"
   :collection-type :system
   :default-initargs (:top-level t)))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name license
   :label "License"
   :data-fields
   ((:name license-code
	   :initarg :license-code
	   :accessor license-code
	   :initform nil
	   :db-type string
	   :key t
	   :display t
	   :editable t)
    (:name license-holder 
	   :initarg :license-holder
	   :accessor license-holder
	   :initform nil
	   :db-type string
	   :display t
	   :editable t)
    (:name payment-detail 
	   :initarg :payment-details
	   :accessor payment-details
	   :initform nil)
    (:name license-modules 
	   :initarg :license-modules
	   :accessor license-modules
	   :initform nil
	   :db-type (data-group :data-spec module :key-accessor module-name)
	   :display t
	   :editable t)
    (:name license-entities 
	   :initarg :license-entities
	   :accessor license-entities
	   :db-type (data-group :data-spec entity :key-accessor name)
	   :initform nil
	   :display t
	   :editable t)
    (:name license-date 
	   :initarg :license-date
	   :accessor license-date
	   :initform (get-universal-time)
	   :db-type date
	   :header t)
    (:name license-status 
	   :initarg :license-status
	   :accessor license-status
	   :initform :demo 
	   :documentation "(:demo :suspended :active)"
	   :db-type string
	   :display t
	   :editable t)
    (:name super-user-access-p 
     :initarg :super-user-access-p
     :accessor super-user-access-p
     :initform t
     :db-type boolean))

   :metaclass xdb2:storable-versioned-class
   :collection-name "licenses"
   :collection-type :merge
   :default-initargs (:top-level t)
 )
  
  )


(defun print-license-code (doc)
  (license-code doc))


(defun find-license (code)
  (fetch-item "licenses"
	      :test (lambda (doc)
		      (string-equal (license-code doc) code))))

(defun find-system-license* ()    
  (map
   nil
   (lambda (lic)
     (when (string-equal *sys-license-code* (license-code lic))
       (return-from find-system-license* lic)))
   (data-items *sys-license-code*
	       "licenses")))


(defun system-license ()
  (let ((license (find-system-license*)))
      (unless license
	(setf license (persist-data (make-instance 'license 
						   :license-code *sys-license-code*
						   :license-holder "System Admin")
				    :collection-name "licenses"
				    :license-code *sys-license-code*))
	(persist-data (make-user "admin@cl-wfx.com" "admin"
				 :super-user-p t
				 :license-codes (list (license-code license)))
		       :collection-name "users"
		       :license-code *sys-license-code*
		      ))
    license))



(defvar *license-code-lock* (bordeaux-threads:make-lock))

(defun generate-license-code ()
  (declare (optimize speed))
  (format nil "~:@(~36r~)" (random (expt 2 32))))

(defun generate-new-license-code ()
  (loop for id = (generate-license-code)
	 unless (find-license id)
	 return id))

(defgeneric assign-license-code (license)
  (:documentation "Assigns a code to the license. Uses a thread lock in :around to ensure unique code."))

(defmethod assign-license-code :around ((license license))
  (bordeaux-threads:with-lock-held (*license-code-lock*)
      (call-next-method)))

(defmethod assign-license-code ((license license))
  (let ((code (generate-new-license-code)))
    (setf (license-code license) code)))







