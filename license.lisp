(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "license"
     :label "License"
     :elements ((:name :license-code
		 :label "License Code"
		 :key-p t
		 :concrete-type :string
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :license-holder
		 :label "License Holder"
		 :key-p nil
		 :concrete-type :string
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :modules
		 :label "License Modules"
		 :key-p nil
		 :concrete-type (:type :list
				 :complex-type :collection-objects
				 :document-type "module"
				 :collection "Modules"
				 :accessor :name)
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :license-date
		 :label "License Date"
		 :key-p nil
		 :concrete-type :date
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :license-status
		 :label "License Status"
		 :key-p nil
		 :concrete-type (:type :keyword
				 :complex-type :value-list
				 :elements (:demo :suspended :active))
		 :attributes (:display t :editable t)
		 :documentation "")))
    :destinations (:core))

   (:collection
    (:name "licenses"
     :label "Licenses"
     :document-type "license")
    :destinations (:core)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:lookup))))))))

(defun get-license (code)
  (query-document
   (core-collection "licenses")
   :query (lambda (document)
	    (string-equal (getx document :license-code) code))))

(defvar *license-code-lock* (bordeaux-threads:make-lock))

(defun generate-license-code ()
  (declare (optimize speed))
  (format nil "~:@(~36r~)" (random (expt 2 32))))

(defun generate-new-license-code ()
  (loop for id = (generate-license-code)
	unless (get-license id)
	  return id))

(defgeneric assign-license-code (license)
  (:documentation "Assigns a code to the license. Uses a thread lock in :around to ensure unique code."))

(defmethod assign-license-code :around ((license document)))

(defmethod assign-license-code ((license document))
  (let ((code))
    (setf (getx license :license-code) code)))

(defun make-license (system license-holder &optional code status date)
  (bordeaux-threads:with-lock-held (*license-code-lock*)
    (let ((license (get-license (if  code
				     code
				     (generate-new-license-code)))))

      (when license
	(setf (getx license :license-holder) license-holder)
	(when status
	  (setf (getx license :license-status) status))
	(when date
	  (setf (getx license :license-date) date)))

      (unless license
	(setf license (make-document
		       :document-type "license"
		       :elements
		       (list :license-code (if code
					       code
					       (generate-new-license-code))
			     :license-holder license-holder
			     :license-date (format-date (get-universal-time))
			     :license-status :active)))

	(persist-document (core-collection "licenses") license)

	(init-license-universe system
			       (getx license :license-code))))))

(defgeneric make-license-package (system-name license-code))

;;TODO: is :use still needed???
(defmethod make-license-package (system-name license-code)
  (eval
   `(defpackage ,(intern (string-upcase (frmt "~A~A" system-name license-code)))
      (:use :cl-wfx-code)
      ;;  (:shadow :start-session :parameter :request :session)
      (:export))))

(defmethod ensure-demo-license ((system system) &key &allow-other-keys)
  (make-license system "Demo" "000000"))

(defun available-entities (license-code)
  (if (getx (current-user) :super-user-p)
      (get-license-entities license-code)
      (if (license-user license-code)
	  (digx (license-user license-code)
		:accessible-entities))))

(defun get-license-codes ()
  (let ((licenses
	  (query-data (core-collection "licenses")))
	(codes))
    (dolist (lic licenses)
      (setf codes (push (getx lic :license-code)
			codes)))
    codes))

(defun available-licenses (&optional user)
  (if (getx (or user (current-user)) :super-user-p)
      (get-license-codes)
      (if (or user (current-user))
	  (getx (or user (current-user)) :license-codes))))
