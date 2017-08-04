(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "license"
	   :label "License"
	   :top-level-p t
	   :fields ((:name :license-code
			   :label "License Code"
			   :key-p t
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")		    
		    (:name :license-holder
			   :label "License Holder"
			   :key-p nil
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :modules
			   :label "License Modules"
			   :key-p nil
			   :db-type (:type :list
					   :complex-type :collection-items
					   :data-type "module"
					   :collection "Modules"
					   :accessor :name)
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :license-date
			   :label "License Date"
			   :key-p nil
			   :db-type :date
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :license-status
			   :label "License Status"
			   :key-p nil
			   :db-type (:type :keyword
					   :complex-type :value-list
					   :values (:demo :suspended :active))
			   :attributes (:display t :editable t)
			   :documentation "")))
    :destinations (:core))
   
   (:collection
    (:name "licenses"
     :label "Licenses"
     :data-type "license")
    :destinations (:core)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:lookup))))))))

(defun get-license (code)
  (fetch-item (core-collection "licenses")
	      :test (lambda (item)
		      (string-equal (getx item :license-code) code))))


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
(defmethod assign-license-code :around ((license item))
  )

(defmethod assign-license-code ((license item))
  (let ((code ))
    (setf (getx license :license-code) code)))

(defun make-license (system license-holder &optional code)
  
  (bordeaux-threads:with-lock-held (*license-code-lock*)
   
    (let ((license (get-license (if  code
				     code
				     (generate-new-license-code)))))

      (unless license

	(setf license (make-item
		     :data-type "license"
		     :values
		     (list :license-code (if  code
					      code
					      (generate-new-license-code))
			      :license-holder license-holder
			      :license-date (get-universal-time)
			      :license-status :active)))
	
	(persist-item (core-collection "licenses") license)
	
	(init-license-universe system
			       (getx license :license-code))))))


(defgeneric make-license-package (system-name license-code))

(defmethod make-license-package (system-name license-code)  
  (eval
   `(defpackage ,(intern (string-upcase (frmt "~A~A" system-name license-code)))
	  (:use :cl-wfx-scripts)
	  ;;  (:shadow :start-session :parameter :request :session)
	  (:export))))


(defmethod ensure-demo-license ((system system) &key &allow-other-keys)
  (make-license system "Demo" "000000"))

