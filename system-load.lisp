(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (fetch-item (core-collection "context-specs")
	      :test (lambda (spec)
		      (string-equal context-name (name spec)))))



(defmethod load-context-specs :before ((system system) &key &allow-other-keys)

  (unless (get-context-spec (core-store) "Login")
    (persist-item (core-collection "context-specs")
		  '(:name "Login"
		    :permissions nil
		    :for-everyone t)))
  
  (unless (get-context-spec (core-store ) "Data Types")
    (persist-item (core-collection "context-specs")    
		  '(:name "Data Types"
		    :permissions (:filter :search)
		    :collection "data-types")))
  
  (unless (get-context-spec (core-store) "Context Specs")
    (persist-item (core-collection "context-specs")
		  '(:name "Context Specs"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "context-specs")))
  
  (unless (get-context-spec (core-store) "Modules")
	  (persist-item (core-collection "context-specs")
			'(:name "Modules"
			  :permissions (:update :delete)
			  :for-everyone t 
			  :collection "modules")))
  
  (unless (get-context-spec (core-store) "Licenses")
	  (persist-item (core-collection "context-specs")
			'(:name "Licenses"
			  :permissions (:update :delete)
			  :for-everyone t 
			  :collection "licenses")))
  
  (unless (get-context-spec (core-store) "Entities")
    (persist-item (core-collection "context-specs")
		  '(:name "Entities"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "entities")))
  (unless (get-context-spec (core-store) "Users")
    (persist-item (core-collection "context-specs")
		  '(:name "Users"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "users")))
  
  (unless (get-context-spec (core-store) "License Users")
    (persist-item (core-collection "context-specs")
		  '(:name "License Users"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "license-users")))
  
  (unless (get-context-spec (core-store) "Allsorts")
    (persist-item (core-collection "context-specs")
		  '(:name "Allsorts"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "allsorts")))
  
  (unless (get-context-spec (core-store) "Scripts")
    (persist-item (core-collection "context-specs")
		  '(:name "Scripts"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "scripts")))
  
  (unless (get-context-spec (core-store) "Reports")
    (persist-item (core-collection "context-specs")
		  '(:name "Reports"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "reports"))))

(defun make-menu-item (name context-spec)
  (make-item
   :data-type "menu-item"
   :values
   (list :name name
	 :context-spec context-spec)))

(defgeneric setup-context-login (module context-spec system
				 &key &allow-other-keys))

(defmethod load-modules :around ((system system) &key &allow-other-keys)

  (let ((sys-mod (get-module (core-store) "Core")))
    
    (unless sys-mod
      (setf sys-mod	   
	    (list :name "Core"
		  :module-short "cor"
		  :menu nil))
      (setf (getf sys-mod :contexts)
	    (remove-if #'not (list
			      ;;  (get-context-spec "theme")
			      ;;(get-context-spec "Allsorts")
			      ;; (get-context-spec "script")
			      ;; (get-context-spec "repl")
			      ;;(get-context-spec "Data Specs")
			      (get-context-spec (core-store ) "Context Specs")
			      ;;  (get-context-spec "report")
			      ;; (get-context-spec "report-view")
			      (get-context-spec (core-store ) "Modules")
			      (get-context-spec (core-store ) "Licenses")
			      (get-context-spec (core-store ) "Entities")
			      (get-context-spec (core-store ) "Users")
			      (get-context-spec (core-store ) "License Users")
			      (get-context-spec (core-store ) "Scripts")
			      (get-context-spec (core-store ) "Reports")
			      ;; (get-context-spec "import-data")
			      
			      )))
      
      
      (let ((menu-items (loop for spec in (getf sys-mod :contexts)
			   when spec
			   collect (make-item
				    :data-type "context-spec"
				    :values
				    (list
				     :name (digx spec :name)
				     :context-spec
				     spec)))))
	
	(setf menu-items (append menu-items
				 (list (make-item
					:data-type "menu-item"
					:values
					(list
					 :name "Logout"
					 :context-spec 
					 (get-context-spec (core-store ) "Login")
					 :context-parameters 
					 (list (make-item
						:values
						(list
						 :name "action"
						 :value "logout"))))))))
	
	
	
	(setf (getf sys-mod :menu)
	      (list (make-item
		     :data-type "menu-item"
		     :values
		     (list :name "System"
			   :menu-items  menu-items)))))

      (setf sys-mod (persist-item (core-collection "modules") sys-mod)))
    
    (dolist (spec (digx sys-mod :contexts))
      (setup-context sys-mod spec *system*))
 
    (setup-context-login sys-mod
			 (get-context-spec (core-store ) "Login") *system*))
  (call-next-method))




