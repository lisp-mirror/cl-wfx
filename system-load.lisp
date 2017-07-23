(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (fetch-item (core-collection "context-specs")
	      :test (lambda (spec)
		      (string-equal context-name (name spec)))))



(defmethod load-context-specs :before ((system system) &key &allow-other-keys)
	    
  (persist-item (core-collection "context-specs")
		'(:name "Login"
		  :permissions nil
		  :for-everyone t))
  
  (persist-item (core-collection "context-specs")    
		'(:name "Data Types"
		  :permissions (:filter :search)
		  :collection "data-types"))
  
  
  (persist-item (core-collection "context-specs")
		'(:name "Context Specs"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "context-specs"))
  
  (persist-item (core-collection "context-specs")
		'(:name "Modules"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "modules"))
  
  (persist-item (core-collection "context-specs")
		'(:name "Licenses"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "licenses"))
  
  (persist-item (core-collection "context-specs")
		'(:name "Entities"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "entities"))
  
  (persist-item (core-collection "context-specs")
		'(:name "Users"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "users"))
  
  (persist-item (core-collection "context-specs")
		'(:name "License Users"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "license-users"))
  
  (persist-item (core-collection "context-specs")
		'(:name "Allsorts"
		  :permissions (:update :delete)
		  :for-everyone t 
		  :collection "allsorts")))

(defun make-menu-item (name context-spec)
  (list :name name
	:context-spec context-spec))

(defgeneric setup-context-login (module context-spec system &key &allow-other-keys))

(defmethod load-modules :around ((system system) &key &allow-other-keys)

  (let ((sys-mod (get-module (core-store) "Core Admin")))
    
    (unless sys-mod
      (setf sys-mod
	    (list :name "Core"
		  :module-short "cor"
		  :menu (list 
			 ))))
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
			    ;; (get-context-spec "import-data")
			    
			    )))
   
    
    (let ((menu-items (loop for spec in (getf sys-mod :contexts)
			 when spec
			 collect (make-menu-item (getx spec :name)
						 spec))))
 
      (setf menu-items (append menu-items
			       (list (list
				      :item-name "Logout"
				      :context-spec 
				      (get-context-spec (core-store ) "Login")
				      :context-parameters 
				      (list  (list
					      :name "action"
					      :value "logout"))))))
      
      
      
      (setf (getf sys-mod :menu) 
	    (list :menu-name "System"
		  :menu-items  menu-items)))
 
    (let ((mod (persist-item (core-collection "modules") sys-mod)))      
      (dolist (spec (getx mod :contexts))
	(setup-context mod spec *system*))

      (setup-context-login mod (get-context-spec (core-store ) "Login") *system*))))




