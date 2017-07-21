(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (fetch-item (core-collection "context-specs")
	      :test (lambda (spec)
		      (string-equal context-name (name spec)))))

(defun make-menu-item (name context-spec)
  (list :name name
	:context-spec 
	context-spec))

(defgeneric setup-context-login (module context-spec system))

(defmethod load-modules :around ((system system) &key &allow-other-keys)

  (let ((sys-mod (get-module "System Admin")))
    
    (unless sys-mod
      (setf sys-mod
	    (make-instance 
	     'module :module-name "System Admin"
	     :module-short "sys"
	     :menu (list (make-instance 
			  'menu :menu-name "System")))))
    (setf (contexts sys-mod)
	  (remove-if #'not (list
			    ;;  (get-context-spec "theme")
			    ;;(get-context-spec "Allsorts")
			    ;; (get-context-spec "script")
			    ;; (get-context-spec "repl")
			    ;;(get-context-spec "Data Specs")
			    (get-context-spec "Context Specs")
			    ;;  (get-context-spec "report")
			    ;; (get-context-spec "report-view")
			    (get-context-spec "Modules")
			    (get-context-spec "Licenses")
			    (get-context-spec "Entities")
			    (get-context-spec "Users")
			    (get-context-spec "License Users")
			    ;; (get-context-spec "import-data")
			    
			    )))
    
    
    (setup-context-login sys-mod (get-context-spec "Login") *system*)
    
    (dolist (spec (contexts sys-mod))
            ;;TODO: sort out module shit
      (setup-context sys-mod spec *system*))
    
    (let ((menu-items (loop for spec in (contexts sys-mod)
			 when spec
			 collect (make-menu-item (name spec)
						 spec))))
      
      (setf menu-items (append menu-items
			       (list (list
				      :item-name "Logout"
				      :context-spec 
				      (get-context-spec "Login")
				      :context-parameters 
				      (list  (list
					      :name "action"
					      :value "logout"))))))
      
      (setf (getx (first (getx sys-mod :menu)) :menu-items) menu-items))
    
    (persist-item (core-collection "modules") sys-mod)))



(defmethod load-context-specs :before ((system system) &key &allow-other-keys)
	    
  (persist-item (core-collection "context-specs")
	       '(:name "Login"
		     :permissions (:update :delete)
		     :for-everyone t))
  
   
  (persist-item (core-collection "context-specs")    
    '(:name "Data Specs"
     :permissions (:update :delete :search)
     :for-everyone t 
     (:data-spec :name data-spec)))
  
  
  (persist-item (core-collection "context-specs")
    '(:name "Context Specs"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name context-spec)))
  
  (persist-item (core-collection "context-specs")
    '(:name "Modules"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name module)))
  
  (persist-item (core-collection "context-specs")
    '(:name "Licenses"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name license)))
  
  (persist-item (core-collection "context-specs")
    '(:name "Entities"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name entity)))
  
  (persist-item (core-collection "context-specs")
    '(:name "Users"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name user)))
  
  (persist-item (core-collection "context-specs")
    '(:name "License Users"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name license-user)))
  
  (persist-item (core-collection "context-specs")
    '(:name "Allsorts"
     :permissions (:update :delete)
     :for-everyone t 
     (:data-spec :name allsort))))
