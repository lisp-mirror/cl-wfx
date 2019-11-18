(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (query-data-object
   (core-collection "context-specs")
   :query (lambda (spec)
	   (string-equal context-name (name spec)))))



(defmethod load-context-specs :before ((system system) &key &allow-other-keys)

  (persist-object (core-collection "context-specs")
		  '(:name "Login"
		    :permissions nil
		    :for-everyone t
		    :icon "fa-sign-out-alt"))

  (persist-object (core-collection "context-specs")
		  '(:name "Export"
		    :permissions nil
		    :for-everyone t
		    :icon "fa-sign-out-alt"))

  (persist-object (core-collection "context-specs")
		  '(:name "Import"
		    :permissions nil
		    :for-everyone t
		    :icon "fa-sign-in-alt" ))
  
  (persist-object (core-collection "context-specs")
		  '(:name "REPL"
		    :permissions nil
		    :for-everyone t
		    :icon "fa-terminal"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Set Password"
		    :permissions nil
		    :for-everyone t
		    :icon "fa-key"))
  
  (persist-object (core-collection "context-specs")    
		  '(:name "Data Types"
		    :permissions (:filter :search)
		    :collection "data-types"
		    :icon "fa-database"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Context Specs"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "context-specs"
		    :icon "fa-window-restore"))
  
  (persist-object (core-collection "context-specs")
			'(:name "Modules"
			  :permissions (:update :delete)
			  :for-everyone t 
			  :collection "modules"
			  :icon "fa-list"))
  
  (persist-object (core-collection "context-specs")
			'(:name "Licenses"
			  :permissions (:update :delete)
			  :for-everyone t 
			  :collection "licenses"
			  :icon "fa-id-badge"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Entities"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "entities"
		    :icon "fa-stream"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Users"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "users"
		    :icon "fa-users"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "License Users"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "license-users"
		    :icon "fa-user-friends"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Named Lists"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "named-lists"
		    :icon "fa-clipboard-list"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Lambdas"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "lambdas"
		    :icon "fa-code"))

  (persist-object (core-collection "context-specs")
		  '(:name "Packages"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "packages"
		    :icon "fa-archive"))

  (persist-object (core-collection "context-specs")
		  '(:name "Java Scripts"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "java-scripts"
		    :icon "fa-file-code"))

  (persist-object (core-collection "context-specs")
		  '(:name "Stylesheets"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "style-sheets"
		    :icon "fa-toilet-paper"))

  (persist-object (core-collection "context-specs")
		  '(:name "Email Accounts"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "email-accounts"
		    :icon "fa-at"))

  (persist-object (core-collection "context-specs")
		  '(:name "Entity Email Accounts"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "entity-email-accounts"
		    :icon "fa-envelope"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Reports"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "reports"
		    :icon "fa-file-signature"))
  
  (persist-object (core-collection "context-specs")
		  '(:name "Entity Reports"
		    :permissions (:update :delete)
		    :for-everyone t 
		    :collection "entity-reports"
		    :icon "fa-file-invoice-dollar")))

(defun make-menu-item (name context-spec)
  (make-item
   :data-type "menu-item"
   :values
   (list :name name
	 :context-spec context-spec)))


(defmethod load-modules :around ((system system) &key &allow-other-keys)


  (setup-context-system system)

  
  (let* ((sys-mod (get-module (core-store) "Core"))
	 (contexts (list
		    ;;  (get-context-spec "theme")
		    ;;(get-context-spec "Allsorts")
			      
		    ;; (get-context-spec "repl")
		    ;;(get-context-spec "Data Specs")
		    (get-context-spec (core-store ) "Context Specs")
		    ;;  (get-context-spec "report")
		    ;; (get-context-spec "report-view")
		    (get-context-spec (core-store ) "Modules")
		    (get-context-spec (core-store ) "Licenses")
		    (get-context-spec (core-store ) "Entities")
		    (get-context-spec (core-store ) "Named Lists")
		    (get-context-spec (core-store ) "Users")
		    (get-context-spec (core-store ) "User Profiles")
		    (get-context-spec (core-store ) "License Users")
		    (get-context-spec (core-store ) "Lambdas")
		    (get-context-spec (core-store ) "Packages")
		    (get-context-spec (core-store ) "Java Scripts")
		    (get-context-spec (core-store ) "Stylesheets")
			     			   
		    (get-context-spec (core-store ) "Email Accounts")
		    (get-context-spec (core-store ) "Entity Email Accounts")
		    (get-context-spec (core-store ) "Reports")
		    (get-context-spec (core-store ) "Entity Reports")
		    ;; (get-context-spec "import-data")
			      
		    ))
	 (menu-items (loop for spec in contexts
			when spec
			collect (make-item
				 :data-type "menu-item"
				 :values
				 (list
				  :name (digx spec :name)
				  :context-spec
				  spec)))))

    (setf menu-items (append menu-items
			     (list
			      (make-item
			       :data-type "menu-item"
			       :values
			       (list
				:name "Import"
				:context-spec 
				(get-context-spec (core-store )
						  "Import")))			      
			      (make-item
			       :data-type "menu-item"
			       :values
			       (list
				:name "REPL"
				:context-spec 
				(get-context-spec (core-store )
						  "REPL")))
			      (make-item
			       :data-type "menu-item"
			       :values
			       (list
				:name "Set Password"
				:context-spec 
				(get-context-spec (core-store )
						  "Set Password")))
			      (make-item
			       :data-type "menu-item"
			       :values
			       (list
				:name "Logout"
				:context-spec 
				(get-context-spec (core-store ) "Login")
				:context-parameters 
				(list (make-item
				       :data-type "context-parameter"
				       :values
				       (list
					:name "wfxaction"
					:value "logout")))
				))
			      )))
    (when sys-mod
      (setf (getx sys-mod :contexts) contexts)
      (let ((menu
	     (query-data-object
	      (getx sys-mod :menu)
	      :query (lambda (item)
		       (equalp (getx item :name) "System")))))

	(setf (getx menu :menu-items) menu-items)))
    
    (unless sys-mod
      (setf sys-mod	   
	    (list :name "Core"
		  :module-short "cor"
		  :menu nil))
      (setf (getf sys-mod :contexts)
	    (remove-if #'not contexts))
      
      (setf (getf sys-mod :menu)
	    (list (make-item
		   :data-type "menu-item"
		   :values
		   (list :name "System"
			 :menu-items  menu-items)))))

    (setf sys-mod (persist-object (core-collection "modules") sys-mod))
    
    (setup-file-upload *system*)
    
    )
  (call-next-method))





