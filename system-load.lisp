(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (query-document
   (core-collection "context-specs")
   :query (lambda (spec)
	    (string-equal context-name (name spec)))))

(defmethod load-context-specs :before ((system system) &key &allow-other-keys)

  (persist-document (core-collection "context-specs")
		    '(:name "Login"
		      :permissions nil
		      :for-everyone t
		      :icon "fa-sign-out-alt"))

  (persist-document (core-collection "context-specs")
		    '(:name "Export"
		      :permissions nil
		      :for-everyone t
		      :icon "fa-sign-out-alt"))

  (persist-document (core-collection "context-specs")
		    '(:name "Import"
		      :permissions nil
		      :for-everyone t
		      :icon "fa-sign-in-alt"))

  (persist-document (core-collection "context-specs")
		    '(:name "REPL"
		      :permissions nil
		      :for-everyone t
		      :icon "fa-terminal"))

  (persist-document (core-collection "context-specs")
		    '(:name "Set Password"
		      :permissions nil
		      :for-everyone t
		      :icon "fa-key"))

  (persist-document (core-collection "context-specs")
		    '(:name "Data Types"
		      :permissions (:filter :search)
		      :collection "document-types"
		      :icon "fa-database"))

  (persist-document (core-collection "context-specs")
		    '(:name "Context Specs"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "context-specs"
		      :icon "fa-window-restore"))

  (persist-document (core-collection "context-specs")
		    '(:name "Modules"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "modules"
		      :icon "fa-list"))

  (persist-document (core-collection "context-specs")
		    '(:name "Licenses"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "licenses"
		      :icon "fa-id-badge"))

  (persist-document (core-collection "context-specs")
		    '(:name "Entities"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "entities"
		      :icon "fa-stream"))

  (persist-document (core-collection "context-specs")
		    '(:name "Users"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "users"
		      :icon "fa-users"))

  (persist-document (core-collection "context-specs")
		    '(:name "License Users"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "license-users"
		      :icon "fa-user-friends"))

  (persist-document (core-collection "context-specs")
		    '(:name "Named Lists"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "named-lists"
		      :icon "fa-clipboard-list"))

  (persist-document (core-collection "context-specs")
		    '(:name "Lambdas"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "lambdas"
		      :icon "fa-code"))

  (persist-document (core-collection "context-specs")
		    '(:name "Packages"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "packages"
		      :icon "fa-archive"))

  (persist-document (core-collection "context-specs")
		    '(:name "Java Scripts"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "java-scripts"
		      :icon "fa-file-code"))

  (persist-document (core-collection "context-specs")
		    '(:name "Stylesheets"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "style-sheets"
		      :icon "fa-toilet-paper"))

  (persist-document (core-collection "context-specs")
		    '(:name "Email Accounts"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "email-accounts"
		      :icon "fa-at"))

  (persist-document (core-collection "context-specs")
		    '(:name "Entity Email Accounts"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "entity-email-accounts"
		      :icon "fa-envelope"))

  (persist-document (core-collection "context-specs")
		    '(:name "Email Templates"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "email-templates"
		      :icon "fa-edit"))

  (persist-document (core-collection "context-specs")
		    '(:name "Email Logs"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "email-Logs"
		      :icon "fa-archive"))

  (persist-document (core-collection "context-specs")
		    '(:name "Reports"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "reports"
		      :icon "fa-file-signature"))

  (persist-document (core-collection "context-specs")
		    '(:name "Entity Reports"
		      :permissions (:update :delete)
		      :for-everyone t
		      :collection "entity-reports"
		      :icon "fa-file-invoice-dollar")))

(defun make-menu-document (name context-spec)
  (make-document
   :document-type "menu-document"
   :elements
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
		    (get-context-spec (core-store) "Named Lists")
		    (get-context-spec (core-store) "Context Specs")
		    ;;  (get-context-spec "report")
		    ;; (get-context-spec "report-view")
		    (get-context-spec (core-store) "Modules")
		    (get-context-spec (core-store) "Licenses")
		    (get-context-spec (core-store) "Entities")
		    :divider

		    (get-context-spec (core-store) "Users")
		    (get-context-spec (core-store) "User Profiles")
		    (get-context-spec (core-store) "License Users")

		    :divider

		    (get-context-spec (core-store) "Lambdas")
		    (get-context-spec (core-store) "Packages")
		    (get-context-spec (core-store) "Java Scripts")
		    (get-context-spec (core-store) "Stylesheets")

		    :divider

		    (get-context-spec (core-store) "Email Accounts")
		    (get-context-spec (core-store) "Entity Email Accounts")
		    (get-context-spec (core-store) "Email Templates")
		    (get-context-spec (core-store) "Email Logs")
		    :divider
		    (get-context-spec (core-store) "Reports")
		    (get-context-spec (core-store) "Entity Reports")))
	 (menu-documents (loop for spec in contexts
			       when spec
				 collect (make-document
					  :document-type "menu-document"
					  :elements (list
						     :name (if (document-p spec)
							       (digx spec :name)
							       :divider)
						     :context-spec spec)))))

    (setf menu-documents (append menu-documents
				 (list
				  (make-document
				   :document-type "menu-document"
				   :elements
				   (list
				    :name "Import"
				    :context-spec
				    (get-context-spec (core-store)
						      "Import")))
				  (make-document
				   :document-type "menu-document"
				   :elements
				   (list
				    :name "REPL"
				    :context-spec
				    (get-context-spec (core-store)
						      "REPL")))
				  (make-document
				   :document-type "menu-document"
				   :elements
				   (list
				    :name "Set Password"
				    :context-spec
				    (get-context-spec (core-store)
						      "Set Password")))
				  (make-document
				   :document-type "menu-document"
				   :elements
				   (list
				    :name "Logout"
				    :context-spec
				    (get-context-spec (core-store) "Login")
				    :context-parameters
				    (list (make-document
					   :document-type "context-parameter"
					   :elements
					   (list
					    :name "wfxaction"
					    :value "logout"))))))))
    (when sys-mod
      (setf (getx sys-mod :contexts) contexts)
      (let ((menu
	      (query-document
	       (getx sys-mod :menu)
	       :query (lambda (document)
			(equalp (getx document :name) "System")))))

	(when menu
	  (setf (getx menu :menu-documents) menu-documents))

	(unless menu
	  (setf (getx sys-mod :menu)
		(list (make-document
		       :document-type "menu"
		       :elements
		       (list :name "System"
			     :menu-documents  menu-documents)))))))

    (unless sys-mod
      (setf sys-mod
	    (list :name "Core"
		  :module-short "cor"
		  :menu nil))
      (setf (getx sys-mod :contexts)
	    (remove-if #'not contexts))

      (setf (getx sys-mod :menu)
	    (list (make-document
		   :document-type "menu"
		   :elements
		   (list :name "System"
			 :menu-documents  menu-documents)))))

    (setf sys-mod (persist-document (core-collection "modules") sys-mod))

    (setup-file-upload *system*))
  (call-next-method))

