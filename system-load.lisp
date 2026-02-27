(in-package :cl-wfx)

(defun get-core-context-spec (context-name)
  (query-document
   (core-collection "context-specs")
   :query (lambda (spec)
            (string-equal context-name (name spec)))))

(defun make-context-specs (universe store collection data-type specs)
  (dolist (spec specs)
    (persist-document (core-collection "context-specs")
                      (make-document :universe universe
                                     :store store
                                     :collection collection
                                     :document-type data-type
                                     :elements
                                     spec))))

(defmethod load-context-specs :before ((system system) &key &allow-other-keys)
  (let* ((collection (core-collection "context-specs"))
         (store (store collection))
         (universe (universe store))

         (context-type (get-multiverse-element
                        :document-type store
                        "context-spec"))
         (specs

           '((:name "Login"
              :permissions nil
              :for-everyone t
              :icon "fa-sign-out-alt")
             (:name "Export"
              :permissions nil
              :for-everyone t
              :icon "fa-sign-out-alt")
             (:name "Import"
              :permissions nil
              :for-everyone t
              :icon "fa-sign-in-alt")
             (:name "REPL"
              :permissions nil
              :for-everyone t
              :icon "fa-terminal")
             (:name "Set Password"
              :permissions nil
              :for-everyone t
              :icon "fa-key")
             (:name "Data Types"
              :permissions (:filter :search)
              :collection "document-types"
              :icon "fa-database")
             (:name "Context Specs"
              :permissions (:update :delete)
              :for-everyone t
              :collection "context-specs"
              :icon "fa-window-restore")
             (:name "Modules"
              :permissions (:update :delete)
              :for-everyone t
              :collection "modules"
              :icon "fa-list")
             (:name "Licenses"
              :permissions (:update :delete)
              :for-everyone t
              :collection "licenses"
              :icon "fa-id-badge")
             (:name "Entities"
              :permissions (:update :delete)
              :for-everyone t
              :collection "entities"
              :icon "fa-stream")
             (:name "Users"
              :permissions (:update :delete)
              :for-everyone t
              :collection "users"
              :icon "fa-users")
             (:name "License Users"
              :permissions (:update :delete)
              :for-everyone t
              :collection "license-users"
              :icon "fa-user-friends")
             (:name "Named Lists"
              :permissions (:update :delete)
              :for-everyone t
              :collection "named-lists"
              :icon "fa-clipboard-list")
             (:name "Lambdas"
              :permissions (:update :delete)
              :for-everyone t
              :collection "lambdas"
              :icon "fa-code")
             (:name "Packages"
              :permissions (:update :delete)
              :for-everyone t
              :collection "packages"
              :icon "fa-archive")
             (:name "Java Scripts"
              :permissions (:update :delete)
              :for-everyone t
              :collection "java-scripts"
              :icon "fa-file-code")
             (:name "Stylesheets"
              :permissions (:update :delete)
              :for-everyone t
              :collection "style-sheets"
              :icon "fa-toilet-paper")
             (:name "Email Accounts"
              :permissions (:update :delete)
              :for-everyone t
              :collection "email-accounts"
              :icon "fa-at")
             (:name "Entity Email Accounts"
              :permissions (:update :delete)
              :for-everyone t
              :collection "entity-email-accounts"
              :icon "fa-envelope")
             (:name "Email Templates"
              :permissions (:update :delete)
              :for-everyone t
              :collection "email-templates"
              :icon "fa-edit")
             (:name "Email Logs"
              :permissions (:update :delete)
              :for-everyone t
              :collection "email-Logs"
              :icon "fa-archive")
             (:name "Reports"
              :permissions (:update :delete)
              :for-everyone t
              :collection "reports"
              :icon "fa-file-signature")
             (:name "Entity Reports"
              :permissions (:update :delete)
              :for-everyone t
              :collection "entity-reports"
              :icon "fa-file-invoice-dollar"))))
    (make-context-specs universe store collection context-type specs)))

(defun make-menu-document (name context-spec)
  (make-document
   :document-type "menu-document"
   :elements
   (list :name name
         :context-spec context-spec)))

(defmethod load-modules :around ((system system) &key &allow-other-keys)

  (setup-context-system system)

  (let* ((type-menu (get-multiverse-element
                     :document-type (store (core-collection "modules"))
                     "menu"))
         (menu-type (get-multiverse-element
                     :document-type (store (core-collection "modules"))
                     "menu-document"))
         (parameter-type (get-multiverse-element
                          :document-type (store (core-collection "modules"))
                          "context-parameter"))
         (sys-mod (get-module (core-store) "Core"))
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
                                        :document-type menu-type
                                        :elements (list
                                                   :name (if (document-p spec)
                                                             (digx spec :name)
                                                             :divider)
                                                   :context-spec spec)))))

    (setf menu-documents (append menu-documents
                                 (list
                                  (make-document
                                   :document-type menu-type
                                   :elements
                                   (list
                                    :name "Import"
                                    :context-spec
                                    (get-context-spec (core-store)
                                                      "Import")))
                                  (make-document
                                   :document-type menu-type
                                   :elements
                                   (list
                                    :name "REPL"
                                    :context-spec
                                    (get-context-spec (core-store)
                                                      "REPL")))
                                  (make-document
                                   :document-type menu-type
                                   :elements
                                   (list
                                    :name "Set Password"
                                    :context-spec
                                    (get-context-spec (core-store)
                                                      "Set Password")))
                                  (make-document
                                   :document-type menu-type
                                   :elements
                                   (list
                                    :name "Logout"
                                    :context-spec
                                    (get-context-spec (core-store) "Login")
                                    :context-parameters
                                    (list (make-document
                                           :document-type parameter-type
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
                       :document-type type-menu
                       :elements
                       (list :name "System"
                             :menu-documents  menu-documents)))))))

    (unless sys-mod
      (setf sys-mod
            (make-document :document-type "module"
                           ;;:multiverse (multiverse (universe system))
                           :universe (universe system)
                           :collection (core-collection "modules")
                           :elements
                           (list :name "Core"
                                 :module-short "cor"
                                 :menu nil)))
      (setf (getx sys-mod :contexts)
            (remove-if #'not contexts))

      (setf (getx sys-mod :menu)
            (list (make-document
                   :document-type type-menu
                   :elements
                   (list :name "System"
                         :menu-documents  menu-documents)))))

    (setf sys-mod (persist-document (core-collection "modules") sys-mod))

    (setup-file-upload *system*))
  (call-next-method))

