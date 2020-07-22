(in-package :cl-wfx)

(add-core-definitions
 '((:type-def
    (:name "context-parameter"
     :label "Context Parameter"
     
     :elements ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :value
		     :label "Value"
		     :key-p nil
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:type-def
    (:name "menu-document"
     :label "Menu Item"
     
     :elements ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :children
		     :label "Children"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-objects
				     :type-def "menu-document"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-spec
		     :label "Context Spec"
		     :key-p nil
		     :db-type (:type :document
				     :complex-type :collection
				     :type-def "context-spec"
				     :collection "context-specs"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-parameters
		     :label "Context Parameters"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-objects
				     :type-def "context-parameter"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:type-def
    (:name "menu"
     :label "Menu"
     
     :elements ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :menu-documents
		     :label "Menu Items"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-objects
				     :type-def "menu-document"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:type-def
    (:name "module"
     :label "Module"
     
     :elements ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :module-short
		     :label "Module Short"
		     :key-p nil
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :contexts
		     :label "Contexts"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :collection-objects
				     :type-def "context-spec"
				     :collection "context-specs"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :menu
		     :label "Menu"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-objects
				     :type-def "menu"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:collection
    (:name "modules"
     :label "Modules"
     :type-def "module")
    :destinations (:core :system :license)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))))
     (:system
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:lookup))))
     (:license
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))))

(defun get-module (store module-name)   
  (query-document
   (get-collection store "modules" )
   :query (lambda (document)
	    (string-equal module-name (getx document :name)))))

(defun get-module-context (module context-name)  
  (dolist (context (getx module :contexts))
    (if (string-equal (getx context :name) context-name)
	(return-from get-module-context context))))

(defun get-module-short (store module-short)  
  (query-document
   (get-collection store "modules")
   :query (lambda (document)
	    (string-equal module-short (getx document :module-short)))))








