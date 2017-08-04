(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "context-parameter"
     :label "Context Parameter"
     :top-level-p nil
     :fields ((:name :name
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
   
   (:data-type
    (:name "menu-item"
     :label "Menu Item"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :children
		     :label "Children"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-items
				     :data-type "menu-item"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-spec
		     :label "Context Spec"
		     :key-p nil
		     :db-type (:type :item
				     :complex-type :collection
				     :data-type "context-spec"
				     :collection "context-specs"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-parameters
		     :label "Context Parameters"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-items
				     :data-type "context-parameter"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:data-type
    (:name "menu"
     :label "Menu"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :menu-items
		     :label "Menu Items"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-items
				     :data-type "menu-item"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:data-type
    (:name "module"
     :label "Module"
     :top-level-p t
     :fields ((:name :name
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
				     :complex-type :collection-items
				     :data-type "context-spec"
				     :collection "context-specs"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :menu
		     :label "Menu"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :list-items
				     :data-type "menu"
				     :accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:collection
    (:name "modules"
     :label "Modules"
     :data-type "module")
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
  (fetch-item (get-collection store "modules" )
	      :test (lambda (item)
		      (string-equal module-name (getx item :name)))))

(defun get-module-context (module context-name)  
  (dolist (context (getx module :contexts))
    (if (string-equal (getx context :name) context-name)
	(return-from get-module-context context))))

(defun get-module-short (store module-short)  
  (fetch-item (get-collection store "modules")
	     :test (lambda (item)
		    
		     (string-equal module-short (getx item :module-short)))))








