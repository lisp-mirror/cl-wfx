(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "context-parameter"
     :label "Context Parameter"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :value
		     :label "Value"
		     :key t
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
		     :key t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :children
		     :label "Children"
		     :key t
		     :db-type (:type :list
				     :list-type :item
				     :data-type "menu-item"
				     :key-accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-spec
		     :label "Context Spec"
		     :key nil
		     :db-type (:type :item
				     :item-type "context-spec"
				     :key-accessor :name
				     :collection "context-specs")
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :context-parameters
		     :label "Context Parameters"
		     :key nil
		     :db-type (:type :list
				     :list-type :item
				     :data-type "context-parameter"
				     :key-accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:data-type
    (:name "menu"
     :label "Menu"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :menu-items
		     :label "Menu Items"
		     :key t
		     :db-type (:type :list
				     :list-type :item
				     :data-type "menu-item"
				     :key-accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:data-type
    (:name "module"
     :label "Module"
     :top-level-p t
     :fields ((:name :name
		     :label "Name"
		     :key t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :module-short
		     :label "Module Short"
		     :key t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :contexts
		     :label "Contexts"
		     :key t
		     :db-type (:type :list
				     :list-type :item
				     :data-type "context-spec"
				     :key-accessor :name
				     :collection "context-specs")
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :menu
		     :label "Menu"
		     :key t
		     :db-type (:type :list
				     :list-type :item
				     :data-type "menu"
				     :key-accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:collection
    (:name "modules"
     :label "Modules"
     :data-type "modules")
    :destinations (:core :system :license))))

(defun get-module (collection module-name)  
  (fetch-item collection
	      :test (lambda (item)
		      (string-equal module-name (getx item :module-name)))))

(defun get-module-context (module context-name)  
  (dolist (context (getx module :contexts))
    (if (string-equal (getx context :name) context-name)
	(return-from get-module-context context))))

(defun get-module-short (collection module-short)  
  (fetch-item collection
	     :test (lambda (item)
		     (string-equal module-short (getx item :module-name)))))

