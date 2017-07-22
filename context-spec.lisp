(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "context-spec"
     :label "Context Spec"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :stores
	     :label "Stores"
	     :key-p nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :collection
	     :label "Collection"
	     :key-p nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key-p nil
	     :db-type (:type :list
			     :list-type :keyword
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :url 
	     :label "Url"
	     :key-p nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :allow-access-to-all-p 
	     :label "Allow Access to all"
	     :key-p nil
	     :db-type :boolean
	     :attributes (:display t :editable t))
      (:name :args 
	     :label "Args"
	     :key-p nil
	     :db-type (:type :list
			     :list-type :keyword
			     :delimiter " ")
	     :attributes (:display t :editable t)))
     :destinations (:core :license)))
   
   (:collection
    (:name "context-specs"
     :label "Context Specs"
     :data-type "context-spec")
    :destinations (:core :system :license))))

(defun get-context-spec (store name)
  (fetch-item (get-collection store "context-specs")
	      :test (lambda (item)
		      (equal name (getx item :name)))))
