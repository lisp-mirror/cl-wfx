(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
     :name "context-spec"
     :label "Context Spec"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :stores
	     :label "Stores"
	     :key nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :collection
	     :label "Collection"
	     :key nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key nil
	     :db-type (:type :list
			     :list-type :keyword
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :url 
	     :label "Url"
	     :key nil
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :allow-access-to-all-p 
	     :label "Allow Access to all"
	     :key nil
	     :db-type :boolean
	     :attributes (:display t :editable t))
      (:name :args 
	     :label "Args"
	     :key nil
	     :db-type (:type :list
			     :list-type :keyword
			     :delimiter " ")
	     :attributes (:display t :editable t)))
    :destinations (:core :license))
   
   (:collection
    (:name "context-specs"
     :label "Context Specs"
     :data-type "user")
    :destinations (:core :system :license))))

(defun get-context-spec (collection name)
  (fetch-item collection
	      :test (lambda (item)
		      (equal name (getx item :name)))))
