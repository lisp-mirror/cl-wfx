(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "context-script"
     :label "Context Script"
     :top-level-p t
     :fields
     ((:name :script
	     :label "Script"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "script"
			     :collection "scripts"
			     :accessor :name)
	     :attributes (:display t :editable t))

      (:name :events 
	     :label "Events"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :values (:select :save :delete :new
					      :export :search :filter)
			     :delimiter " ")
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:data-type
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
      (:name :report
	     :label "Report"
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
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :scripts
	     :label "Scripts"
	     :db-type (:type :list
			     :complex-type :list-items
			     :data-type "context-script"
			     :accessor (:script :name))			  
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
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))
   
   (:collection
      (:name "context-specs"
	     :label "Context Specs"
	     :data-type "context-spec")
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
	 (:license (:view :copy :lookup))))
       (:license
	(:user-levels
	 (:core (:update :delete :lookup))
	 (:system (:update :delete :lookup))
	 (:license (:update :delete :lookup))))))))

(defun get-context-spec (store name)
  (fetch-item (get-collection store "context-specs")
	      :test (lambda (item)
		      (equalp name (getx item :name)))))

(defun get-context-spec-x (name)
  (wfx-fetch-item "context-specs"
	      :test (lambda (item)
		      (equalp name (getx item :name)))))
