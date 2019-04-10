(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "context-lambda"
     :label "Context Lambda"
     :top-level-p t
     :fields
     ((:name :lambda
	     :label "Lambda"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "lambda"
			     :collection "lambdas"
			     :accessor :name)
	     :attributes (:display t :editable t))

      (:name :events
	     :label "Events"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :values (:select :save
					      :delete
					      :new
					      :export
					      :search
					      :filter
					      :select-action-list
					      :select-action)
			     :delimiter " ")
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:data-type
    (:name "context-js"
     :label "Context js"
     :top-level-p t
     :fields
     ((:name :java-script
	     :label "Java Script"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "java-script"
			     :collection "java-scripts"
			     :accessor :name)
	     :attributes (:display t :editable t))
      (:name :event 
	     :label "Event"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :values (:select :save
					      :delete
					      :new
					      :export
					      :search
					      :filter)
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
      (:name :lambdas
	     :label "Lambdas"
	     :db-type (:type :list
			     :complex-type :list-objects
			     :data-type "context-lambda"
			     :accessor (:lambda :name))			  
	     :attributes (:display t :editable t))
      (:name :lambdas
	     :label "Lambdas"
	     :db-type (:type :list
			     :complex-type :list-objects
			     :data-type "context-lambda"
			     :accessor (:java-script :name))			  
	     :attributes (:display t :editable t))
      (:name :package
	     :label "Package"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "package"
			     :collection "packages"
			     :accessor :name)
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
  (query-data-object
   (get-collection store "context-specs")
   :query (lambda (item)
	    (equalp name (getx item :name)))))

(defun get-context-spec-x (name)
  (wfx-query-data-object
   "context-specs"
   :query (lambda (item)
	   (equalp name (getx item :name)))))
