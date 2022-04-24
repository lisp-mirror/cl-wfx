(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "context-lambda"
     :label "Context Lambda"

     :elements
     ((:name :lambda
       :label "Lambda"
       :key-p t
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "lambda"
		       :collection "lambdas"
		       :accessor :name)
       :attributes (:display t :editable t))

      (:name :events
       :label "Events"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :elements (:select :save
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

   (:document-type
    (:name "context-js"
     :label "Context js"

     :elements
     ((:name :java-script
       :label "Java Script"
       :key-p t
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "java-script"
		       :collection "java-scripts"
		       :accessor :name)
       :attributes (:display t :editable t))
      (:name :event
       :label "Event"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :elements (:select :save
				  :delete
					  :new
				  :export
					  :search
				  :filter)
		       :delimiter " ")
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:document-type
    (:name "context-spec"
     :label "Context Spec"

     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :stores
       :label "Stores"
       :key-p nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :report
       :label "Report"
       :key-p nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :collection
       :label "Collection"
       :key-p nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :permissions
       :label "Permissions"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t))
      (:name :lambdas
       :label "Lambdas"
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "context-lambda"
		       :accessor (:lambda :name))
       :attributes (:display t :editable t))
      (:name :lambdas
       :label "Lambdas"
       :concrete-type (:type :list
		       :complex-type :list-objects
		       :document-type "context-lambda"
		       :accessor (:java-script :name))
       :attributes (:display t :editable t))
      (:name :package
       :label "Package"
       :key-p t
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "package"
		       :collection "packages"
		       :accessor :name)
       :attributes (:display t :editable t))
      (:name :url
       :label "Url"
       :key-p nil
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :allow-access-to-all-p
       :label "Allow Access to all"
       :key-p nil
       :concrete-type :boolean
       :attributes (:display t :editable t))
      (:name :args
       :label "Args"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:collection
    (:name "context-specs"
     :label "Context Specs"
     :document-type "context-spec")
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
  (query-document
   (get-collection store "context-specs")
   :query (lambda (document)
	    (equalp name (getx document :name)))))

(defun get-context-spec-x (name)
  (wfx-query-document
   "context-specs"
   :query (lambda (document)
	    (equalp name (getx document :name)))))
