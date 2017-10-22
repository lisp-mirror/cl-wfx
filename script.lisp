(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "script"
     :label "Script"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :script
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
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
      (:name "scripts"
	     :label "Scripts"
	     :data-type "script")
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
	 (:license (:update :delete :lookup))))))

   (:data-type
    (:name "entity-script"
     :label "Entity Script"
     :top-level-p t
     :fields
     ((:name :entity
	     :label "Entity"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "entity"
			     :collection "entities"
			     :accessor (:name))
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :script
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
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
	(:name "entity-scripts"
	 :label "Entity Scripts"
	 :data-type "entity-script"
;;	 :bucket-keys (:entity)
	 )
	:destinations (:license))
   ))
