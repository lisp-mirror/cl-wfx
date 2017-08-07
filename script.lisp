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
     :destinations (:core :license)))
   
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
   ))
