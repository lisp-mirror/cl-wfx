(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "field"
     :label "Fields"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :key-p
		     :label "Key"
		     :key-p nil
		     :db-type :boolean
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :type-def
		     :label "Type Def"
		     :key-p nil
		     :db-type (:type :list
				     :complex-type :p-list)
		     :attributes (:display t :editable t)
		     :documentation "")
	      
	      (:name :type-def
		     :label "Attributes"
		     :key-p nil
		     :db-type (:type :list
				     :list-type :keyword
				     :delimiter " ")
		     :attributes (:display t :editable t)
		     :documentation "")))
    :destinations (:core :system :license))
   
   (:data-type
    (:name "data-type"
     :label "Data Type"
     :top-level-p nil
     :fields ((:name :name
		     :label "Name"
		     :key-p t
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")		    
	      (:name :label
		     :label "Label"
		     :key-p nil
		     :db-type :string
		     :attributes (:display t :editable t)
		     :documentation "")
	      (:name :fields
		     :label "Fields"
		     :key-p nil
		     :db-type (:type :list
				     :list-type :item
				     :data-type "field"
				     :key-accessor :name)
		     :attributes (:display t :editable t)
		     :documentation "")
	      ))
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
      (:license (:view :copy :lookup))))
    (:license
     (:user-levels
      (:core (:update :delete :lookup))
      (:system (:update :delete :lookup))
      (:license (:update :delete :lookup))))))))


(defun get-data-spec (spec-name)
  (if (current-user)
      (wfx-fetch-item "data-specs"
		  :test (lambda (item)
			  (equalp (name item) spec-name)))
      (get-data-spec spec-name)))
