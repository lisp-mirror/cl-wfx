(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "element"
     :label "Elements"

     :elements ((:name :name
		 :label "Name"
		 :key-p t
		 :concrete-type :string
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :key-p
		 :label "Key"
		 :key-p nil
		 :concrete-type :boolean
		 :attributes (:display t :editable t)
		 :documentation "")
		(:name :concrete-type
		 :label "Concrete Type"
		 :key-p nil
		 :concrete-type (:type :list
				 :complex-type :p-list)
		 :attributes (:display t :editable t)
		 :documentation "")

		(:name :attributes
		 :label "Attributes"
		 :key-p nil
		 :concrete-type (:type :list (:name "role-allocation"
					      :label "Role Allocation"
					      :elements ((:name :allocation
							  :label "Allocation"
							  :key-p t
							  :concrete-type (:type :keyword
									  :complex-type :value-list
									  :values-lambda
										(cl-wfx::get-named-list-sorted-values
										 "Allocation Roles")
									  :values ())
							  :attributes (:display t :editable t)
							  :documentation "")
							 (:name :percentage
							  :label "Percentage"
							  :concrete-type :number
							  :attributes (:display t :editable t)
							  :min 0
							  :max 100
							  :documentation ""))

						    ;; :client-validation #'allocation-percentage-check)
					      :list-type :keyword
					      :delimiter " ")
				       :attributes (:display t :editable t)
				       :documentation "")))
     :destinations (:core :system :license))

    (:document-type
     (:name "document-type"
      :label "Data Type"

      :elements ((:name :name
		  :label "Name"
		  :key-p t
		  :concrete-type :string
		  :attributes (:display t :editable t)
		  :documentation "")
		 (:name :label
		  :label "Label"
		  :key-p nil
		  :concrete-type :string
		  :attributes (:display t :editable t)
		  :documentation "")
		 (:name :elements
		  :label "Fields"
		  :key-p nil
		  :concrete-type (:type :list
				  :list-type :document
				  :document-type "element"
				  :key-accessor :name)
		  :attributes (:display t :editable t)
		  :documentation "")))
     :destinations (:core :system :license))

    (:collection
     (:name "modules"
      :label "Modules"
      :document-type "module")
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
       (wfx-query-document
	"data-specs"
	:query (lambda (document)
		 (equalp (name document) spec-name)))
       (get-data-spec spec-name)))
