(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "element"
     :label "Fields"

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
		(:name :document-type
		 :label "Type Def"
		 :key-p nil
		 :concrete-type (:type :list
				 :complex-type :p-list)
		 :attributes (:display t :editable t)
		 :documentation "")

		(:name :document-type
		 :label "Attributes"
		 :key-p nil
		 :concrete-type (:type :list
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

(defun get-document-type (store type-name)
  (query-document
   (get-collection store "document-types")
   :query (lambda (document)
	    (equalp (getx document :name) type-name))))
