(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
      (:name "key-value"
       :label "Key Value"
       :top-level-p nil
       :fields ((:name :key
		       :label "Key"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t)) 
		(:name :value
		       :label "Value"
		       :key-p t
		       :db-type :string
		       :attributes (:display t :editable t)))
       :documentation "")
    :destinations (:core :system :license))

   (:data-type
      (:name "list-value"
       :label "List Value"
       :top-level-p nil
       :fields ((:name :key
		       :label "Key"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t)) 
		(:name :value
		       :label "Value"
		       :key-p t
		       :db-type :string
		       :attributes (:display t :editable t))
		(:name :value-attributes
		       :label "Value Attributes"
		       :db-type (:type :list
				       :complex-type :list-items 
				       :data-type "key-value"
				       :accessor (:key)
				       :documentation "")		       
		       :attributes (:display t :editable t)))
       :documentation "")
    :destinations (:core :system :license))

   (:data-type
      (:name "named-list"
       :label "named-list"
       :top-level-p nil
       :fields ((:name :list-name
		       :label "List Name"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t))
		(:name :list-values
		       :label "List Values"
		       :db-type (:type :list
				       :complex-type :list-items 
				       :data-type "list-value"
				       :accessor (:key)
				       :documentation "")		       
		       :attributes (:display t :editable t)) 
		(:name :value
		       :label "Value"
		       :key-p t
		       :db-type :string
		       :attributes (:display t :editable t)))
       :documentation "")
      :destinations (:system :license))
   
   (:collection
    (:name "named-lists"
     :label "Named Lists"
     :data-type "license")
    :destinations (:core :system :license)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))))


(defun get-named-list (name)
  (fetch-item
   (core-collection "named-lists")
   :test (lambda (item)
	   (string-equal name (getx item :list-name)))))

(defun get-named-list-values (name)
  (let ((list
	 (get-named-list name)))
    (getx list :list-values)))

(defun sort-order (list-item)
  (dolist (att (getx list-item :value-attributes))
    (when (equalp (getx att :key) :sort-order)
      (return-from sort-order (getx att :value)))))

(defun named-list-sort-order-function (list-values)
  (sort (copy-list list-values) #'> :key #'sort-order))

(defun get-named-list-sorted-values (name
				     &optional
				       (sort-function
					#'named-list-sort-order-function))
  (let ((list
	 (get-named-list name)))
    (apply sort-function (getx list :list-values))))
