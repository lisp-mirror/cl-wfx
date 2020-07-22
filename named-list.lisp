(in-package :cl-wfx)

(add-core-definitions
 '((:type-def
      (:name "key-value"
       :label "Key Value"
       
       :elements ((:name :key
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

   (:type-def
      (:name "list-value"
       :label "List Value"
       
       :elements ( 
		(:name :value
		       :label "Value"
		       :key-p t
		       :db-type :string
		       :attributes (:display t :editable t))
		(:name :attributes
		       :label "Attributes"
		       :db-type (:type :list
				       :complex-type :list-objects 
				       :type-def "key-value"
				       :accessor (:key)
				       :documentation "")		       
		       :attributes (:display t :editable t)))
       :documentation "")
    :destinations (:core :system :license))

   (:type-def
      (:name "named-list"
       :label "named-list"
       
       :elements ((:name :list-name
		       :label "List Name"
		       :db-type :string
		       :key-p t
		       :attributes (:display t :editable t))
		(:name :list-values
		       :label "List Values"
		       :db-type (:type :list
				       :complex-type :list-objects 
				       :type-def "list-value"
				       :accessor (:key)
				       :documentation "")		       
		       :attributes (:display t :editable t)))
       :documentation "")
      :destinations (:core :system :license))
   
   (:collection
    (:name "named-lists"
     :label "Named Lists"
     :type-def "named-list")
    :destinations (:core :system :license)
    :access
    (:stores
     (:core
      (:user-levels
       (:core (:update :delete :lookup))
       (:system (:update :delete :lookup))
       (:license (:update :delete :lookup))))))))


(defun get-named-list (name &key store)
  (wfx-query-document
   (get-collection (or
		     store
		     (system-store))
		    "named-lists")
   :query (lambda (document)
	   (string-equal name (getx document :list-name)))))

(defun get-named-list-values (name &key store)
  (let ((list
	 (get-named-list name :store store)))
    (getx list :list-values)))

(defun sort-order (list-document)
  (dolist (att (getx list-document :attributes))
    (when (equalp (getx att :key) :sort-order)
      (return-from sort-order (getx att :value)))))

(defun named-list-sort-order-function (list-values)
  (sort (copy-list list-values) #'> :key #'sort-order))

(defun get-named-list-sorted (name
			      &key
				(sort-function
				 #'named-list-sort-order-function)
				store)
  (let ((list
	 (get-named-list-values name :store store)))
    (when list
	(funcall sort-function list))))

(defun get-named-list-sorted-values (name
				     &key
				       (sort-function
					#'named-list-sort-order-function)
				       store)
  (let* ((list (get-named-list name :store store))
	 (sorted-list (if list
			  (funcall sort-function (getx list :list-values))))
	 (value-list))
    (dolist (document sorted-list)
      (setf value-list (pushnew (getx document :value) value-list)))
    value-list))
