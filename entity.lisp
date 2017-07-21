(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "entity"
	   :label "Entity"
	   :top-level-p t
	   :fields ((:name :name
			   :label "Name"
			   :key t
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")		    
		    (:name :entity-type
			   :label "Entity Type"
			   :key nil
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :root-p
			   :label "Root Entity"
			   :key nil
			   :db-type :boolean
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :children
			   :label "Children"
			   :key nil
			   :db-type (:type :list
					   :list-type :item
					   :data-type "entity"
					   :key-accessor :name
					   :collection "entities"
					   )
			   :attributes (:display t :editable t)
			   :documentation "")
		  		    (:name :license-status
			   :label "License Status"
			   :key nil
			   :db-type (:type :list
					   :list-type :keyword
					   :list-values (:demo :suspended :active))
			   :attributes (:display t :editable t)
			   :documentation ""))))
   :destinations (:license)))

(defgeneric match-context-entities (item))

(defun get-license-entities (code)
  (fetch-items (license-collection code "entities")))

(defun get-license-entity (license name)
   (labels ((tail-entity (entities name)
	      (dolist (entity entities)
		(if (string-equal name (getx entity :name))
		  (return-from get-license-entity entity)
		  (tail-entity (getx entity :children) name)))))
     (tail-entity (get-license-entities (getx license :license-code)) name)))

(defun get-entity (name)
  (let ((license-codes (or (and (active-user) (getx (active-user) :selected-licenses))
			   (and (current-user) (getx (current-user) :license-codes)))))    
    (dolist (code license-codes)
      (let ((license (get-license code)))
	(when license
	  (let ((entity (get-license-entity 
			 license
			 name)))
	    (when entity
	      (return-from get-entity entity))))))))

(defun match-entity (item)
  (when (active-user) 
    (find item (getx (active-user) :selected-entities))))

(defmethod match-context-entities ((item item))
  (match-entity (getx item :entity)))
