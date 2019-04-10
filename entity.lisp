(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "entity"
	   :label "Entity"
	   :top-level-p t
	   :fields ((:name :name
			   :label "Name"
			   :key-p t
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")		    
		    (:name :entity-type
			   :label "Entity Type"
			   :key-p nil
			   :db-type :string
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :root-p
			   :label "Root Entity"
			   :key-p nil
			   :db-type :boolean
			   :attributes (:display t :editable t)
			   :documentation "")
		    (:name :children
			   :label "Children"
			   :key-p nil
			   :db-type (:type :list
					   :complex-type :hierarchical
					   :data-type "entity"
					   :collection "entities"
					   :accessor (:name)
					   :child-accessor (:children))
			   :attributes (:display t :editable t)
			   :documentation "")
		  		    (:name :license-status
			   :label "License Status"
			   :key-p nil
			   :db-type (:type :keyword
					   :complex-type :value-list
					   :values (:demo :suspended :active))
			   :attributes (:display t :editable t)
			   :documentation "")))
    :destinations (:license))
   
   (:collection
    (:name "entities"
     :label "Entities"
     :data-type "entity")
    :destinations (:license)
    :access
    (:stores		  
     (:license
      (:user-levels
       (:license (:update :delete :lookup))))))))

(defgeneric match-context-entities (item))

(defun get-license-entities (code)
  (query-data (license-collection code "entities")))

(defun get-license-entity (license name)
   (labels ((tail-entity (entities name)
	      (dolist (entity entities)
		(if (string-equal name (getx entity :name))
		  (return-from get-license-entity entity)
		  (tail-entity (getx entity :children) name)))))
     
     (tail-entity (get-license-entities (getx license :license-code)) name)))


(defun get-entity (name)
  (let ((license-codes (or (and (active-user)
				(getx (active-user)
				      :selected-licenses))
			   (and (current-user)
				(available-licenses)))))    
    (dolist (code license-codes)
      (let ((license (get-license code)))
	(when license
	  (let ((entity (get-license-entity 
			 license
			 name)))
	    (when entity
	      (return-from get-entity entity))))))))

(defun match-entity (item)

  (let ((data-type-def (get-data-type (item-store item)
					(item-data-type item))))
      
    (cond ((not (getx (active-user) :selected-entities))
	   
	   (when  (getx (current-user) :super-user-p)
	     item))
	  ((equalp (item-data-type item) "entity")
	  
	   (find (item-hash item)
		 (getx (active-user) :selected-entities)
		 :test #'equalp))
	  ((and (slot-exists-p data-type-def 'entity-accessor)
		(and data-type-def (entity-accessor data-type-def)))
	   (let ((entity (apply 'digx item (entity-accessor data-type-def))))
	     (if entity
		 (find (item-hash entity) 
		       (getx (active-user) :selected-entities)
		       :test #'equalp)
		 (when  (getx (current-user) :super-user-p)
		   item))))
	  ((exists-p item :entity)

	   (if (getx item :entity)
	       (find (item-hash (getx item :entity))
		     (getx (active-user) :selected-entities)
		     :test #'equalp)
	       (when  (getx (current-user) :super-user-p)
		 item)))
	  (t
	   item))))

(defmethod match-context-entities ((item item))
  (match-entity item))
