(in-package :cl-wfx)

(add-core-definitions
 '((:type-def
    (:name "entity"
	   :label "Entity"
	   
	   :elements ((:name :name
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
					   :type-def "entity"
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
					   :elements (:demo :suspended :active))
			   :attributes (:display t :editable t)
			   :documentation "")))
    :destinations (:license))
   
   (:collection
    (:name "entities"
     :label "Entities"
     :type-def "entity")
    :destinations (:license)
    :access
    (:stores		  
     (:license
      (:user-levels
       (:license (:update :delete :lookup))))))))

(defgeneric match-context-entities (document))

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

(defun match-entity (document)

  (let ((document-type-def (if (stringp (document-type-def document))
			   (get-document-type (document-store document)
					  (document-type-def document))
			   (document-type-def document))))
    (cond ((not (getx (active-user) :selected-entities))
	   (when  (getx (current-user) :super-user-p)
	     document))
	  ((equalp (name document-type-def) "entity")
	   (find (document-hash document)
		 (getx (active-user) :selected-entities)
		 :test #'equalp))
	  ((and (slot-exists-p document-type-def 'entity-accessor)
		(and document-type-def (entity-accessor document-type-def)))
	   (let ((entity (apply 'digx document (entity-accessor document-type-def))))
	     (if entity
		 (find (document-hash entity) 
		       (getx (active-user) :selected-entities)
		       :test #'equalp)
		 (when  (getx (current-user) :super-user-p)
		   document))))
	  ((cl-getx:place-exists-p document :entity)
	   (if (getx document :entity)
	       (find (document-hash (getx document :entity))
		     (getx (active-user) :selected-entities)
		     :test #'equalp)
	       (when  (getx (current-user) :super-user-p)
		 document)))
	  (t
	   document))))

(defmethod match-context-entities ((document document))
  (match-entity document))
