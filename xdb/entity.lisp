(in-package :cl-wfx)

(defclass entity-doc (doc)
     ((entity :initarg :entity
	       :initform nil
	       :accessor entity
	       :db-type (data-member :data-spec entity :key-accessor name)
	       :key t
	     
	       :printer #'print-entity-name))
     (:metaclass xdb2:storable-mixin))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name entity
   :label "Entity"
   :super-classes (license-doc)
   :data-fields
   ((:name name
	   :initarg :name
	   :accessor name
	   :label "Name"
	   :key t
	   :db-type symbol
	   :display t
	   :editable nil)
    (:name entity-type
	   :initarg :entity-type
	   :accessor entity-type
	   :initform nil
	   :db-type string
	   :display t
	   :editable t)
    (:name root-p
	   :initarg :root-p
	   :accessor root-p
	   :initform nil
	   :db-type boolean
	   :display t
	   :editable t)
    (:name children
	   :initarg :children
	   :accessor children
	   :initform nil
	   :db-type (list entity)
	   :display t
	   :editable nil))
   :metaclass xdb2:storable-versioned-class
   :collection-name "entities"
   :collection-type :license
   :default-initargs (:top-level t)))

(defun print-entity-name (doc)
  (typecase doc
    (entity
     (name doc))
    (entity-doc
     (and (entity doc)
          (name (entity doc))))))

(defgeneric match-context-entities (doc))

(defmethod match-context-entities ((doc t))
  t)



(defun get-license-entity (license name)
   (labels ((tail-entity (entities name)
	      (dolist (entity entities)
		(if (string-equal name (name entity))
		  (return-from get-license-entity entity)
		  (tail-entity (children entity) name)))))
     (tail-entity (license-entities license) name)))

(defun get-entity (name)
  (get-license-entity 
   (license (current-user))
   name))

(defun match-entity (object)
  #|(and (typep object 'entity-doc)
       (assoc (entity object) (request-page-permissions *current-page*))
       t)
  |#
  
  (when (active-user) 
  ;;  (break "~A ~A" object (current-entities (active-user)))
    (find object (current-entities (active-user)))))

(defmethod match-context-entities ((doc entity-doc))
 ;; (break "~A" doc)
  (match-entity (entity doc)))
