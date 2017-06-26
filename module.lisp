(in-package :cl-wfx)

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name context-parameter
   :label "Context Parameter"
   :super-classes (doc)
   :data-fields
   ((:name parameter-name 
	   :initarg :parameter-name
	   :accessor parameter-name
	   :initform nil
	   :db-type string
	   :key t)
    (:name parameter-value 
	   :initarg :parameter-value
	   :accessor parameter-value
	   :initform nil
	   :db-type string))
   :metaclass xdb2:storable-versioned-class))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name menu-item
   :label "Menu Item"
   :super-classes (doc)
   :data-fields
   ((:name item-name
	   :initarg :item-name
	   :accessor item-name
	   :initform nil
	   :db-type string
	   :key t)
    (:name children 
	   :initarg :children
	   :initform ()
	   :accessor children
	   :db-type (list menu-item))
    (:name context-spec 
	   :initarg :context-spec
	   :accessor context-spec
	   :initform nil
	   :db-type (data-member context-spec :key-accessor context-name))
    (:name context-parameters 
	   :initarg :context-parameters
	   :accessor context-parameters
	   :initform nil
	   :db-type (list context-parameter)))
    :metaclass xdb2:storable-versioned-class))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  
  (:data-spec
   :name menu
   :label "Menu"
   :super-classes (doc)
   :data-fields
   ((:name menu-name 
	   :initarg :menu-name
		 :accessor :menu-name
		 :initform nil
		 :db-type string
		 :key t)
      (:name menu-items 
	     :initarg :menu-items
		  :accessor menu-items
		  :initform nil
		  :db-type (list menu-item)))
   :metaclass xdb2:storable-versioned-class))


(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name module
   :label "Module"
   :super-classes (doc)
   :data-fields
   ((:name module-name
	   :initarg :module-name
	   :accessor module-name
	   :initform nil
	   :db-type string
	   :display t
	   :editable t
	   )
    (:name module-short
	   :initarg :module-short
	   :accessor module-short
	   :initform nil
	   :db-type string
	   :key t
	   :display t
	   :editable t
	   )
    (:name contexts
	   :initarg :contexts
	   :accessor contexts
	   :initform nil
	   :db-type (data-group :data-spec context-spec :key-accessor context-name)
	  ;; :display t
	   :editable t
	   )
    (:name menu 
	   :initarg :menu
	   :accessor menu
	   :initform nil
	   :db-type (list menu)
	   :display t
	   :editable t
	   )
    ;;(help)
    )
   :metaclass xdb2:storable-versioned-class
   :collection-name "modules"
   :collection-type :merge
   :default-initargs (:top-level t)))


(defun get-module (module-name)  
  (fetch-item "modules"
	     :test (lambda (doc)
		     (string-equal module-name (module-name doc)))))

(defun get-module-short (module-short)  
  (fetch-item "modules"
	     :test (lambda (doc)
		     (string-equal module-short (module-short doc)))))

(defclass payment-detail (doc)
  ((company-name)
   (company-no)
   (vat-no)
   (currency)
   (frequency))
  (:metaclass xdb2:storable-versioned-class))


#|
;;TODO: is this still being used?
(defclass license-module (doc)
  ((module :initarg :module
	   :accessor module
	   :initform nil
	   :key t
	   :db-type (data-member module))
   (entities :initarg :entities
	     :accessor entities
	     :initform nil
	     :db-type (data-group entity :key-accessor entity-name)))
  (:metaclass data-versioned-class))
|#
