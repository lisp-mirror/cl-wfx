(in-package :cl-wfx)

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
    (:data-spec
     :name allsort
     :label "Allsort"
     :super-classes (license-doc)
     :data-fields
     ((:name name
	     :initarg :name
	     :accessor name
	     :label "Name"
	     :key-p t
	     :db-type symbol
	     :display t
	     :editable nil)
      (:name sort-value
	     :initarg :sort-value
	     :accessor sort-value
	     :initform nil
	     :db-type keyword
	     :display t
	     :editable t)
      (:name sort-order
	     :initarg :sort-order
	     :accessor sort-order
	     :initform nil
	     :db-type number
	     :display t
	     :editable t)
      (:name description
	     :initarg :description
	     :accessor description
	     :initform nil
	     :db-type string
	     :display t
	     :editable t))
     
  
     :metaclass xdb2:storable-versioned-class
     :collection-name ""
     :collection-type :merge
     :default-initargs (:top-level t)))
