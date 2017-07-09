(in-package :cl-wfx)
;;(break "common")
(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name doc
   :label "Doc"
   :super-classes ()
   :data-fields
   ((:name user :initarg :user
	   :initform nil
	   :accessor user
	   :db-type string
	   :display t)
    (:name log-action :initarg :log-action
	   :initform nil
	   :accessor log-action
	   :db-type string
	   :display t
	   :documentation "Inserted, updated, deleted, rolledback."))
   :metaclass xdb2:storable-mixin))

(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name date-doc
   :label "Date Doc"
   :super-classes ()
   :data-fields
   ((:name start-date :initarg :start-date
	   :initform nil
	   :accessor start-date
	   :key t
	   :db-type date
	   :parser #'parse-date
	   :display t
	   :editable t
	   )
    (:name end-date :initarg :end-date
	   :initform nil
	   :accessor end-date
	   :db-type date
	   :parser #'parse-date
	   :display t
	   :editable t
	   ))
   :metaclass xdb2:storable-mixin))


(monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
  (:data-spec
   :name license-doc
   :label "License Doc"
   :super-classes (doc)
   :data-fields
   ((:name license-code :initarg :license-code
	   :initform nil
	   :accessor license-code
	   :db-type string		   
	   :display t
	   :editable t))
   :metaclass xdb2:storable-mixin))

