(in-package :cl-wfx)

(defclass doc ()
  ((user :initarg :user
         :initform nil
         :accessor user)
   (log-action :initarg :log-action
               :initform nil
               :accessor log-action
               :documentation "Inserted, updated, deleted, rolledback."))
  (:metaclass xdb2:storable-mixin))


(defclass date-doc ()
  ((start-date :initarg :start-date
               :initform nil
               :accessor start-date
               :db-type date
               :parser #'parse-date)
   (end-date :initarg :end-date
             :initform nil
             :accessor end-date
             :db-type date
             :parser #'parse-date
            ;; :validate #'validate-end-date
	     ))
  (:metaclass xdb2:storable-mixin))

(defclass license-doc (doc)
     ((license :initarg :license
	       :initform nil
	       :accessor license
	       :db-type (data-member license :key-accessor license-code)
	       :key t
	     ;;  :header t
	     ;;  :required t
	       :printer #'print-license-code))
     (:metaclass xdb2:storable-mixin))
