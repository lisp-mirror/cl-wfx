(in-package :cl-wfx)

(defclass data-mixin (xdb2:storable-mixin)
  ((collection-type :initarg :collection-type
               :initform nil
               :accessor collection-type
	       :documentation "System,merge,license. Nil = system default")
   (collection-name :initarg :collection-name
               :initform nil
               :accessor collection-name)
   (report :initarg :report
           :initform nil
           :accessor report)
   (all-slots :initarg :all-slots
              :initform nil
              :accessor all-slots
              :documentation "Includes virtual slots")
   (before-persist :initarg :before-persist
		  :accessor before-persist-event
		  :initform nil)
   (after-persist :initarg :after-persist
		  :accessor after-persist-event
		  :initform nil)
  #| (url :initarg :url
        :initform nil
        :accessor url)
   |#
   ))

(defclass data-class (data-mixin xdb2:storable-class)
  ())

(defclass data-versioned-class (data-mixin xdb2:storable-versioned-class)
  ())

(defclass data-object (xdb2:storable-object)
  ()
  (:metaclass data-class))

(defclass data-versioned-object (xdb2:storable-versioned-object data-object)
  ()
  (:metaclass data-versioned-class))

(defclass data-slot (xdb2:storable-slot)
  ((header :initarg :header
           :initform `(,xdb2:*slot-dummy* nil)
           :accessor header)
   (required :initarg :required
             :initform `(,xdb2:*slot-dummy* nil)
             :accessor required)
   (virtual :initarg :virtual
            :initform `(,xdb2:*slot-dummy* nil)
            :accessor virtual)
   (validate :initarg :validate
             :initform `(,xdb2:*slot-dummy* nil)
             :accessor validate)))

(defclass data-versioned-slot (data-slot xdb2:storable-versioned-slot)
  ())

(defclass data-direct-slot-definition
    (data-slot standard-direct-slot-definition)
  ())

(defclass data-effective-slot-definition
    (data-slot standard-effective-slot-definition)
  ())

(defclass data-versioned-direct-slot-definition
    (data-versioned-slot standard-direct-slot-definition)
  ())

(defclass data-versioned-effective-slot-definition
    (data-versioned-slot standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class data-mixin)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'data-direct-slot-definition))

(defmethod effective-slot-definition-class ((class data-mixin) &key)
  (find-class 'data-effective-slot-definition))

(defmethod direct-slot-definition-class ((class data-versioned-class) &key)
  (find-class 'data-versioned-direct-slot-definition))

(defmethod effective-slot-definition-class ((class data-versioned-class)
                                            &key)
  (find-class 'data-versioned-effective-slot-definition))


(declaim (inline slot-val))
(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))


(defmethod compute-effective-slot-definition
    ((class data-mixin) slot-name direct-definitions)
  (declare (ignore slot-name))

  (call-next-method)

 ;; (break "~A ~A" class slot-name)

  (let ((effective-definition (call-next-method))
        (direct-definition (car direct-definitions))
        (slots '(header required virtual validate)))
    (when (typep direct-definition 'data-slot)      
	(loop for slot in slots
	   do
	     (xdb2:compute-slot-option effective-definition
				  slot
				  direct-definitions)))
    (loop for slot in slots
       for value = (slot-val effective-definition slot)
       when (and (consp value)
		 (eq (car value) xdb2:*slot-dummy*))
       do (setf (slot-value effective-definition slot) (cadr value)))
      
    (setf (virtual effective-definition)
	  (xdb2:parse-function (virtual effective-definition))
	  (validate effective-definition)
	  (xdb2:parse-function (validate effective-definition)))
    effective-definition))

(defmethod compute-slots ((class data-mixin))
  (let* ((slots (call-next-method))
         (sorted (stable-sort
                  (loop for slot in slots
                        collect (cons (cond ((numberp (header slot))
                                             (header slot))
                                            ((numberp (xdb2:key slot))
                                             (xdb2:key slot))
                                            (t
                                             0))
                                      slot))
                  #'< :key #'car)))
    (setf (all-slots class)
          (map-into sorted #'cdr sorted))
    (remove-if #'virtual slots)))



(defmethod initialize-instance :around ((class data-class)
                                        &rest args)
  (apply #'xdb2:initialize-storable-class #'call-next-method 
                         class 'data-object args))

(defmethod reinitialize-instance :around ((class data-class)
                                          &rest args)
  (apply #'xdb2:initialize-storable-class #'call-next-method
                         class 'data-object args))


(defmethod initialize-instance :around ((class data-versioned-class)
                                        &rest args)
  (apply #'call-next-method class 
			 :default-superclass 'data-versioned-object
                         args))

(defmethod reinitialize-instance :around ((class data-versioned-class)
                                          &rest args)
  (apply #'call-next-method  class 
			 :default-superclass 'data-versioned-object
                         args))


(defclass doc ()
  ((user :initarg :user
         :initform nil
         :accessor user)
   (log-action :initarg :log-action
               :initform nil
               :accessor log-action
               :documentation "Inserted, updated, deleted, rolledback."))
  (:metaclass data-mixin))

#|
(defun collection-from-doc (doc)
  (collection-from-class (class-of doc)))
|#

(defmethod xdb2:doc-collection ((doc data-object))
  (when (xdb2:top-level doc)
    
    (collection-from-doc doc)))

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
             :validate #'validate-end-date))
  (:metaclass data-mixin))
