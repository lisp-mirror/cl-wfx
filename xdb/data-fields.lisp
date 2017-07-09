(in-package :cl-wfx)

(defun item-val* (field item)
  (let* ((name (getf field :name)))
    (slot-value item name)))

(defmethod item-val ((type (eql 'symbol)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'keyword)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'string)) field item &key &allow-other-keys)
  (item-val* field item))

(defmethod item-val ((type (eql 'email)) field item &key &allow-other-keys)
  (item-val* field item))

(defmethod item-val ((type (eql 'script)) field item &key &allow-other-keys)
  (item-val* field item))

(defmethod item-val ((type (eql 'boolean)) field item &key &allow-other-keys)
  (item-val* field item))


(defmethod item-val ((type (eql 'list)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'list-item)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'data-group)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'data-member)) field item &key &allow-other-keys)
   (item-val* field item))

(defmethod item-val ((type (eql 'number)) field item &key &allow-other-keys)
   (item-val* field item))


(defun set-item-val* (field item value)
  (let* ((name (getf field :name)))
    (setf (slot-value item name) value)))

(defun set-item-val-read* (field item value type-test read-error)
  (let* ((name (getf field :name))
	 (*read-eval* nil)
	 (final-val))
    
    (if value
	(if (stringp value)
	    (setf final-val (read-from-string value))
	    (setf final-val value))
	(setf final-val value))
    
    (if final-val
	(if  (apply type-test (list final-val))
	     (setf (slot-value item name) final-val)
	     (error (frmt read-error final-val)))
	(setf (slot-value item name) final-val))))

(defmethod set-item-val ((type (eql 'symbol)) field item value  
			 &key &allow-other-keys)
  (set-item-val-read* field item value #'symbolp  "~S is not a symbol!"))

(defmethod set-item-val ((type (eql 'keyword)) field item value  &key &allow-other-keys)
  (set-item-val-read* field item value #'keywordp  "~S is not a keyword!"))

(defmethod set-item-val ((type (eql 'list)) field item value  
			 &key &allow-other-keys)
  (set-item-val* field item value))


(defmethod validate-item-val ((type (eql 'list-item)) field item value  
			 &key &allow-other-keys)
  (let* ((full-type (cdr (getf field :db-type)))
	 (list (getf full-type :list))
	 (*read-eval* nil)
	 (valid (find (if (not (equalp (getf full-type :type) 'string))
			  (read-from-string value)
			  value)
		      list :test #'string-equal)))
 
    (list valid (if (not valid)
		    (frmt "Value not one of ~S" list)))))

(defmethod set-item-val ((type (eql 'list-item)) field item value  
			 &key &allow-other-keys)
  (let ((full-type (cdr (getf field :db-type))))
    (set-item-val (getf full-type :type) field item value)))

(defmethod set-item-val ((type (eql 'string)) field item value  &key &allow-other-keys)
   (let* ((name (getf field :name)))
       (setf (slot-value item name) (frmt "~A" value))))

(defmethod set-item-val ((type (eql 'email)) field item value  &key &allow-other-keys)
   (let* ((name (getf field :name)))
       (setf (slot-value item name) (frmt "~A" value))))

(defmethod set-item-val ((type (eql 'script)) field item value  &key &allow-other-keys)
  (set-item-val-read* field item value #'consp  "~S is not a cons!"))

(defmethod set-item-val ((type (eql 'boolean)) field item value  
			 &key &allow-other-keys)
  (set-item-val* field item value))

(defmethod set-item-val ((type (eql 'data-group)) field item value  
			 &key &allow-other-keys)
  (set-item-val-read* field item value #'listp "~R is not a list!"))


(defun type-of-p (field value)
  (let* ((full-type (cdr (getf field :db-type))))
    (equalp (class-name (class-of value)) (getf full-type :data-spec))))

(defmethod set-item-val ((type (eql 'data-member)) field item value  
			 &key &allow-other-keys)  
  (if (not (empty-p value))
    (let* ((id (parse-integer value))
	   (full-type (cdr (getf field :db-type)))
	   (data-spec (get-data-spec (getf full-type :data-spec)))
	   (object (fetch-item (collection-name data-spec)
			       :test (lambda (item)
				       (if (equalp id (xdb2:id item))
					   item)))))    
      (if (type-of-p field object)
	  (set-item-val* field item object)
	  (error (frmt "~S is not of type ~A!" object (getf full-type :data-spec)))))
    (set-item-val* field item nil)))

(defmethod set-item-val ((type (eql 'number)) field item value  
			 &key &allow-other-keys)
  (set-item-val-read* field item value #'numberp "~R is not a number!"))
