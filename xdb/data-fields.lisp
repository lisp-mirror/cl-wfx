(in-package :cl-wfx)

(defun item-val* (item-def item)
  (let* ((name (getf item-def :name)))
    (slot-value item name)))

(defmethod item-val ((type (eql 'symbol)) item-def item &key &allow-other-keys)
   (item-val* item-def item))

(defmethod item-val ((type (eql 'keyword)) item-def item &key &allow-other-keys)
   (item-val* item-def item))



(defmethod item-val ((type (eql 'string)) item-def item &key &allow-other-keys)
  (item-val* item-def item))

(defmethod item-val ((type (eql 'email)) item-def item &key &allow-other-keys)
  (item-val* item-def item))

(defmethod item-val ((type (eql 'script)) item-def item &key &allow-other-keys)
  (item-val* item-def item))

(defmethod item-val ((type (eql 'boolean)) item-def item &key &allow-other-keys)
  (item-val* item-def item))


(defmethod item-val ((type (eql 'list)) item-def item &key &allow-other-keys)
   (item-val* item-def item))

(defmethod item-val ((type (eql 'data-group)) item-def item &key &allow-other-keys)
   (item-val* item-def item))

(defmethod item-val ((type (eql 'number)) item-def item &key &allow-other-keys)
   (item-val* item-def item))


(defun set-item-val* (item-def item value)
  (let* ((name (getf item-def :name)))
    (setf (slot-value item name) value)))

(defun set-item-val-read* (item-def item value type-test read-error)
  (let* ((name (getf item-def :name))
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

(defmethod set-item-val ((type (eql 'symbol)) item-def item value  
			 &key &allow-other-keys)
  (set-item-val-read* item-def item value #'symbolp  "~S is not a symbol!"))

(defmethod set-item-val ((type (eql 'keyword)) item-def item value  &key &allow-other-keys)
  (set-item-val-read* item-def item value #'keywordp  "~S is not a keyword!"))

(defmethod set-item-val ((type (eql 'list)) item-def item value  
			 &key &allow-other-keys)
  (set-item-val* item-def item value))

(defmethod set-item-val ((type (eql 'string)) item-def item value  &key &allow-other-keys)
   (let* ((name (getf item-def :name)))
       (setf (slot-value item name) (frmt "~A" value))))

(defmethod set-item-val ((type (eql 'email)) item-def item value  &key &allow-other-keys)
   (let* ((name (getf item-def :name)))
       (setf (slot-value item name) (frmt "~A" value))))

(defmethod set-item-val ((type (eql 'script)) item-def item value  &key &allow-other-keys)
  (set-item-val-read* item-def item value #'consp  "~S is not a cons!"))

(defmethod set-item-val ((type (eql 'boolean)) item-def item value  
			 &key &allow-other-keys)
  (set-item-val* item-def item value))

(defmethod set-item-val ((type (eql 'data-group)) item-def item value  
			 &key &allow-other-keys)
  (set-item-val-read* item-def item value #'listp "~R is not a list!"))

(defmethod set-item-val ((type (eql 'data-group)) item-def item value  
			 &key &allow-other-keys)
  (set-item-val-read* item-def item value #'numberp "~R is not a number!"))
