(in-package :cl-wfx)

(defvar *example-type-defs*
  '(:example-type-defs
    (:type :item
     :complex-type :collection
     :data-type "data-type"
     :collection "some-collection"  
     :accessor (:some-field-that-contains-an-item 
		:the-contained-item-field-name 
		:etc :etc ))
			     
    (:type :item
     :complex-type :hierarchical
     :data-type "data-type"
     :collection "some-collection"
     :accessor (:some-field-name :etc)
     :child-accessor  (:some-childholder-field-name :etc))
			     
    (:type :string 
     :complex-type :key-value
     :values ((:key "1" :val "Ones") (:key "2" :val "Twos"))
     :documentation ":type is any basic type")
			     
    (:type :string
     :complex-type :value-string-list
     :values ("rst" "arst" "Ast")
     :delimiter " "
     :documentation ":type is any basic type, delimiter is used to return a string delimeted with this and when set used to split string and coerce basic type"
     )
    
    (:type :string
     :complex-type :value-list
     :values ("rst" "arst" "Ast")
     :documentation ":type is any basic type"
     )
			     
    (:type :list 
     :complex-type :key-value-list ;;((:key "1" :val "Ones") (:key "2" :val "Twos"))
     :value-type :string	   ;;or any other basic type
     )
			     
    (:type :list
     :complex-type :collection-items 
     :data-type "arst"
     :collection "arst"
     :accessor (:context-spec :etc)
     :documentation "Selected items from a collection"
     )
			     
    (:type :list
     :complex-type :item ;;item not in a collection
     :data-type "user-permission"
     :accessor (:context-spec :etc))
			     
    (:type :list
     :complex-type :contained-item ;;item contianed in itself
     :data-type "user-permission"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name)
     )
			     
    (:type :list
     ;;a loose item contianed in another item in a collection
     :complex-type :colletion-contained-item 
     :data-type "user-permission"
     :collection "the-other-collection-from-which-you-want-a-contained-item"
     :accessor (:context-spec :etc)
     :container-accessor (:some-other-field-name))
    ))


(defgeneric getfx (item field &key &allow-other-keys))

(defmethod getfx ((item item) field &key &allow-other-keys)
  (let ((db-type (if (listp (getf field :db-type))
		     (or (getf (getf field :db-type) :complex-type)
			 (getf (getf field :db-type) :type))
		     (getf field :db-type))))
    (getsfx db-type field item)))

(defmethod (setf getfx) (value (item item) field &key &allow-other-keys)
  (let ((db-type (if (listp (getf field :db-type))
		     (or (getf (getf field :db-type) :complex-type)
			 (getf (getf field :db-type) :type))
		     (getf field :db-type))))
    (setf (getsfx db-type field item) value)))

(defun getsfx* (field item)
  (let* ((name (getf field :name)))
    (getx item name)))

(defgeneric getsfx (type field item &key &allow-other-keys))

(defmethod getsfx ((type (eql :symbol)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :keyword)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :string)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :date)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :email)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :script)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :boolean)) field item &key &allow-other-keys)
  (getsfx* field item))

(defmethod getsfx ((type (eql :list)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :list-item)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :data-group)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :data-list)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :data-member)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :data-child-member)) field item &key &allow-other-keys)
   (getsfx* field item))


(defmethod getsfx ((type (eql :number)) field item &key &allow-other-keys)
   (getsfx* field item))

(defmethod getsfx ((type (eql :integer)) field item &key &allow-other-keys)
   (getsfx* field item))

(defun field-type-val (field key)
  (Let ((type (getf field :db-type)))
    (when (listp type)
	(getf type key))))

(defun set-getsfx* (field item value)
  (let* ((name (getf field :name)))
    (setf (getx item name) value)))

(defun setsfx-read* (field item value type-test read-error)
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
	     (setf (getx item name) final-val)
	     (error (frmt read-error final-val)))
	(setf (getx item name) final-val))))


(defgeneric validate-sfx (type field item value &key &allow-other-keys))

(defmethod validate-sfx (type field item value &key &allow-other-keys)
  (values t nil))

(defmethod (setf getsfx) :around (value type field item   
			 &key &allow-other-keys)
  (validate-sfx type field item value))

(defmethod (setf getsfx) (value (type (eql :symbol)) field item   
			 &key &allow-other-keys)
  (setsfx-read* field item value #'symbolp  "~S is not a symbol!"))

(defmethod (setf getsfx) ((type (eql :keyword)) field item value  &key &allow-other-keys)
  (setsfx-read* field item value #'keywordp  "~S is not a keyword!"))

(defmethod (setf getsfx) (value (type (eql :string)) field item &key &allow-other-keys)
   (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) (value (type (eql :email)) field item &key &allow-other-keys)
   (setf (getx item (getf field :name)) (frmt "~A" value)))

(defmethod (setf getsfx) ((type (eql :number)) field item value  
			 &key &allow-other-keys)
  (setsfx-read* field item value #'numberp "~R is not a number!"))

(defmethod (setf getsfx) ((type (eql :integer)) field item value  
			 &key &allow-other-keys)
  (setsfx-read* field item value #'numberp "~R is not an integer!"))

(defmethod (setf getsfx) ((type (eql :date)) field item value  
			 &key &allow-other-keys)
  (set-getsfx* field item value))


(defmethod (setf getsfx) (value (type (eql :value-string-list)) field item   
			 &key &allow-other-keys)
  (let* ((name (getf field :name))
	 (delimiter (coerce (field-type-val field :delimiter) 'character))
	 (type (field-type-val field :type))
	 (split (split-sequence:split-sequence delimiter value))
	 (list))
    (dolist (x split)
      (if (equalp type 'keyword)
	  (setf list (append list 
			     (list (intern (string-upcase (trim-whitespace x)) 
					   :KEYWORD))))
	  (setf list (append list (list (trim-whitespace x))))))
    (setf (getx item name) list)))


(defmethod validate-sfx ((type (eql :value-list)) field item value
			 &key &allow-other-keys)
  (let* ((list (field-type-val field :values))
	 (*read-eval* nil)
	 (valid (find (if (not (equalp (field-type-val field :type) :string))
			  (read-from-string value)
			  value)
		      list :test #'equalp)))
 
    (values valid (if (not valid)
		    (frmt "Value not one of ~S" list)))))


(defmethod (setf getsfx) (value (type (eql :value-list)) field item 
			 &key &allow-other-keys)
  (setf (getsfx (field-type-val field :type) field item) value))


(defmethod (setf getsfx) (value (type (eql :script)) field item   &key &allow-other-keys)
  (setsfx-read* field item value #'consp "~S is not a cons!"))


(defmethod (setf getsfx) (value (type (eql :boolean)) field item   
			 &key &allow-other-keys)
  (set-getsfx* field item value))


(defmethod (setf getsfx) (value (type (eql :collection-items)) field item   
			 &key &allow-other-keys)
  (setsfx-read* field item value #'listp "~R is not a list!"))


(defun type-of-p (field value)
  (equalp (class-name (class-of value)) (field-type-val field :data-type)))



(defmethod (setf getsfx) (value (type (eql :collection)) field item   
			 &key &allow-other-keys)  
  ;;TODO: Remember to use full hash reference in dropdowns???????
 #|
  (if (not (empty-p value))
    (let* ((id (parse-integer value))
	   (full-type (cdr (getf field :db-type)))
	   (data-spec (get-data-spec (field-type-val field :data-spec)))
	   (object (fetch-item (collection-name data-spec)
			       :test (lambda (item)
				      ;; (break "~A ~A ~A" item id (xdb2:id item))
				       (if (equalp id (item-hash item))
					   item))))) 
     ;;(break "~A ~A ~A" item object value)
      (if (type-of-p field object)
	  (set-getsfx* field item object)
	  (error (frmt "~S is not of type ~A!" object (field-type-val field :data-spec)))))
    )
 |#
  (set-getsfx* field item value)
  )


(defmethod (setf getsfx) ((type (eql :hierarchical)) field item value  
			 &key source &allow-other-keys) 
 
  ;;TODO: will id not be full ref hash now?
  #|
  (if (not (empty-p value))
      
    (let* ((id (parse-integer value))
	   (list (getx source
		       (field-type-val field :child-accessor)))
	   (object)) 
      
      (dolist (contact list)
	(when (equalp id (item-hash contact))
	  (setf object contact)))
      (if (type-of-p field object)
	  (set-getsfx* field item object)
	  (error (frmt "~S is not of type ~A!" object (field-type-val field :data-type)))))
  |#
    (set-getsfx* field item nil))



