(in-package :cl-wfx)

(defvar *data* nil)

(add-core-definitions
 '((:data-type
    (:name "file"
     :label "file"
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :path
	     :label "path"
	     :key-p nil
	     :db-type :script
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:data-type
    (:name "report"
     :label "Report"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :script
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :args 
	     :label "Args"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :files
	     :label "Files"
	     :db-type (:type :item
			     :complex-type :list-items
			     :data-type "file"
			     :accessor (:name))))
     :destinations (:core :system :license)))
   
   (:collection
      (:name "reports"
	     :label "Reports"
	     :data-type "report")
      :destinations (:core :system :license)
      :access
      (:stores
       (:core
	(:user-levels
	 (:core (:update :delete :lookup))))
       (:system
	(:user-levels
	 (:core (:update :delete :lookup))
	 (:system (:update :delete :lookup))
	 (:license (:view :copy :lookup))))
       (:license
	(:user-levels
	 (:core (:update :delete :lookup))
	 (:system (:update :delete :lookup))
	 (:license (:update :delete :lookup))))))

   (:data-type
    (:name "entity-report"
     :label "Entity Report"
     :top-level-p t
     :fields
     ((:name :entity
	     :label "Entity"
	     :key-p t
	     :db-type (:type :list
			     :complex-type :collection
			     :data-type "entity"
			     :collection "entities"
			     :accessor (:name))
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :string
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :script
	     :attributes (:display t :editable t))
      (:name :permissions 
	     :label "Permissions"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :args 
	     :label "Args"
	     :key-p nil
	     :db-type (:type :keyword
			     :complex-type :value-string-list
			     :delimiter " ")
	     :attributes (:display t :editable t))
      (:name :files
	     :label "Files"
	     :db-type (:type :item
			     :complex-type :list-items
			     :data-type "file"
			     :accessor (:name))))
     :destinations (:license)))

   (:collection
	(:name "entity-reports"
	 :label "Entity Reports"
	 :data-type "entity-report"
;;	 :bucket-keys (:entity)
	 )
	:destinations (:license))))

(defun call-data-script (name)
  (let ((script (wfx-fetch-items "scripts"
				 :test (lambda (item)
					 (equalp (getx item :name) name)))))
    (when script
      (funcall (getx script :code)))))

(defvar *item* nil)

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :data-row))
				  element)
  (when *data*
    (when (getf *data* :data) 
      (with-html-string
	  (dolist (*item* (eval (getf element :data-script)))
	    (when *item*
		(cl-who:htm
		 (:div :class "row"	       
		       (dolist (cell (getf element :cells))
			 (cl-who:htm
			  (:div :class "col"
				(cl-who:str
				 (if (getf cell :value)
				     (getf cell :value)
				     (eval (getf cell :script)))))))))))))))


(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :footer))
				  element)
  
  (with-html-string    
    (:div :class "row"
	  (dolist (cell (getf element :cells))
	    (cl-who:htm
	     (:div :class "col"
		   (:h6
		    (cl-who:str (if (getf cell :value)
				    (getf cell :value)
				    (eval (getf cell :script)))))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :header-row))
				  element)
  (with-html-string
    (:div :class "row"
	  (dolist (cell (getf element :cells))
	    (cl-who:htm
	     (:div :class "col"
		   (:h5
		    (cl-who:str
		     (if (getf cell :value)
			 (getf cell :value)
			 (eval (getf cell :script)))))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :row))
				  element)
  (with-html-string
    (:div :class "row"
	  (dolist (cell (getf element :cells))
	    (cl-who:htm
	     (:div :class "col"
		
		   (cond ((getf cell :value)
			   (cl-who:str
			    (getf cell :value)))
			 ((getf cell :type)
		
			   (dolist (elementx (getf cell :elements))
			     (cl-who:str (render-report-element
					  type
					  (getf elementx :type)
					  elementx
					  ))))
			  ((getf cell :script)
			   (cl-who:str
			    (eval (getf cell :script))))
			  (t
			   (error "WTF?")))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :footer-row))
				  element)
  (with-html-string
    (:div :class "row"
	  (dolist (cell (getf element :cells))
	    (cl-who:htm
	     (:div :class "col"
		   (:h5
		    (cl-who:str
		     (if (getf cell :value)
			 (getf cell :value)
			 (eval (getf cell :script)))))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :table))
				  element)
  (cl-wfx::with-html-string
    (:div :class "col"
     (dolist (table-element (getf element :elements))
       (cl-who:str
	(render-report-element
	 type (getf table-element :type) table-element))))))


(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :title))
				  element)
  (cl-wfx::with-html-string
    (:div :class "row"	  
	  (:div :class "col"
		(when (getf element :image)
		  (cl-who:htm
		   (:image :src (getf element :image))))
		(:h3
		 (cl-who:str (getf element :text)))))))

(defun render-selection (report)
  (when (digx report :code :selection-script)
    (eval (digx report :code :selection-script))))

(defmethod render-report ((type (eql :html)) report-name)
  (let* ((report (wfx-fetch-item "reports"
				 :test (lambda (item)
					 (equalp (getx item :name)
						 report-name)))))
    (cl-wfx::with-html-string
      (:div :id "report-body"
	    (when report
	      (cl-who:str 
	       (eval (getx report :code))))))))

(defun ajax-report (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (render-report :html (getx (context-spec *context*) :report)))



