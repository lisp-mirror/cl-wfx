(in-package :cl-wfx)

(defvar *data* nil)

(add-core-definitions
 '((:document-type
    (:name "file"
     :label "file"
     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :path
       :label "path"
       :key-p nil
       :concrete-type :lambda
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:document-type
    (:name "report"
     :label "Report"
     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :lambda
       :attributes (:display t :editable t))
      (:name :permissions
       :label "Permissions"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t))
      (:name :args
       :label "Args"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t))
      (:name :files
       :label "Files"
       :concrete-type (:type :document
		       :complex-type :list-objects
		       :document-type "file"
		       :accessor (:name))))
     :destinations (:core :system :license)))

   (:collection
    (:name "reports"
     :label "Reports"
     :document-type "report")
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

   (:document-type
    (:name "entity-report"
     :label "Entity Report"

     :elements
     ((:name :entity
       :label "Entity"
       :key-p t
       :concrete-type (:type :list
		       :complex-type :collection
		       :document-type "entity"
		       :collection "entities"
		       :accessor (:name))
       :attributes (:display t :editable t)
       :documentation "")
      (:name :name
       :label "Name"
       :key-p t
       :concrete-type :string
       :attributes (:display t :editable t))
      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :lambda
       :attributes (:display t :editable t))
      (:name :permissions
       :label "Permissions"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t))
      (:name :args
       :label "Args"
       :key-p nil
       :concrete-type (:type :keyword
		       :complex-type :value-string-list
		       :delimiter " ")
       :attributes (:display t :editable t))
      (:name :files
       :label "Files"
       :concrete-type (:type :document
		       :complex-type :list-objects
		       :document-type "file"
		       :accessor (:name))))
     :destinations (:license)))

   (:collection
    (:name "entity-reports"
     :label "Entity Reports"
     :document-type "entity-report")
    :destinations (:license))))

(defvar *document* nil)

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :data-row))
				  element)
  (when *data*
    (when (getf *data* :data)
      (with-html-string
	  (dolist (*document* (eval (getf element :data-script)))
	    (when *document*
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
					   elementx))))
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
  (when (digx report :code :selection-lambda)
    (eval (digx report :code :selection-lambda))))

(defmethod render-report ((type (eql :html)) report-name)
  (let* ((report (wfx-query-document
		  "reports"
		  :query (lambda (document)
			   (equalp (getx document :name)
				   report-name)))))
    (cl-wfx::with-html-string
	(:div :id "report-body"
	      (when report
		(cl-who:str
		 (eval (getx report :code))))))))

(defun ajax-report (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (render-report :html (getx (context-spec *context*) :report)))

