(in-package :cl-wfx)

(defvar *data* nil)

(add-core-definitions
 '((:data-type
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
	     :attributes (:display t :editable t)))
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
	 (:license (:update :delete :lookup))))))))

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
;;	(break "poes ~A" (eval (getf element :data-script)))
	  (dolist (*item* (eval (getf element :data-script)))
	    
	    (when *item*
		(cl-who:htm
		 (:div :class "row"	       
		       (dolist (cell (getf element :cells))
			 (cl-who:htm
			;;  (break "wtf ~A" *item*)
			  (:div :class "col"
				(cl-who:str (if (getf cell :value)
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
		    (cl-who:str (if (getf cell :value)
						(getf cell :value)
						(eval (getf cell :script)))))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :footer-row))
				  element)
  (with-html-string
    (:div :class "row"
	  (dolist (cell (getf element :cells))
	    (cl-who:htm
	     (:div :class "col"
		   (:h5
		    (cl-who:str (if (getf cell :value)
						(getf cell :value)
						(eval (getf cell :script)))))))))))

(defmethod render-report-element ((type (eql :html))
				  (element-type (eql :table))
				  element)
  (cl-wfx::with-html-string
    (:table
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
		   (:image :href (getf element :image))))
		(:h3
		 (cl-who:str (getf element :text)))))))



(defun render-selection (report)
  (when (digx report :code :selection-script)
    (eval (digx report :code :selection-script))))


(defmethod render-report ((type (eql :html)) report-name)
  (let* ((report (wfx-fetch-item "reports"
				:test (lambda (item)
					(equalp (getx item :name)
						report-name))))
	 (*data* (eval (digx report :code :data-script))))
    
    (cl-wfx::with-html-string
      (:div (cl-who:str (render-selection report)))
      (:div
       (dolist (element (digx report :code :elements))
	 (cl-who:str
	  (render-report-element type (getf element :type) element)))))))

(defun ajax-report (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (render-report :html (getx (context-spec *context*) :report)))



