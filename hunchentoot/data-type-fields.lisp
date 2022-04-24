(in-package :cl-wfx)

(defun print-document-val-s* (element document &key default-value)
  (let ((*print-case* :downcase)
	(val (if document (getx document element)))
	(accessors (if (listp (getx element :concrete-type))
		       (digx element :concrete-type :accessor))))

    (when (and (listp val)
	       (or (listp (first val))
		   (document-p (first val))))
      (when (document-p val))
      (setf val (first val)))

    (if (if accessors
	    (accessor-value val accessors)
	    val)
	(frmt "~S" val)
	(if default-value
	    default-value
	    ""))))

(defun print-document-val-a* (element document &key default-value)
  (let* ((*print-case* :downcase)
	 (val (if document (getx document element)))
	 (accessors (if (listp (getx element :concrete-type))
			(digx element :concrete-type :accessor)))
	 (final-val (if val
			(frmt "~A" (if accessors
				       (accessor-value val accessors)
				       val))
			(if default-value
			    default-value
			    ""))))

    (replace-all (replace-all final-val "'" "&#39;") "\"" "&#34;")))

(defmethod print-document-val ((type (eql :symbol)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :keyword)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :lambda)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defun print-document-blob* (element document &key default-value)
  (let ((*print-case* :downcase)
	(blob (and document (getx document element))))

    (if (and blob (blob-p blob))
	(blob-raw blob)
	default-value)))

(defmethod print-document-val ((type (eql :lisp-code)) element document
			       &key default-value &allow-other-keys)
  (print-document-blob* element document :default-value default-value))

(defmethod print-document-val ((type (eql :css)) element document
			       &key default-value &allow-other-keys)
  (print-document-blob* element document :default-value default-value))

(defmethod print-document-val ((type (eql :html)) element document
			       &key default-value &allow-other-keys)
  (print-document-blob* element document :default-value default-value))

(defmethod print-document-val ((type (eql :java-script)) element document
			       &key default-value &allow-other-keys)
  (print-document-blob* element document :default-value default-value))

(defmethod print-document-val ((type (eql :text-blob)) element document
			       &key default-value &allow-other-keys)
  (print-document-blob* element document :default-value default-value))

(defmethod print-document-val ((type (eql :string)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :link)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :text)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defun file-server-path (collection element document)
  (string-downcase
   (frmt "~A/files/~A/~A/~A"
	 (if (and (document-collection document)
		  (location (document-collection document)))
	     (location (document-collection document))
	     (string-downcase
	      (frmt "~A~A"
		    (location
		     (store
		      collection))
		    (name collection))))
	 (document-document-type document)
	 (getx element :name)
	 ;; (document-hash document)
	 (if (not (empty-p (getx document (getx element :name))))
	     (sanitize-file-name (getx document (getx element :name)))))))

(defun file-url (collection element document)
  (string-downcase
   (frmt "~A~A/~A/files/~A/~A/~A"
	 (site-url *system*)
	 (first (getx (active-user) :selected-licenses))
	 (name collection)
	 (document-document-type document)
	 (getx element :name)
	 ;; (document-hash document)
	 (if (not (empty-p (getx document (getx element :name))))
	     (sanitize-file-name (getx document (getx element :name)))))))

(defmethod print-document-val ((type (eql :image)) element document
			       &key default-value &allow-other-keys)
  (declare (ignore default-value))

  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (server-path (file-server-path collection element document)))
    (with-html-string

	(if (getx document (getx element :name))
	    (let ((image-url (file-url collection element document)))
	      (push (hunchentoot::create-static-file-dispatcher-and-handler
		     image-url
		     server-path)
		    hunchentoot::*dispatch-table*)
	      (cl-who:htm
	       (:a :data-toggle "modal"
		   :data-target (string-downcase
				 (frmt "#~A-modal" (getx element :name)))
		   (:img
		    :style "width:128px;height:128px;"
		    :src image-url))
	       (:div :class "modal fade"
		     :tab-index -1
		     :id (string-downcase (frmt "~A-modal" (getx element :name)))
		     (:div :class "modal-dialog modal-dialog-centered "
			   (:img
			    :style "max-width:120%;"
			    :src image-url)))))
	    (cl-who:htm (:img :src "/umage/cor/web/images/logo-small.png"))))))

(defmethod print-document-val ((type (eql :email)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :file)) element document
			       &key default-value &allow-other-keys)
  (declare (ignore default-value))

  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (server-path (file-server-path collection element document)))
    (with-html-string

	(if (getx document (getx element :name))
	    (let ((file-url (file-url collection element document)))

	      (push (hunchentoot::create-static-file-dispatcher-and-handler
		     file-url
		     server-path)
		    hunchentoot::*dispatch-table*)
	      (cl-who:htm
	       (:a :href file-url
		   (cl-who:str file-url))))))))

(defmethod print-document-val ((type (eql :number)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :integer)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :date-time)) element document
			       &key default-value &allow-other-keys)
  (let* ((*print-case* :downcase)
	 (val (if document (getx document element)))
	 (accessors (if (listp (getx element :concrete-type))
			(digx element :concrete-type :accessor)))
	 (accessor-val (if accessors
			   (accessor-value
			    val
			    accessors)
			   val))
	 (final-val (if val
			(frmt "~A"
			      (if (typep val 'local-time:timestamp)
				  (progn
				    (local-time:format-timestring
				     nil
				     accessor-val
				     :format
				     '((:year 4) #\- (:month 2) #\- (:day 2)
				       #\T (:hour 2) #\: (:min 2))))
				  accessor-val))
			(if default-value
			    default-value
			    ""))))

    final-val))

(defmethod print-document-val ((type (eql :date)) element document
			       &key default-value &allow-other-keys)

  (let* ((*print-case* :downcase)
	 (val (if document (getx document element)))
	 (accessors (if (listp (getx element :concrete-type))
			(digx element :concrete-type :accessor)))

	 (final-val (if val
			(frmt "~A" (if accessors
				       (accessor-value (if (typep val 'local-time:timestamp)
							   (local-time:format-timestring
							    local-time:+iso-8601-date-format+ val)
							   val)
						       accessors)
				       (if (typep val 'local-time:timestamp)
					   (local-time:format-timestring
					    local-time:+iso-8601-date-format+ val)
					   val)))
			(if default-value
			    (if (typep default-value 'local-time:timestamp)
				(local-time:format-timestring
				 local-time:+iso-8601-date-format+ default-value)
				default-value)
			    ""))))
    final-val))

(defmethod print-document-val ((type (eql :time)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-a* element document :default-value default-value))

(defmethod print-document-val ((type (eql :boolean)) element document
			       &key default-value &allow-other-keys)
  (if (getx document element)
      "checked"
      (if default-value
	  default-value
	  "")))

(defmethod print-document-val ((type (eql :key-value)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :document)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :value-list)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :key-value-list)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :value-string-list))
			       element document &key default-value
			       &allow-other-keys)
  (declare (ignore default-value))

  (let* ((delimiter (digx element :concrete-type :delimiter))
	 (val (cl-naive-store.document-type-defs::getxe document element (digx element :concrete-type :type)))
	 (final-val))

    (when (and val (listp val))

      (dolist (x val)
	(setf final-val
	      (if final-val
		  (concatenate 'string final-val (if (stringp delimiter)
						     delimiter
						     (eval delimiter))
			       (if (equalp (digx element :concrete-type :type)
					   :keyword)
				   (string-downcase (frmt "~S" x))
				   x))
		  (if (equalp (digx element :concrete-type :type)
			      :keyword)
		      (string-downcase (frmt "~S" x))
		      x)))))

    final-val))

(defmethod print-document-val ((type (eql :list-objects)) element document
			       &key default-value &allow-other-keys)
  (print-document-val-s* element document :default-value default-value))

(defmethod print-document-val ((type (eql :collection-objects))
			       element document &key default-value
			       &allow-other-keys)
  (declare (ignore default-value))

  (let ((document-val (getx document element))
	(accessor (digx element :concrete-type :accessor)))

    (when (listp document-val)
      (when document-val
	(frmt "~A"
	      (accessor-value (first document-val) accessor))))))

(defmethod print-document-val ((type (eql :contained-document)) element document
			       &key default-value
			       &allow-other-keys)
  (declare (ignore default-value))

  (let ((document-val (getx document element))
	(accessor (digx element :concrete-type :accessor)))

    (when (or (listp document-val)
	      (document-p document-val))
      (when document-val
	(frmt "~A"
	      (accessor-value document-val accessor))))))

(defmethod print-document-val ((type (eql :collection-contained-document))
			       element document &key default-value
			       &allow-other-keys)
  (declare (ignore default-value))
  (print-document-val-s* element document))

(defmethod print-document-val ((type (eql :collection)) element document
			       &key &allow-other-keys)

  (let ((document-val (getx document element))
	(accessor (digx element :concrete-type :accessor)))

    (when (or (listp document-val)
	      (document-p document-val))
      (when document-val
	(frmt "~A"
	      (accessor-value document-val accessor))))))

(defmethod print-document-val ((type (eql :hierarchical)) element document
			       &key default-value &allow-other-keys)
  (declare (ignore default-value))

  ;;TODO: Sort this shit out need to loop tree
  (let ((document-val (getx document element))
	(final))
    (when document-val
      (dolist (x document-val)
	(if final
	    (setf final (frmt "~A ~A"
			      final
			      (accessor-value x (digx element :concrete-type :accessor))))
	    (frmt "~A" (accessor-value x (digx element :concrete-type :accessor))))))))

(defgeneric render-input-val (type element document &key &allow-other-keys))

(defun document-default-val* (element document)
  (if (getx element :default-value)
      (if (stringp (getx element :default-value))
	  (getx element :default-value)
	  (apply
	   (eval%
	    (getx element :default-value))
	   (list document)))))

(defun render-input-val* (type element document)
  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "text"
		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "text"
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or
		     (parameter name)
		     (print-document-val
		      type
		      element
		      document
		      :default-value
		      (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :symbol)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :keyword)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :string)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :link)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :text)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control"
	     :id name
	     :name name :cols 50 :rows 3
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document
			  :default-value
			  (document-default-val* element document)))))
	(with-html-string
	    (:textarea
	     :class "form-control"
	     :id name
	     :name name :cols 50 :rows 3
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document
		:default-value
		(document-default-val* element document)))))))))

(defun render-image (element document)
  (let* ((collection (wfx-get-collection
		      (gethash :collection-name
			       (cache *context*))))
	 (image-path (file-url collection element document)))
    (with-html-string
	(:input :type "hidden"
		:id (string-downcase
		     (frmt "args-~A-~A"
			   (getx element :name)
			   (document-hash document)))
		:value (string-downcase
			(frmt "{\"license\":\"~A\", \"collection\":\"~A\", \"datatype\":\"~A\", \"element\":\"~A\"}"
			      (first (getx (active-user) :selected-licenses))
			      (name collection)
			      (document-document-type document)
			      (getx element :name))))

      (:input :type "hidden"
	      :id (string-downcase
		   (frmt "init-~A-~A" (getx element :name)
			 (document-hash document)))
	      :value image-path
	      :name (string-downcase
		     (frmt "~A" (getx element :name))))

      (:input :type "file"
	      :class "file-upload"
	      :multiple t
	      :id (string-downcase
		   (frmt "~A-~A" (getx element :name)
			 (document-hash document)))
	      :required (if (getx element :key-p)
			    "required")))))

(defun ajax-render-file-upload (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))

  (let* ((document-type (parameter "document-type"))
	 (document (getcx document-type :edit-document))
	 (elements (getcx document-type :elements)))

    (dolist (element elements)
      (when (equalp (getx element :name) (parameter "element-name"))
	(return-from ajax-render-file-upload
	  (render-image element document))))))

(defmethod render-input-val ((type (eql :image)) element document
			     &key  &allow-other-keys)

  (with-html-string
      (:div :class "col"
	    (:div :class "row"
		  :id (string-downcase
		       (frmt "file-upload-row-~A" (document-document-type document)))
		  :name (string-downcase
			 (frmt "file-upload-row-~A" (document-document-type document)))
		  (cl-who:str (render-image element document))))))

(defmethod render-input-val ((type (eql :file)) element document
			     &key &allow-other-keys)

  (with-html-string
      (:div :class "col"
	    (:div :class "row"
		  :id (string-downcase
		       (frmt "file-upload-row-~A" (document-document-type document)))
		  :name (string-downcase
			 (frmt "file-upload-row-~A" (document-document-type document)))
		  (cl-who:str (render-image element document)))))
  #|
  (with-html-string
  (:div :class "row"
  (:div :class "col"
  (:input
  :class "file-upload"
  :type "file"
  :onchange (frmt "$(\"#file-~A\").val($(this).val());"
  (getx element :name)))

  (:input :type "text" :class "form-control "
  :name (getx element :name)
  :id (frmt "file-~A" (getx element :name))
  :value (getx document (getx element :name))
  :readonly t))))
  |#)

(defmethod render-input-val ((type (eql :email)) element document
			     &key &allow-other-keys)
  (render-input-val* type element document))

(defmethod render-input-val ((type (eql :number)) element document
			     &key &allow-other-keys)

  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string

	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "number"
		    :step "any"
		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))

	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "number"
		    :step "any"
		    :min (digx element :min)
		    :max (digx element :max)
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or (parameter name)
			(print-document-val
			 type
			 element
			 document
			 :default-value
			 (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :integer)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "number"
		    :step "any"
		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "number"
		    :step "1"
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or (parameter name)
			(print-document-val
			 type
			 element
			 document
			 :default-value
			 (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :date-time)) element document
			     &key &allow-other-keys)

  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:input :class "form-control "
		    :id name
		    :name name
		    :type "datetime-local"
		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))
	(with-html-string
	    (:input :class "form-control date"
		    :id name
		    :name name
		    :type "datetime-local"
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or
		     (parameter name)
		     (print-document-val
		      type
		      element
		      document
		      :default-value
		      (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :date)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:input :class "form-control "
		    :id name
		    :name name
		    :type "date"

		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))
	(with-html-string
	    (:input :class "form-control date"
		    :id name
		    :name name
		    :type "date"
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or
		     (parameter name)
		     (print-document-val
		      type
		      element
		      document
		      :default-value
		      (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :time)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))
    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "time"
		    :value
		    (print-document-val
		     type
		     element
		     document
		     :default-value
		     (document-default-val* element document))
		    :disabled "disabled"))
	(with-html-string
	    (:input :class "form-control"
		    :id name
		    :name name
		    :type "time"
		    :required (if (getx element :key-p)
				  "required")
		    :value
		    (or
		     (parameter name)
		     (print-document-val
		      type
		      element
		      document
		      :default-value
		      (document-default-val* element document))))))))

(defmethod render-input-val ((type (eql :boolean)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name))
	(print-val (getxe document element type)))

    (with-html-string
	(:div :class "form-check"
	      (if (not (digx element :attributes :editable))
		  (cl-who:htm (:div :class "form-check-label"
				    (:input
				     :class "form-check-input"
				     :type "checkbox"
				     :id name
				     :name name
				     :value (getxe
					     document
					     element
					     type
					     :default-value
					     (document-default-val* element document))
				     :checked print-val
				     :aria-label "..."
				     :disabled "disabled")))
		  (cl-who:htm (:div :class "form-check-label"
				    (:input
				     :class "form-check-input"
				     :type "checkbox"
				     :id name
				     :name name
				     :required (if (getx element :key-p)
						   "required")
				     :value (or
					     (parameter name)
					     (getxe
					      document
					      element
					      type
					      :default-value
					      (document-default-val* element document)))
				     :checked print-val
				     :aria-label "..."))))))))

(defmethod render-input-val ((type (eql :css)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-css-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-css-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :html)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-html-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-html-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :java-script)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-js-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-js-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :lambda)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-lisp-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-lisp-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :lisp-code)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-lisp-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-lisp-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :text-blob)) element document
			     &key &allow-other-keys)
  (let ((name (getx element :name)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control wfx-lisp-code"
	     :id name
	     :name name :cols 50 :rows 10
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  type
			  element
			  document))))
	(with-html-string
	    (:textarea
	     :class "form-control"
	     :id name
	     :name name :cols 50 :rows 10
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str
	      (or
	       (parameter name)
	       (print-document-val
		type
		element
		document))))))))

(defmethod render-input-val ((type (eql :value-string-list))
			     element document &key &allow-other-keys)
  (let* ((name (getx element :name))
	 (delimiter (digx element :concrete-type :delimiter)))

    (if (stringp delimiter)
	delimiter
	(setf delimiter (eval delimiter)))

    (if (not (digx element :attributes :editable))
	(with-html-string
	    (:textarea
	     :class "form-control"
	     :id name
	     :name name :cols 20 :rows 3
	     :disabled "disabled"
	     (cl-who:str (print-document-val
			  :value-string-list
			  element
			  document)))
	  (:span
	   (cl-who:str (frmt "Delimit by ~A" (cond ((string-equal delimiter " ")
						    "#\Space")
						   ((string-equal delimiter (frmt "~%"))
						    "#\Return")
						   (t
						    (frmt "~S" delimiter)))))))
	(with-html-string
	    (:textarea
	     :class "form-control"
	     :id name
	     :name name :cols 20 :rows 3
	     :required (if (getx element :key-p)
			   "required")
	     (cl-who:str (or
			  (parameter name)
			  (print-document-val
			   :value-string-list
			   element
			   document))))
	  (:span (cl-who:str
		  (frmt "Delimit by ~A" (cond ((string-equal delimiter " ")
					       "#\Space")
					      ((string-equal delimiter (frmt "~%"))
					       "#\Return")
					      (t
					       (frmt "~S" delimiter))))))))))

