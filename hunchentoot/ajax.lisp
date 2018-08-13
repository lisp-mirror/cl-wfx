(in-package :cl-Wfx)

(defun context-uri ()
  (or (parameter "context-uri")
      (frmt "~A?cs=~A"
	    (hunchentoot:script-name*)
	    (parameter "cs")
	    )
      )
  )

(defvar *in-ajax-request* nil)

(defmethod render :around (renderer &key id from-ajax)
  (if from-ajax
      (let ((*in-ajax-request* (list renderer id)))
        (call-next-method))
      (with-html
        (:div :id id
	      (call-next-method)))))

(defun js-render (renderer id &rest args-scripts)
  (format nil "ajax_render(~s, ~s, ~s~@[, [~{~a~^,~}]~])"
          (context-uri)
          renderer
	  id
          args-scripts))

(defun js-render-event-key (source-id event-key renderer id &rest args-scripts)
  (format nil "ajax_render_event_key(~s, ~s, ~s, ~s, ~s~@[, [~{~a~^,~}]~])"
	  (context-uri)
	  renderer
	  source-id
	  event-key         
	  id
          args-scripts))

(defun js-render-form-values (renderer id form-name
                              &rest args-scripts)
  (format nil "ajax_render(~s, ~s, ~s ,get_form_values(~s)~@[.concat([~{~a~^,~}])~])"
          (context-uri)
	  renderer
	  id
          form-name
          args-scripts))


#|

(defun receive-image ()
  (destructuring-bind (path name application-type)
      (post-parameter "image-upload")
    (declare (ignore application-type))
    (let* ((dot (position #\. name :from-end t))
           (type (and dot
                      (subseq name (1+ dot))))
           (new-name (make-pathname
                      :name (format nil "~a~a~a"
                                    (filter-pathname (subseq name 0 dot))
                                    (random 99999) (get-universal-time))
                      :type type
                      :directory '(:relative "images")))
           (new-path (move-upload path new-name)))
      (when (probe-file new-path)
        new-name))))

(defajax image-upload ()
  (setf (content-type*) "text/plain")
  (let ((path (receive-image)))
    (and path
     (namestring (merge-pathnames path "/insite/")))))

(loop for post-parameter in (hunchentoot:post-parameters*)
            if (equal (car post-parameter) "files")
            collect post-parameter))

(defun js-render-disabled-form-values (widget form-name
                                       &rest args-scripts)
  (format nil "ajax_render(~s, ~s, get_form_values(~s,true)~@[.concat([~{~a~^,~}])~])"
          (hunchentoot:script-name*)
          (if (typep widget 'widget)
              (name widget)
              widget)
          form-name
          args-scripts))

(defun js-render-return-false (widget &rest args-scripts)
  (format nil "~a; return false;"
          (apply #'js-render widget args-scripts)))

(defun js-apply (widget &rest args)
  (format nil "ajax_render(~s, ~s, [~{~a~^,~}].concat(~a))"
          (hunchentoot:script-name*)
          (if (typep widget 'widget)
              (name widget)
              widget)
          (butlast args)
          (car (last args))))

|#
