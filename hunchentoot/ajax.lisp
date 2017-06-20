(in-package :cl-Wfx)



(defvar *in-ajax-request* nil)

(defmethod render :around (renderer &key id from-ajax)
  (if from-ajax
      (let ((*in-ajax-request* (list renderer id)))
        (call-next-method))
      (with-html
        (:div :id id
	      (call-next-method)))))

(defun js-render (renderer id &rest args-scripts)
 ;; (break "huh")
  (format nil "ajax_render(~s, ~s, ~s~@[, [~{~a~^,~}]~])"
          (hunchentoot:script-name*)
          renderer
	  id
          args-scripts))

(defun js-render-event-key (source-id event-key renderer id &rest args-scripts)
  (format nil "ajax_render_event_key(event, ~s, ~s, ~s, ~s, ~s~@[, [~{~a~^,~}]~])"
	  source-id
	  event-key
          (hunchentoot:script-name*)
          renderer
	  id
          args-scripts))

#|
(defun js-render-form-values (widget form-name
                              &rest args-scripts)
  (format nil "ajax_render(~s, ~s, get_form_values(~s)~@[.concat([~{~a~^,~}])~])"
          (hunchentoot:script-name*)
          (if (typep widget 'widget)
              (name widget)
              widget)
          form-name
          args-scripts))

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
