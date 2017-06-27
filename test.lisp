(in-package :cl-wfx)

(declaim (optimize (debug 3)))


(defvar *hunch-sys* (make-instance 'hunch-system 
				   :system-name "cl-wfx"
				   :port 8888
				   :system-folder "/Development/cl-wfx/"
				   :web-folder "Development/cl-wfx/web/"
			;;	   :system-web-folder "Development/cl-wfx/web/"
			;;	   :system-package *package*
				   :site-url "/cl-wfx/"
				   :ajax-url "/cl-wfx/ajax/"
			;;	   :login-url "login"
			;;	   :debug-errors-p t
				   :message-log-destination *standard-output*
				   :default-context "Data Specs"
				   ))

(defmethod load-context-specs ((system hunch-system) &key &allow-other-keys)
	    
  )

(start-sys *hunch-sys*)


(with-system *hunch-sys*  
 ;; (setup-context nil (get-context-spec "Data Specs") *hunch-sys*)
  )



#|


;;(hunchentoot:start *hunch-sys*)

(with-system *hunch-sys*
  (get-context-spec "Data Specs"))
(with-system *hunch-sys*
  (get-system-collection "Data Spec"))
|#




(defun shitx (fuck)
  (monkey-html-lisp:with-html 
    (:html
   (:body
    (:div (monkey-html-lisp:htm 
	    (format nil "aaah - ~A" "shit")))
    (:div :class "some-class"
     (let ((x (random 10)))
       (if (> x 5)
	   (monkey-html-lisp:htm (:p (:print "arst")))
	   (monkey-html-lisp:htm (:p (:print "arstarst")))
	   )))
    
    (:div :class (if (> (random 10) 5)
		     (monkey-html-lisp:htm "someless5")
		     (monkey-html-lisp:htm "somegreat5")
		     )
     (let ((x (random 10)))
       (if (> x 5)
	   (monkey-html-lisp:htm (:p "arsggggggggt"))
	   (monkey-html-lisp:htm (:p "araaaaaaaaaaast"))
	   )))
    
    (:div :class (if (> (random 10) 5)
		     "shit"
		     "fuck"
		      )
     (:format "~A" "fuck")
     (:div
      (:if (> (random 10) 5)
		      "shit"
		      "fuck"
		      )
      )
     (:div (read-from-string fuck))
     )))))
