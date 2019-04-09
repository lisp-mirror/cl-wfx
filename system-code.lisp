(in-package :cl-wfx)

(add-core-definitions
 '((:data-type
    (:name "lambda"
     :label "Lambda"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :key
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :lambda
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))
   
   (:collection
    (:name "lambdas"
     :label "Lambdas"
     :data-type "lambda")
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
    (:name "package"
     :label "Package"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :key
	     :attributes (:display t :editable t))
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :lisp-code
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))
   
   (:collection
    (:name "packages"
     :label "Packages"
     :data-type "package")
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
    (:name "java-script"
     :label "Java Script"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :key
	     :attributes (:display t :editable t))
      (:name :load-timing
	     :label "Load Timing"
	     :db-type (:type :keyword
			     :complex-type :value-list
			     :values (:header 
				      :footer 
				      ))
	     :attributes (:display t :editable t)
	     :documentation "")
      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :js
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))
   
   (:collection
    (:name "java-scripts"
     :label "Java Scripts"
     :data-type "java-script")
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
    (:name "stylesheet"
     :label "Stylesheet"
     :top-level-p t
     :fields
     ((:name :name 
	     :label "Name"
	     :key-p t
	     :db-type :key
	     :attributes (:display t :editable t))

      (:name :code
	     :label "Code"
	     :key-p nil
	     :db-type :css
	     :attributes (:display t :editable t)))
     :destinations (:core :system :license)))
   
   (:collection
    (:name "stylesheets"
     :label "Stylesheets"
     :data-type "stylesheet")
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

   ))



(defun apply-lambda (name args)
  (let ((lambdax (wfx-fetch-items "lambdas"
				 :test (lambda (item)
					 (equalp (getx item :name) name)))))
    (when lambdax
      (apply (getx lambdax :code) args))))
