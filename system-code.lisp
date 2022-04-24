(in-package :cl-wfx)

(add-core-definitions
 '((:document-type
    (:name "lambda"
     :label "Lambda"

     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :key
       :attributes (:display t :editable t))
      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :lambda
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:collection
    (:name "lambdas"
     :label "Lambdas"
     :document-type "lambda")
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
    (:name "package"
     :label "Package"

     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :key
       :attributes (:display t :editable t))
      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :lisp-code
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:collection
    (:name "packages"
     :label "Packages"
     :document-type "package")
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
    (:name "java-script"
     :label "Java Script"

     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :key
       :attributes (:display t :editable t))
      (:name :load-timing
       :label "Load Timing"
       :concrete-type (:type :keyword
		       :complex-type :value-list
		       :elements (:header
				  :footer))
       :attributes (:display t :editable t)
       :documentation "")
      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :js
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:collection
    (:name "java-scripts"
     :label "Java Scripts"
     :document-type "java-script")
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
    (:name "stylesheet"
     :label "Stylesheet"

     :elements
     ((:name :name
       :label "Name"
       :key-p t
       :concrete-type :key
       :attributes (:display t :editable t))

      (:name :code
       :label "Code"
       :key-p nil
       :concrete-type :css
       :attributes (:display t :editable t)))
     :destinations (:core :system :license)))

   (:collection
    (:name "stylesheets"
     :label "Stylesheets"
     :document-type "stylesheet")
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

(defun apply-lambda (name args)
  (let ((lambdax (wfx-query-data
		  "lambdas"
		  :query (lambda (document)
			   (equalp (getx document :name) name)))))
    (when lambdax
      (apply (getx lambdax :code) args))))
