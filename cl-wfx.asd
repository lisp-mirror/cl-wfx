(defsystem cl-wfx
  :name "cl-wfx"
  :version "0.1"
  :depends-on (;;:closer-mop 
	       :monkey-lisp
		  :monkey-html-lisp
	       :alexandria :ironclad :split-sequence :xdb2 
			   :hunchentoot :ht-simple-ajax :cl-who :cl-json 
			   :parse-number
			   :csv-parser
			   :cl-ftp
			   :dx-pdf
                           :vecto-graphs
			   :lparallel
			   :local-time)
  :serial t
  :components ((:file "package")
	       (:file "common")
	       (:file "script")
	       (:file "data")
	       (:file "data-spec")
	       (:file "xdb/common")
	       (:file "xdb/data-spec")
	       
	       
	       (:file "context")
	       (:file "user")
	       (:file "license")
	       (:file "module")		       
	       (:file "system")
	       (:file "session")
	       (:file "request")
	       
	       (:module "xdb"
			 :serial t
			 :components
			 ((:file "data")
			  (:file "system")))
	       
	       (:module "hunchentoot"
			 :serial t
			 :components
			 ((:file "common")
			  (:file "ajax")
			  (:file "context")
			  (:file "system")			  
			  (:file "request")
			  ))
	     ))




