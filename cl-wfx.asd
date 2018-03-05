(defsystem "cl-wfx"
  :description "A simple web system framework.."
  :version "0.0.1"
  :author "Phil Marneweck <phil@psychedelic.co.za>"
  :licence "MIT"
  :depends-on ("alexandria" 
	       "cl-ppcre"
	       "ironclad"
	       "bordeaux-threads"
	       "cl-naive-store"
	       "hunchentoot"
	       "cl-who"
	       "split-sequence"
	       "ht-simple-ajax"
	       "cl-json"
	       "cl-smtp"
	       "sandbox"
	      ;; "postoffice"
	       )
  :components ((:file "packages")
               (:file "common" :depends-on ("packages"))	       
	       (:file "system" :depends-on ("common"))
	       (:file "session" :depends-on ("system"))
	       (:file "data" :depends-on ("system"))
	       (:file "script" :depends-on ("system" "data"))
	       
	       (:file "named-list" :depends-on ("data"))

	       (:file "mailer" :depends-on ("data"))
	       
	       (:file "user" :depends-on ("system" "data"))
	       (:file "license" :depends-on ("data"))
	       (:file "entity" :depends-on ("license"))
	       (:file "context" :depends-on ("system"))
	       (:file "context-spec" :depends-on ("data" "script"))
	       (:file "module" :depends-on ("system" "context-spec"))
	       (:file "request" :depends-on ("context" "context-spec" "module"))
	       
	       (:file "data-type-fields" :depends-on ("system" "context-spec"))
	       (:file "hunchentoot/common" :depends-on ("request"))
	       (:file "hunchentoot/ajax")
	       (:file "hunchentoot/system" 
		      :depends-on ("system" "request" "hunchentoot/common"))
	       (:file "hunchentoot/request" :depends-on ("hunchentoot/system" ))
	       (:file "hunchentoot/data-type-fields"
		      :depends-on ("data-type-fields"))
	       (:file "hunchentoot/grid" 
		      :depends-on ("hunchentoot/request"))
	       (:file "hunchentoot/context" 
		      :depends-on ("hunchentoot/grid"))
	       (:file "report" :depends-on ("script" "hunchentoot/context"))
	       (:file "system-load" :depends-on ("request" "hunchentoot/context"))
	       ))

#|
(defsystem cl-wfx
  :name "cl-wfx"
  :version "0.1"
  :depends-on (;;:closer-mop 
	       :monkey-lisp
		  :monkey-html-lisp
	       :alexandria 
	       :ppcre
	       :ironclad :split-sequence :xdb2 
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
	     ;;  (:file "script")
	     ;;  (:file "data")
	     ;;  (:file "data-spec")
	       
	       
	     ;;  (:file "xdb/data-spec")
	     ;;  (:file "xdb/common")
	     ;;  (:file "context-spec")
	    ;;   (:file "xdb/context-spec")
	       
	       
	       (:file "context")
	       (:file "user")
	       #|
	       (:file "license")
	       (:file "module")		       
	       (:file "system")
	       (:file "session")
	       (:file "request")
	       (:file "xdb/entity")
	       (:file "allsorts")
	       (:module "xdb"
			 :serial t
			 :components
			 ((:file "data")
			  (:file "data-fields")
			  (:file "system")))
	       
	       (:module "hunchentoot"
			 :serial t
			 :components
			 ((:file "common")
			  (:file "ajax")
			  (:file "grid")
			  (:file "context")
			  (:file "system")			  
			  (:file "request")
			  ))
	       |#
	     ))




|#
