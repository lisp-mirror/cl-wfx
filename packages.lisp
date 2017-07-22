(in-package :common-lisp-user)

(defpackage :cl-wfx
  (:use :cl :cl-naive-store)
  
  (:export
   ;;#### common.lisp
   
   :current-user
   :active-user
   :getx   
   :frmt
   :trim-whitespace
   :read-no-eval   
   :empty-p
   :ensure-parse-integer
   :parse-date

   ;;#### script.lisp
   
   ;;
   
   :system 
   
   ;;#### context.lisp
   
   :context
   :context-id
   :module
   :context-spec
   :session
   :url
   :cache
   
   ;;#### 
   :load-context-specs
   :load-modules
   
   :init-sys-universe
   :init-universe-definitions
   :add-core-definitions
   :add-system-definitions
   :core-collection
   :system-collection
   :license-collecton
   
   :hunch-system
   ))


