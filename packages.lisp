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

   
   ;;#### data.lisp
   :core-store
   :core-collection
   :system-store
   :system-collection
   :license-store
   :license-collection
   :wfx-fetch-items
   :wfx-fetch-item
   
   ;;????
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

   ;;#### license.lisp
   :get-license
   :get-license-entity
   
   ;;#### module.lisp
   :get-module
   :get-module-context
   :get-module-short

   ;;user.lisp
   :get-user
   :get-license-user
   
   ;;#### context-spec.lisp
   :get-context-spec

   ;;#### hunchentoot/common.lisp
   :with-html
   :with-html-string
   
   ;;#### hunchentoot/context.lisp
   :setup-context
   ))


