(in-package :common-lisp-user)



(defpackage :cl-wfx
  (:use :cl :cl-naive-store)
  
  (:export
   ;;#### common.lisp

   :*system*
   :*context*
   :current-user
   :active-user
   :getx   
   :frmt
   :trim-whitespace
      
   :empty-p
   :ensure-parse-integer
   :parse-date
   :format-date
   :date-diff
   :replace-all
   :frmt-money
   :ensure-num
   :read-no-eval
   :read-eval
   :eval%
   :apply%
   :load%

   :plist-to-value-pairs
   
   ;;#### data.lisp
   :init-license-universe
   :core-store
   :core-collection
   :system-store
   :system-collection
   :license-store
   :license-collection
   :wfx-get-collection
   :wfx-fetch-items
   :wfx-fetch-item
   :wfx-context-fetch-items
   :wfx-context-fetch-item
   
   :sanitize-data-file
   
   ;;????
   :system 

   ;;###system-code.lisp
   :lambda-eval
   :call-lambda
   :apply-lambda

   ;;### Request
   :action-handler
   :parameter
   
   ;;#### context.lisp
   
   :context
   :context-id
   :module
   :context-spec
   :session
   :url
   :cache
   :*lambda-functions*
   :context-log
   
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

   ;;### System
   
   :hunch-system
   :site-url
   :ajax-process
   :ajax-url
   :image-processor
   :image-url
   :default-context
   :request-exclusions
   :action-parameter-allowed-values
   :action-parameters
   :theme

   :start-sys
  
   ;;### Session
   :current-user
   :active-user

   ;;#### license.lisp
   :get-license
   :get-license-entity

   ;;entity.lisp
   :match-context-entities
   
   ;;#### module.lisp
   :get-module
   :get-module-context
   :get-module-short

   ;;user.lisp
   :get-user
   :get-license-user
   :make-user
   :change-user
   :add-user
   
   
   ;;#### context-spec.lisp
   :get-context-spec

   ;;#### hunchentoot/common.lisp
   :hunch-request
   :with-html
   :with-html-string
   
   ;;#### hunchentoot/context.lisp
   :setup-context
   :tea
   :ts
   :tsa
   :custom-render-context
   :render-page
   
   ))


(defpackage :wfx-repl
  (:use :cl :cl-naive-store :cl-wfx))
