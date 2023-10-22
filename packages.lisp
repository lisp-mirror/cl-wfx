(in-package :common-lisp-user)

(defpackage :cl-wfx
  (:use
   :cl
   :cl-getx
   :cl-naive-store.naive-core
   :cl-naive-store.naive-indexed
   :cl-naive-store.document-types
   :cl-naive-store.definitions
   :cl-naive-store.naive-documents)

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
   :format-universal-date-web
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
   :wfx-universe
   :init-license-universe
   :core-store
   :core-collection
   :system-store
   :system-collection
   :license-store
   :license-collection
   :wfx-get-collection
   :wfx-query-data
   :wfx-query-document
   :wfx-query-context-data
   :wfx-query-context-document

   :sanitize-data-file

   ;;????
   :system

   ;;###system-code.lisp
   :lambda-eval
   :call-lambda
   :apply-lambda

   ;;### Request
   :request-parameter
   :action-handler
   :parameter

   ;;#### context.lisp
   :context-parameter
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
   :session-parameter
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

   ;;### mailer.lisp
   :*mail-data*
   :get-email-account
   :get-entity-email-account
   :send-mail
   :get-email-template
   :send-template-mail-by-description
   :log-email
   :send-template-mail))

(defpackage :wfx-repl
  (:use :cl :cl-naive-store.naive-core
	:cl-naive-store.naive-documents :cl-wfx))

(defpackage :wfx-importer
  (:use :cl :cl-naive-store.naive-core
	:cl-naive-store.naive-documents :cl-wfx))
