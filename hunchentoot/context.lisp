(in-package :cl-wfx)

(defun render-login ()
  (with-html-string
    (:div :class "row"
          (:div :class "card col-6"
                (:img :class "card-image-top"
                      :src  (frmt "~Acor/web/images/~A"
                                  (site-url *system*)
                                  (if (theme-element
                                       (theme *system*) :login-image)
                                      (theme-element
                                       (theme *system*) :login-image)
                                      "logo-login.png")))
                (:div :class "card-block"
                      (:h4 :class "card-title"
                           "Login")
                      (:form :method "post"
                             :action ""
                             (:input :type "hidden" :id "contextid"
                                     :value (context-id *context*))
                             (:div :class "form-group"
                                   (:label :for "email" "Email")
                                   (:input :type "email"
                                           :class "form-control"
                                           :name "email"
                                           :id "email"
                                           :value ""))
                             (:div :class "form-group"
                                   (:label :for "password" "Password")
                                   (:input :type "password"
                                           :class "form-control"
                                           :name "password"
                                           :id "password"
                                           :placeholder "Password"
                                           :value ""))
                             (:button :name "wfxaction"
                                      :class "btn btn-primary"
                                      :type "submit"
                                      :value "login"
                                      "Login")))
                (:div :class "card-footer"
                      (cl-who:str
                       (gethash :login-error (cache *context*))))))))

(defun context-url (spec-name)
  (frmt "~As-wfx?cs=~A" (site-url *system*)
        (string-downcase
         spec-name)))

(defun clear-hash-documents (hash)
  (when hash
    (loop for key being the hash-keys of hash
          do (remhash key hash))))

(defmethod on-success (user)

  (when (current-user)

    (when *session*
      (clear-hash-documents (cache *session*))
      (clear-hash-documents (contexts *session*))
      (setf (user *session*) nil)))

  (dolist (license-code (available-licenses user))

    (when license-code
      (init-license-universe *system* license-code)))

  (let ((active-user (query-document
                      (core-collection "active-users")
                      :query (lambda (document)
                               (equalp (parameter "email")
                                       (digx document :email))))))
    (unless active-user
      (setf active-user (persist-document (core-collection "active-users")
                                          (list :email (digx user :email)
                                                :selected-licenses nil
                                                :selected-entities nil))))
    (setf (user *session*) user)
    (setf (active-session-user *session*) active-user))

  ;;TODO: Handle default page other than cor pages....
  (hunchentoot:redirect (context-url (default-context *system*))))

(defmethod on-failure ()
  (setf (gethash :login-error (cache *context*))
        "User name or password incorrect."))

(defmethod action-handler ((action (eql :login))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (when (and (parameter "email") (parameter "password"))
    (let ((user (validate-user (parameter "email") (parameter "password"))))
      (if user
          (on-success user)
          (on-failure)))))

(defmethod action-handler ((action (eql :logout))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (setf (contexts *session*) nil)
  (setf *session* nil)
  (persist-document (core-collection "active-users") (active-user))
  (hunchentoot:remove-session hunchentoot:*session*)

  (hunchentoot:redirect (frmt "~As-wfx?cs=login" (site-url *system*))))

(defun user-mods ()
  (wfx-query-data
   "modules"
   :query
   (lambda (document)
     (not (string-equal "Core" (digx document :name))))))

(defun mod-menu (mod)
  (if mod
      (digx (first (digx mod :menu)) :menu-documents)))

(defun system-menu ()
  (let ((sys-mod (query-document
                  (core-collection "modules")
                  :query (lambda (document)
                           (string-equal "Core" (digx document :name))))))
    (if sys-mod
        (digx (first (digx sys-mod :menu)) :menu-documents))))

(defun accessible-entity (entity accessible-entities)
  (if (find entity accessible-entities)
      (return-from accessible-entity entity)
      (dolist (entity (digx entity :children))
        (if (find entity accessible-entities)
            (return-from accessible-entity entity)
            (accessible-entity entity accessible-entities)))))

(defun accessible-entity-roots (accessible-entities license-code)
  (let ((roots))
    (dolist (entity (query-data
                     (license-collection license-code "entities")))
      (when (digx entity :root-p)
        (when (accessible-entity entity accessible-entities)
          (pushnew entity roots))))
    roots))

(defun render-entity-check (entity level accessible-entities)
  (with-html-string
    (:div :class "row"
          (:div :class "col"
                (:div :class "form-check"
                      (:div :class "form-check-label"
                            (dotimes (i level)
                              (cl-who:htm "&nbsp;"))

                            (if (find entity accessible-entities)
                                (if (find (document-hash entity)
                                          (digx (active-user)
                                                :selected-entities)
                                          :test #'equalp)
                                    (cl-who:htm
                                     (:input :class "form-check-input"
                                             :type "checkbox"
                                             :name "tree-entity-id"
                                             :value (document-hash entity)
                                             :checked ""))
                                    (cl-who:htm
                                     (:input :class "form-check-input"
                                             :type "checkbox"
                                             :name "tree-entity-id"
                                             :value (document-hash entity))))
                                (cl-who:htm
                                 (:input :class "form-check-input"
                                         :type "checkbox"
                                         :disabled "")))
                            (cl-who:str (getx entity :name))))))

    (if (digx entity :children)
        (dolist (entity (digx entity :children))
          (cl-who:str (render-entity-check entity
                                           (+ level 1)
                                           accessible-entities))))))

(defun render-entity-tree (license-code accessible-entities)
  (with-html-string
    (:form

     (dolist (entity (accessible-entity-roots
                      accessible-entities license-code))
       (when (digx entity :root-p)
         (cl-who:str (render-entity-check entity 0 accessible-entities))))

     (:div :class "row"
           (:div :class "col"
                 (:button
                  :name "set-entities"
                  :type "submit"
                  :formmethod "post"
                  :class "btn btn-outline-success"
                  :aria-pressed "false"
                  :value "set-entities"
                  "Set Entities"))))))

(defmethod action-handler ((action (eql :set-entities))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (setf (getf (document-elements (active-user)) :selected-entities) nil)

  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "tree-entity-id")
      (dolist (license-code (digx (active-user) :selected-licenses))

        (dolist (entity (available-entities license-code))

          (when (string-equal (frmt "~A" (document-hash entity)) (cdr parameter))
            (pushnew (document-hash entity)
                     (getx (active-user) :selected-entities))
            (persist-document (core-collection "active-users") (active-user))))))))

(defun render-licence-codes ()
  (with-html-string
    (dolist (code (available-licenses))
      (cl-who:htm
       (:div :class "row"
             (:div :class "form-check"
                   (:div :class "form-check-label"
                         (if (find code (digx (active-user) :selected-licenses)
                                   :test #'string-equal)
                             (cl-who:htm
                              (:input :class "form-check-input" :type "checkbox"
                                      :name "license-id"
                                      :value code
                                      :checked ""))
                             (cl-who:htm
                              (:input :class "form-check-input" :type "checkbox"
                                      :name "license-id" :value
                                      (cl-who:str code))))
                         (cl-who:str code))))))))

(defun context-access-p (context)
  (let ((access-p))

    (when (and (active-user)
               (digx (active-user) :selected-licenses))

      (cond ((getx (current-user) :super-user-p)
             (setf access-p t))
            (t (dolist (lic (digx (active-user)
                                  :selected-licenses))
                 (when (license-user lic)
                   (dolist (permission
                            (getx (license-user lic)
                                  :permissions))

                     (when (equalp context
                                   (digx permission
                                         :context-spec))

                       (setf access-p t))))))))
    access-p))

(defun render-user-admin-menu ()
  (with-html-string
    (:div :class "nav-document dropdown"

          (:a :class
              (concatenate
               'string
               "nav-link dropdown-toggle font-weight-bold "
               (theme-element-attribute
                (theme *system*)
                :nav-toggler-right
                :text-color-class))
              :href ""
              :id "userDropdown"
              :data-toggle "dropdown"
              :aria-haspopup="true"
              :aria-expanded "false"
              (if (current-user)
                  (cl-who:str (frmt "~A&nbsp&nbsp&nbsp&nbsp&nbsp" (digx (current-user) :email)))))

          (:div :class "dropdown-menu"
                :aria-labelledby "userDropdown"
                (let ((sys-mod
                        (query-document
                         (core-collection "modules")
                         :query (lambda (document)
                                  (string-equal
                                   "Core"
                                   (digx document :name))))))

                  (dolist (menu (digx sys-mod :menu))
                    (when (equalp (digx menu :name) "System")
                      (dolist (document (digx menu :menu-documents))
                        (let ((parameters))
                          (dolist (param (digx document :context-parameters))
                            (if parameters
                                (setf parameters
                                      (frmt "~A&~A=~A"
                                            parameters
                                            (digx param :name)
                                            (digx param :value)))
                                (setf parameters
                                      (frmt "~A=~A"
                                            (digx param :name)
                                            (digx param :value)))))

                          (when (or (equalp (digx document :name) "Logout")
                                    (context-access-p
                                     (digx document :context-spec)))

                            (when (system-context-p (digx document :context-spec))
                              (when (equalp (digx document :context-spec) :divider)
                                (cl-who:htm (:div :class "dropdown-divider")))

                              (unless (equalp (digx document :context-spec) :divider)
                                (cl-who:htm
                                 (:a :class "dropdown-item"
                                     :href
                                     (if parameters
                                         (frmt "~A&~A"
                                               (context-url
                                                (digx document :context-spec :name))
                                               parameters)
                                         (context-url
                                          (digx document :context-spec :name)))
                                     (:i        :class (frmt "fa far fab ~A"
                                                             (digx document :context-spec :icon))
                                                :style "width:25px;")
                                     " "
                                     (cl-who:str (digx document :name))))))))))))))))

(defun render-header-display (text)
  (with-html-string
    (:span :class
           (concatenate
            'string
            "navbar-text mr-auto font-weight-bold "
            (tea
             (theme *system*)
             :nav-toggler-left
             :text-color-class))

           (cl-who:str
            text))))

(defun data-menu (menu-documents)
  (let ((documents))
    (dolist (document menu-documents)

      (when (and
             (digx document :context-spec)
             (context-access-p
              (digx document :context-spec))
             (document-collection (digx document :context-spec))
             (not (getx (digx document :context-spec) :report)))
        (pushnew document documents)))
    (reverse documents)))

(defun report-menu (menu-documents)
  (let ((documents))
    (dolist (document menu-documents)
      (when (and
             (digx document :context-spec)
             (context-access-p
              (digx document :context-spec))
             (getx (digx document :context-spec) :report))
        (pushnew document documents)))
    (reverse documents)))

(defun other-menu (menu-documents)
  (let ((documents))
    (dolist (document menu-documents)
      (when (and
             (digx document :context-spec)
             (context-access-p
              (digx document :context-spec))
             (and (not (getx (digx document :context-spec) :report))
                  (not (getx (digx document :context-spec) :collection))))
        (pushnew document documents)))
    (reverse documents)))

(defun render-menu-document-text (document)

  (with-html-string
    (:a :class "btn btn-light text-dark w-100 text-left"
        :role "button"

        :href (context-url
               (digx document :context-spec :name))

        (cl-who:str (digx document :name)))))

;;When super-user has selected the system license then exclude grids which dont have collections
;; that have a system/core function ie destination
(defun system-context-p (context)
  (if (equalp context :divider)
      t
      (if (find (name *system*) (getx (active-user) :selected-licenses) :test 'equalp)
          (if (getx context :collection)
              (or (find :core
                        (getf (find-collection-def *system* (getx  context :collection))
                              :destinations)
                        :test 'equalp)
                  (find :system
                        (getf (find-collection-def *system* (getx  context :collection))
                              :destinations)
                        :test 'equalp)))
          t)))

(defun render-menu-document (document)
  (with-html-string
    (:tr
     (:td
      :class "bg-light"
      :style "width:45px;"
      (:a

       :class "btn btn-light"
       :style "width:45px;"
       :href (context-url
              (digx document :context-spec :name))
       (:i

        :class (frmt "fa far ~A"
                     (digx document :context-spec
                           :icon))))
      (:td
       :class "bg-light"
       (cl-who:str (render-menu-document-text document)))))))

(defun render-left-user-menu ()
  (with-html-string
    (:div
     (:nav :id "sidebar"
           :class "sticky-top bg-header  d-print-none"
           :style "box-shadow: 0px 2px 5px;"

           (:div :class "sidebar-header text-center"
                 (:img
                  :style "height:85px;width:85"
                  :src (frmt "~Acor/web/images/~A"
                             (site-url *system*)
                             (tea
                              (theme *system*)
                              :main-nav-logo
                              :src)))
                 (when (empty-p (tea
                                 (theme *system*)
                                 :main-nav-logo
                                 :src))
                   (cl-who:htm
                    (:h3 (:strong (cl-who:str (string-upcase (name *system*))))))))

           (:ul :class "list-unstyled components"

                (:a :class "nav-link bg-header text-dark border"

                    :data-toggle "collapse"
                    :href "#context-menu"
                    (:strong "License Context"))
                (:div :id "context-menu"
                      :class "collapse container"
                      (cl-who:str (render-right-menu)))

                (dolist (mod (user-mods))

                  (dolist (menu (digx mod :menu))

                    (cl-who:htm
                     (:a :class "nav-link bg-header text-dark border-bottom border-right border-left"
                         :data-toggle "collapse"
                         :href "#data-menu"
                         (:strong "Data")))

                    (cl-who:htm
                     (:div :id "data-menu" :class "collapse.show"
                           (:ul :class "nav flex-column ml"
                                (:table :class "table table-sm"

                                        (dolist (document (data-menu (digx menu :menu-documents)))

                                          (when (system-context-p (digx document :context-spec))

                                            (cl-who:str
                                             (render-menu-document document))))))))
                    (cl-who:htm
                     (:a :class "nav-link bg-header text-dark border-bottom border-right border-left"
                         :data-toggle "collapse"
                         :href "#report-menu"
                         (:strong "Reports"))
                     (:div :id "report-menu" :class "collapse"
                           (:ul :class "nav flex-column ml"
                                (:table :class "table table-sm"
                                        (dolist (document (report-menu (digx menu :menu-documents)))
                                          (when (system-context-p (digx document :context-spec))
                                            (cl-who:str
                                             (render-menu-document document))))))))
                    (cl-who:htm
                     (:a :class "nav-link bg-header text-dark border-bottom border-right border-left"
                         :data-toggle "collapse"
                         :href "#other-menu"
                         (:strong "Other"))
                     (:div :id "other-menu" :class "collapse"
                           (:ul :class "nav flex-column ml"
                                (:table :class "table table-sm"
                                        (dolist (document (other-menu (digx menu :menu-documents)))
                                          (cl-who:str (render-menu-document document))))))))))))))

(defun render-license-dropdown (list &key (select-prompt "Select a License"))
  (let ((selected-value (or (parameter "license-select")
                            (car (getx (active-user) :selected-licenses))
                            select-prompt)))

    (with-html-string
      (:div :class "bt-group dropdown w-100"
            (:input :type "hidden" :class "selected-value"
                    :name "license-selected"
                    :value (or selected-value
                               ""))
            (:button :class "btn dropdown-toggle w-100"
                     :type "button"
                     :data-toggle "dropdown"
                     :aria-haspopup "true" :aria-expanded "false"
                     (cl-who:str (or selected-value "")))

            (when (getx (current-user) :super-user-p)
              (setf list (append (list (string-capitalize (name (system-store)))) list)))

            (:div :class "dropdown-menu w-100"
                  (dolist (option list)
                    (cl-who:htm
                     (:span :class "dropdown-item"
                            :onclick (js-render-form-values
                                      "cl-wfx:ajax-license-select"
                                      "entities-selection"
                                      "license-selection"
                                      (js-pair "action"
                                               "license-select-action")
                                      (js-pair "select-action-value"
                                               option))

                            (:input :id "select-action"
                                    :type "hidden"
                                    :value option)
                            (cl-who:str option)))))))))

(defun ajax-license-select (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))

  (when (and (parameter "license-selected")
             (not (equalp (parameter "license-selected") "Select a License")))
    ;;Set license for session
    (setf (getf (document-elements (active-user)) :selected-licenses) nil)
    (setf (getx (active-user) :selected-entities) nil)

    (setf (getx (active-user) :selected-licenses)
          (list (parameter "select-action-value")))

    (init-definitions (universe *system*)
                      :license (parameter "select-action-value")
                      (data-definitions *system*))
    (setf (active-session-user *session*)
          (persist-document (core-collection "active-users") (active-user))))

  (with-html-string
    (:div :class "col"
          (:div :class "row bg-light"
                (:div :class "col bg-light font-weight-bold"
                      "Accessible Entities"))

          (dolist (license-code (digx (active-user)
                                      :selected-licenses))
            (when (or (license-user license-code)
                      (getx (current-user) :super-user-p))
              (cl-who:str
               (render-entity-tree
                license-code
                (available-entities license-code))))))))

(defmethod action-handler ((action (eql :set-licenses))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (setf (getf (document-elements (active-user)) :selected-licenses) nil)
  (setf (getx (active-user) :selected-entities) nil)

  (dolist (parameter (hunchentoot:post-parameters*))
    (when (equalp (car parameter) "license-id")
      (pushnew (cdr parameter)
               (getx (active-user) :selected-licenses)
               :test #'string-equal)

      (init-definitions (universe *system*)
                        :license (cdr parameter)
                        (data-definitions *system*))

      (persist-document (core-collection "active-users") (active-user)))))

(defun render-right-menu ()
  (with-html-string
    (:form
     :id "license-selection"
     (:div :class "col nav-document  bg-light font-weight-bold"
           "Accessible Licenses")
     (:div :class "row"
           (:div :class "col"
                 (cl-who:str (render-license-dropdown
                              (available-licenses)))))
     (:div :class "row" :id "entities-selection"
           (:div :class "col"
                 (:div :class "row"

                       (:div :class "col bg-light font-weight-bold"
                             "Accessible Entities"))
                 (:div :class "row"
                       (:div :class "col"
                             (let ((license-code
                                     (car (getf (document-values (active-user))
                                                :selected-licenses))))
                               (when license-code
                                 (when (getf (document-values (active-user))
                                             :selected-entities)
                                   (cl-who:str
                                    (render-entity-tree
                                     license-code
                                     (available-entities license-code)))))))))))))

(defun theme-element (theme element)
  (digx theme element))

(defun theme-element-attribute (theme element attribute)
  (digx theme element attribute))

(defun tea (theme element attribute &optional default)
  (let ((attribute
          (theme-element-attribute theme element attribute)))
    (if attribute
        attribute
        default)))

(defun theme-style (theme element)
  (let ((style-string ""))
    (dolist (style (digx theme element :style))
      (setf style-string
            (concatenate 'string style-string
                         (frmt "~A: ~A;" (first style) (second style)))))
    style-string))

(defun theme-attribute-style (theme element attribute)
  (let ((style-string ""))
    (dolist (style (digx (theme-element-attribute theme element attribute) :style))
      (setf style-string
            (concatenate 'string style-string
                         (frmt "~A: ~A;" (first style) (second style)))))
    style-string))

(defun ts (theme element &optional default)
  (let ((style
          (theme-style theme element)))
    (if (not (empty-p style))
        style
        default)))

(defun tsa (theme element attribute &optional default)
  (let ((style
          (theme-attribute-style theme element attribute)))
    (if (not (empty-p style))
        style
        default)))

(defgeneric page-css (system &key &allow-other-keys))

(defmethod page-css ((system hunch-system) &key &allow-other-keys))

(defmethod page-css :around ((system hunch-system) &key &allow-other-keys)

  (with-html-string

    (:link :rel "stylesheet"
           :href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"
           :integrity "sha384-Smlep5jCw/wG7hdkwQ/Z5nLIefveQRIY9nfy6xoR1uRYBtpZgI6339F5dgvm/e9B"
           :crossorigin "anonymous")

    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/css/fileinput.min.css"
           :media "all"
           :type "text/css")

    (:link :rel "stylesheet"
           :href "https://use.fontawesome.com/releases/v5.6.0/css/all.css"
           :integrity "sha384-aOkxzJ5uQz7WBObEZcHvV5JvRW3TUc2rNPA7pe3AwnsUohiw1Vj2Rgx2KSOkF5+h"
           :crossorigin "anonymous")

    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/css/bootstrap-datepicker3.standalone.min.css")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/codemirror.js")
    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/codemirror.css")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/comment/comment.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/comment/continuecomment.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/mode/javascript/javascript.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/mode/xml/xml.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/mode/htmlmixed/htmlmixed.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/mode/css/css.js")

    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/hint/show-hint.css")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/hint/show-hint.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/hint/css-hint.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/hint/javascript-hint.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/mode/commonlisp/commonlisp.js")

    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/edit/closebrackets.js")
    (:script
     :src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.44.0/addon/edit/matchbrackets.js")

    (:link :rel "stylesheet"
           :href (frmt "~Aweb/cl-wfx.css" (site-url system)))

    (cl-who:str (call-next-method))))

(defgeneric page-header-js (system &key &allow-other-keys))

(defmethod page-header-js ((system hunch-system) &key &allow-other-keys))

(defmethod page-header-js :around ((system hunch-system) &key &allow-other-keys)
  (with-html-string
    (:script
     :type "text/javascript"
     :src "https://code.jquery.com/jquery-3.3.1.min.js"
     :integrity "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8="
     :crossorigin "anonymous")

    (:script
     :type "text/javascript"
     :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js"
     :ingegrity "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49"
     :crossorigin "anonymous")

    (:script
     :type "text/javascript"
     :src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.min.js"
     :ingegrity "sha384-o+RDsa0aLu++PJvFqy8fFScvbHFLtbvScb8AjopnFD+iEQ7wo/CG0xlczd+2O/em"
     :crossorigin "anonymous")

    (:script
     :type "text/javascript"
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/js/fileinput.min.js")

    (:script
     :type "text/javascript"
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/themes/fa/theme.min.js")
    (:script
     :type "text/javascript"
     :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.6.4/js/bootstrap-datepicker.min.js")

    (:script
     :type "text/javascript"
     :src (frmt "~Aweb/cl-wfx.js" (site-url system)))

    (cl-who:str (call-next-method))))

(defgeneric page-footer-js (system &key &allow-other-keys))

(defmethod page-footer-js ((system hunch-system) &key &allow-other-keys))

(defmethod page-footer-js :around ((system hunch-system) &key &allow-other-keys)

  (with-html-string
    (:script :type "text/javascript"
             (cl-who:str
              "prep_dropdowns();"))

    (:script :type "text/javascript"
             (cl-who:str
              "prep_auto_completes();"))

    (:script :type "text/javascript"
             (cl-who:str
              "//prep_codemiror();"))

    (:script :type "text/javascript"
             (cl-who:str
              "prep_file_upload();"))

    (:script :type "text/javascript"
             (cl-who:str
              "prep_expands();"))

    (:script :type "text/javascript"
             (cl-who:str
              "$(document).ready(function () {

    $('#sidebarCollapse').on('click', function () {
        $('#sidebar').toggleClass('active');
    });

});"))

    (:script :type "text/javascript"
             (cl-who:str "$(document).ready(function(){

    var code = $(\".wfx-lisp-code\")[0];

    if(code){
        var editor = CodeMirror.fromTextArea(code, {
             lineNumbers: true,
                smartIndent: true,
                autoCloseBrackets: true,
                showTrailingSpace: true,
                matchBrackets: true,
                mode: \"text/x-common-lisp\"
        });
        editor.display.wrapper.style.fontSize = \"12px\";
        editor.refresh();

        function updateTextArea() {
            editor.save();
        }

        editor.on('change', updateTextArea);
    }
});"))

    (:script :type "text/javascript"
             (cl-who:str "$(document).ready(function(){

    var code = $(\".wfx-js-code\")[0];

    if(code){
        var editor = CodeMirror.fromTextArea(code, {
             lineNumbers: true,
                smartIndent: true,
                autoCloseBrackets: true,
                showTrailingSpace: true,
                matchBrackets: true,
                mode: \"text/javascript\"
        });
        editor.display.wrapper.style.fontSize = \"12px\";
        editor.refresh();

        function updateTextArea() {
            editor.save();
        }

        editor.on('change', updateTextArea);
    }
});"))

    (:script :type "text/javascript"
             (cl-who:str "$(document).ready(function(){

    var code = $(\".wfx-css-code\")[0];

    if(code){
        var editor = CodeMirror.fromTextArea(code, {
             lineNumbers: true,
                smartIndent: true,
                autoCloseBrackets: true,
                showTrailingSpace: true,
                matchBrackets: true,
                mode: \"text/css\"
        });
        editor.display.wrapper.style.fontSize = \"12px\";
        editor.refresh();

        function updateTextArea() {
            editor.save();
        }

        editor.on('change', updateTextArea);
    }
});"))

    (:script :type "text/javascript"
             (cl-who:str "$(document).ready(function(){

    var code = $(\".wfx-html-code\")[0];

    if(code){

        var editor = CodeMirror.fromTextArea(code, {
             lineNumbers: true,
                smartIndent: true,
                autoCloseBrackets: true,
                showTrailingSpace: true,
                matchBrackets: true,
                mode: \"text/html\"
        });
        editor.display.wrapper.style.fontSize = \"12px\";
        editor.refresh();

        function updateTextArea() {
            editor.save();
        }

        editor.on('change', updateTextArea);
    }
});"))

    (:script :type "text/javascript"
             (cl-who:str
              (frmt
               "function ajax_call(func, callback, args, widget_args) {

    var uri = '~Aajax/' + encodeURIComponent(func);
    var post_parameters = '&contextid=' + contextid.value;
    var i;
    if (args.length > 0) {
        uri += '?';
        for (i = 0; i < args.length; ++i) {
            if (i > 0)
                uri += '&';
            uri += 'arg' + i + '=' + encodeURIComponent(args[i]);
        }
    }

    if (widget_args && widget_args.length > 0) {
        for (i = 0; i < widget_args.length; ++i) {
            var arg = widget_args[i];
            post_parameters += '&' + encodeURIComponent(arg[0]) + '='
                + encodeURIComponent(arg[1]);
        }
    }

    fetchURI(uri, callback, post_parameters);

}" (site-url system))))

    (cl-who:str (call-next-method))))

(defgeneric render-page (system body &key menu-p header-css header-js
                         footer-js
                         &allow-other-keys))

(defmethod render-page ((system hunch-system) body
                        &key menu-p header-css header-js
                        footer-js
                        &allow-other-keys)
  (with-html-string
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, shrink-to-fit=no")

      (cl-who:str (page-css system))
      (cl-who:str header-css)
      (cl-who:str (page-header-js system))
      (cl-who:str header-js))

     (:body

      (:input :type "hidden" :id "contextid" :value (context-id *context*))

      (if (not menu-p)
          (cl-who:htm
           (:div :class "container"
                 (cl-who:str body)))
          (cl-who:htm
           (unless (active-user)
             (hunchentoot:redirect (frmt "~As-wfx?cs=login" (site-url system))))
           (:div :class "wrapper"
                 (cl-who:str (render-left-user-menu))

                 (:div
                  :id "content"
                  :class "container-fluid"

                  (:nav
                   :class "navbar sticky-top d-print-none justify-content-between bg-white"
                   :style
                   (if (tea (theme system) :main-nav-bar :bg-image)
                       (frmt "background-image: url(~Acor/web/images/~A);~A"
                             (site-url system)
                             (tea (theme system) :main-nav-bar :bg-image)
                             (ts (theme system) :main-nav-bar))
                       (ts (theme system) :main-nav-bar
                           "background-size: cover;box-shadow: 0px 1px 2px"))

                   (if (current-user)
                       (cl-who:htm
                        (:button :id "sidebarCollapse"
                                 :class
                                 (concatenate
                                  'string
                                  "navbar-toggler navbar-toggler-left "
                                  (tea
                                   (theme system)
                                   :nav-toggler-left
                                   :text-color-class))

                                 :type "button btn-small"
                                 :aria-label "Toggle application menu"
                                 "&#9776;")))

                   (:a :class "navbar-brand" :href "#"
                       (unless (empty-p (tea
                                         (theme system)
                                         :main-nav-bar
                                         :brand-image))
                         (cl-who:htm
                          (:img
                           :style (ts (theme system) :main-nav-logo
                                      "height:50px;")
                           :src (frmt "~Acor/web/images/~A"
                                      (site-url system)
                                      (tea
                                       (theme system)
                                       :main-nav-bar
                                       :brand-image)))))
                       (name system))

                   (cl-who:str (render-header-display ""))

                   (cl-who:str (render-user-admin-menu)))

                  (:div :class "row"

                        (:div :class "col" :id "wfx-page-body"
                              (:br)
                              (cl-who:str body)))
                  (:br)
                  (:div :class "row"
                        (:div :class "col"
                              (:a :class "btn btn-light text-dark bg-header border border-light rounded w-100 d-print-none"
                                  :data-toggle "collapse"
                                  :href "#debug-info"
                                  :onclick (js-render "cl-wfx:ajax-show-debug" "debug-info-display")
                                  (:strong "Debug Info"))

                              (:div :id "debug-info"
                                    :class "collapse d-print-none"
                                    (:br)
                                    (:div :id "debug-info-display"))))))))

      (cl-who:str (page-footer-js system))
      (cl-who:str footer-js)))))

(defgeneric render-clean-page (system body &key menu-p header-css header-js
                               footer-js
                               &allow-other-keys))

(defmethod render-clean-page ((system hunch-system) body
                              &key header-css header-js
                              footer-js
                              &allow-other-keys)
  (with-html-string
    (:html
     (:head
      (:meta :charset "utf-8")
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1, shrink-to-fit=no")

      (:link :rel "stylesheet"
             :href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/css/bootstrap.min.css"
             :integrity "sha384-Smlep5jCw/wG7hdkwQ/Z5nLIefveQRIY9nfy6xoR1uRYBtpZgI6339F5dgvm/e9B"
             :crossorigin "anonymous")

      (:link :rel "stylesheet"
             :href "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/css/fileinput.min.css"
             :media "all"
             :type "text/css")

      (:link :rel "stylesheet"
             :href "https://use.fontawesome.com/releases/v5.6.0/css/all.css"
             :integrity "sha384-aOkxzJ5uQz7WBObEZcHvV5JvRW3TUc2rNPA7pe3AwnsUohiw1Vj2Rgx2KSOkF5+h"
             :crossorigin "anonymous")

      (:script
       :type "text/javascript"
       :src "https://code.jquery.com/jquery-3.3.1.min.js"
       :integrity "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8="
       :crossorigin "anonymous")

      (:script
       :type "text/javascript"
       :src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.2/js/bootstrap.min.js"
       :ingegrity "sha384-o+RDsa0aLu++PJvFqy8fFScvbHFLtbvScb8AjopnFD+iEQ7wo/CG0xlczd+2O/em"
       :crossorigin "anonymous")

      (:script
       :type "text/javascript"
       :src "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-fileinput/4.4.8/themes/fa/theme.min.js")

      (cl-who:str (and header-css ""))
      (cl-who:str (and header-js "")))

     (:body
      (:input :type "hidden" :id "contextid" :value (context-id *context*))

      (:div :class "container"
            (cl-who:str body))

      (cl-who:str (and footer-js ""))))))

(defun check-user-access ()
  (unless (current-user)
    (hunchentoot:redirect (frmt "~As-wfx?cs=login" (site-url *system*)))))

(defun describe% (object)
  (let* ((stream (make-string-output-stream)))
    (describe object stream)
    (split-sequence:split-sequence
     #\newline
     (get-output-stream-string
      stream))))

(defun ajax-render-repl (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))

  (with-html-string
    (:div :class "row"
          (:div :class "col"
                (:div :class "card"
                      (:div :class "card-block"
                            (:h4 :class "card-title"
                                 "Lisp Code")
                            (:div :class "form-group"
                                  (:label :for "repl-code" "Code")
                                  (:textarea
                                   :class "form-control wfx-lisp-code"
                                   :rows 20
                                   :name "repl-code"
                                   :id "repl-code"
                                   (cl-who:str
                                    (or (parameter "repl-code") ""))))

                            (:button :name "wfxaction"
                                     :class "btn btn-primary"
                                     :type "submit"
                                     :value "eval-repl"
                                     :onclick (frmt "~A;~A"
                                                    (js-render-form-values
                                                     "cl-wfx:ajax-render-repl"
                                                     "wfx-page-body"
                                                     "wfx-page-body"
                                                     (js-pair "wfxaction"
                                                              "eval-repl"))

                                                    (js-render "cl-wfx:ajax-show-debug"
                                                               "debug-info-display"))
                                     "run"))
                      (:div :class "card-footer"
                            (:div (cl-who:esc (frmt "~S" (gethash :repl-result (cache *context*)))))))))))

(defun ajax-show-debug (&key id from-ajax)
  (declare (ignore from-ajax))
  (with-html-string
    (:div :class "card-columns"
          :id id
          (:div :class "card"
                (:h5 :class "card-header"
                     "Parameters")
                (:div :class "card-body"
                      (:p :class "card-text"

                          (cl-who:str (gethash :get-parameters (cache *context*))))
                      (:p :class "card-text"

                          (cl-who:str (gethash :post-parameters (cache *context*))))))
          (:div :class "card"
                (:h5 :class "card-header"
                     "Context Log")
                (:div :class "card-body"

                      (:p :class "card-text"
                          (cl-who:str (gethash :context-log (cache *context*))))))
          (:div :class "card"
                (:h5 :class "card-header"
                     "Context Cache")
                (:div :class "card-body"

                      (let ((count 0))
                        (maphash (lambda (key value)
                                   (incf count)
                                   (unless (or (equalp key :context-log)
                                               (equalp key :get-parameters)
                                               (equalp key :post-parameters)
                                               (equalp key :debug-error)
                                               (equalp key :debug-results)
                                               (equalp key :debug-backtrace)
                                               (equalp key :debug-log))
                                     (cl-who:htm
                                      (:div :class "row"
                                            (:div :class "col"
                                                  (:a ;;:class "btn"
                                                   :data-toggle "collapse"
                                                   :href (frmt "#collapseCache-~A" count)
                                                   :role "button"
                                                   :aria-expanded "false"
                                                   :aria-controls (frmt "collapseCache-~A" count)

                                                   (:strong
                                                    (cl-who:esc
                                                     (frmt "~S" key))))))
                                      (:div :class "row collapse"
                                            :id (frmt "collapseCache-~A" count)
                                            (:div :class "col"
                                                  (cl-who:esc (frmt "~S" value)))))))

                                 (cache *context*)))))

          (:div :class "card"
                (:h5 :class "card-header"
                     "Error")
                (:div :class "card-body"

                      (:p :class "card-text"
                          (if (gethash :debug-error (cache *context*))
                              (cl-who:esc
                               (frmt "~S" (describe% (gethash :debug-error (cache *context*)))))))))

          (:div :class "card"
                (:h5 :class "card-header"
                     "Results")
                (:div :class "card-body"

                      (:div :class "row"
                            (:div :class :col
                                  (let ((more-results (gethash :debug-results (cache *context*)))
                                        (count 0))

                                    (dolist (result more-results)
                                      (incf count)
                                      (cl-who:htm
                                       (:div :class "row"
                                             (:div :class "col"
                                                   (:a ;;:class "btn"
                                                    :data-toggle "collapse"
                                                    :href (frmt "#collapseMore-~A" count)
                                                    :role "button"
                                                    :aria-expanded "false"
                                                    :aria-controls (frmt "collapseMore-~A" count)

                                                    (:strong
                                                     (cl-who:esc
                                                      (frmt "~S" result))))))

                                       (:div :class "row collapse"
                                             :id (frmt "collapseMore-~A" count)

                                             (:div :class "col bg-light"
                                                   (cond ((and (symbolp result)
                                                               (fboundp result))

                                                          (let* ((stream (make-string-output-stream))
                                                                 ;;(x (describe result stream))
                                                                 (splits (split-sequence:split-sequence

                                                                          #\newline
                                                                          (get-output-stream-string
                                                                           stream))))

                                                            (dolist (split splits)

                                                              (cl-who:htm
                                                               (:br)
                                                               (cl-who:esc split)))))
                                                         (t
                                                          (pprint result))))))))))))

          (:div :class "card"
                (:h5 :class "card-header"
                     "Backtrace")
                (:div :class "card-body"

                      (:div :class "row"
                            (:div :class :col
                                  (let ((backtrace (gethash :debug-backtrace (cache *context*)))
                                        (count 0)
                                        (limit-trace nil))

                                    (dolist (trace backtrace)

                                      (unless limit-trace
                                        (incf count)
                                        (cl-who:htm
                                         (:div :class "row"
                                               (:div :class "col"
                                                     (:a ;;:class "btn"
                                                      :data-toggle "collapse"
                                                      :href (frmt "#collapseTrace-~A" count)
                                                      :role "button"
                                                      :aria-expanded "false"
                                                      :aria-controls (frmt "collapseTrace-~A" count)

                                                      (:strong
                                                       (cl-who:esc
                                                        (frmt "~S" (first trace)))))))
                                         (dolist (trace-element (cdr trace))
                                           (cl-who:htm
                                            (:div :class "row collapse"
                                                  :id (frmt "collapseTrace-~A" count)
                                                  (:div :class "col bg-white"
                                                        (cl-who:esc (frmt "~A" trace-element))))))
                                         (when (or (string-equal (frmt "~S" (first trace))
                                                                 "CL-WFX::READ-EVAL")
                                                   (string-equal (frmt "~S" (first trace))
                                                                 "CL-WFX::EVAL%")
                                                   (string-equal (frmt "~S" (first trace))
                                                                 "CL-WFX::FUNCALL%")
                                                   (string-equal (frmt "~S" (first trace))
                                                                 "CL-WFX::APPLY%"))
                                           (setf limit-trace t)))))))))))))

(defmethod action-handler ((action (eql :eval-repl))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (when (and (parameter "wfxaction")
             (parameter "repl-code")
             (not (empty-p (parameter "repl-code"))))

    (multiple-value-bind (result more-results error)
        (eval%
         (parameter "repl-code")
         :package-name :wfx-repl)

      ;;  (break "~S~%~S" result more-results)
      (setf (gethash :repl-result (cache *context*))
            (or result
                (and error (getf error :error))))
      (setf (gethash :repl-more-results (cache *context*))
            more-results)
      (setf (gethash :repl-backtrace (cache *context*))
            (getf error :backtrace)))))

(defun ajax-render-import (&key id from-ajax)
  (declare (ignore id) (ignore from-ajax))
  (with-html-string
    (:div :class "row"
          (:div :class "col"
                (:div :class "card"
                      (:div :class "card-block"
                            (:h4 :class "card-title"
                                 "Data")
                            (:div :class "form-group"
                                  (:label :for "import-data" "Code")
                                  (:textarea
                                   :class "form-control"
                                   :rows 20
                                   :name "import-data"
                                   :id "import-data"
                                   (cl-who:str
                                    (or (parameter "import-data") ""))))))))

    (:div :class "row"
          (:div :class "col"
                (:div :class "card"
                      (:div :class "card-block"
                            (:h4 :class "card-title"
                                 "Parsed Data")
                            (:div :class "form-group"
                                  (:label :for "import-parsed-data" "Code")
                                  (:textarea
                                   :class "form-control"
                                   :rows 20
                                   :name "import-parsed-data"
                                   :id "import-parsed-data"
                                   (dolist (parsed (gethash :import-data (cache *context*)))
                                     (cl-who:str (frmt "~S~%" parsed)))))))))

    (:div :class "row"
          (:div :class "col"
                (:div :class "card"
                      (:div :class "card-block"
                            (:h4 :class "card-title"
                                 "Lisp Code")
                            (:div :class "form-group"
                                  (:label :for "import-code" "Code")
                                  (:textarea
                                   :class "form-control wfx-lisp-code"
                                   :rows 20
                                   :name "import-code"
                                   :id "import-code"
                                   (cl-who:str
                                    (or (parameter "import-code") ""))))

                            (:button :name "wfxaction"
                                     :class "btn btn-primary"
                                     :type "submit"
                                     :value "eval-import"
                                     :onclick (frmt "~A;~A"
                                                    (js-render-form-values
                                                     "cl-wfx:ajax-render-import"
                                                     "wfx-page-body"
                                                     "wfx-page-body"
                                                     (js-pair "wfxaction"
                                                              "eval-import"))

                                                    (js-render "cl-wfx:ajax-show-debug"
                                                               "debug-info-display"))
                                     "Parse Data")
                            (:button :name "wfxaction"
                                     :class "btn btn-primary"
                                     :type "submit"
                                     :value "import-data"
                                     :onclick (frmt "~A;~A"
                                                    (js-render-form-values
                                                     "cl-wfx:ajax-render-import"
                                                     "wfx-page-body"
                                                     "wfx-page-body"
                                                     (js-pair "wfxaction"
                                                              "import-data"))

                                                    (js-render "cl-wfx:ajax-show-debug"
                                                               "debug-info-display"))
                                     "Import Data")))))))

(defun parse-import-data (text fn)
  (let ((csv-parser:*field-separator* #\|))
    (let ((stream (make-string-input-stream text)))

      (loop as line = (csv-parser:read-csv-line stream)
            while line
            do (funcall fn line)))))

(defmethod action-handler ((action (eql :eval-import))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (when (and (parameter "wfxaction")
             (parameter "import-data")
             (not (empty-p (parameter "import-data")))
             (parameter "import-code")
             (not (empty-p (parameter "import-code"))))

    (let ((*package* (or (find-package :wfx-importer)
                         (make-package :wfx-importer))))

      (setf (gethash :import-validation (cl-wfx:cache cl-wfx:*context*)) nil)

      (setf (gethash :import-data (cache *context*))
            (apply%
             (eval%
              (parameter "import-code"))
             (parameter "import-data"))))))

(defmethod action-handler ((action (eql :import-data))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)
  (when (and (parameter "wfxaction")
             (parameter "import-data")
             (not (empty-p (parameter "import-data")))
             (parameter "import-code")
             (not (empty-p (parameter "import-code"))))

    (let ((*package* (or (find-package :wfx-importer)
                         (make-package :wfx-importer))))

      (setf (gethash :import-validation (cl-wfx:cache cl-wfx:*context*)) nil)

      (setf (gethash :import-data (cache *context*))
            (apply%
             (eval%
              (parameter "import-code"))
             (parameter "import-data"))))

    (dolist (object (gethash :import-data (cache *context*)))

      (persist-document (document-collection object) object))))

(defun render-set-password ()
  (with-html-string
    (:div :class "row"
          (:div :class "col"
                (:div :class "card"
                      (:div :class "card-block"
                            (:h4 :class "card-title"
                                 "Reset User Password")
                            (:form :method "post"
                                   :action ""
                                   (:input :type "hidden" :id "contextid"
                                           :value (context-id *context*))
                                   (:div :class "form-group"
                                         (:label :for "email" "Email")
                                         (:input :type "email"
                                                 :class "form-control"
                                                 :name "email"
                                                 :id "email"
                                                 :value (or (parameter "email") "")))

                                   (:div :class "form-group"
                                         (:label :for "password" "Password")
                                         (:input :type "password"
                                                 :class "form-control"
                                                 :name "password"
                                                 :id "password"
                                                 :placeholder "Password"
                                                 :value ""))

                                   (:button :name "wfxaction"
                                            :class "btn btn-primary"
                                            :type "submit"
                                            :value "set-password"
                                            "Set Password"))
                            (:div :class "card-footer"
                                  (cl-who:str
                                   (gethash :user-not-found-error
                                            (cache *context*))))))))))

(defmethod action-handler ((action (eql :set-password))
                           (context context)
                           (request hunch-request)
                           &key &allow-other-keys)

  (when (and (parameter "email") (parameter "password"))
    (let ((user (get-user (parameter "email"))))
      (if user
          (and (change-user user (parameter "password"))
               (setf (gethash :user-not-found-error (cache *context*))
                     "Password Set!"))
          (setf (gethash :user-not-found-error (cache *context*))
                "User not found!")))))

(defun render-export ()
  (with-html-string
    (when (parameter "collection")
      (cond ((string-equal (parameter "export-type") "json")
             (cl-who:str (document-list-to-json (wfx-query-data (parameter "collection")))))
            ((string-equal (parameter "export-type") "csv")
             (cl-who:str (document-list-to-csv (wfx-query-data (parameter "collection")))))
            (t
             (cl-who:str
              (frmt "~S" (documents-to-plist (wfx-query-data (parameter "collection"))))))))))

(defun handle-file (post-parameter)
  (when (and post-parameter (listp post-parameter))
    (destructuring-bind (path filename content-type)
        post-parameter
      (declare (ignore content-type))

      ;; strip directory info send by Windows browsers
      (when (search "Windows" (hunchentoot:user-agent) :test #'char-equal)
        (setf filename (ppcre:regex-replace ".*\\\\" filename "")))

      (let ((server-path (string-downcase
                          (frmt "~A/~A/~A/files/tmp/~A/~A/"
                                (location (universe *system*))
                                (parameter "license")
                                (parameter "collection")
                                (parameter "datatype")
                                (parameter "element")))))

        (ensure-directories-exist server-path)

        (fad:copy-file path
                       (merge-pathnames
                        (sanitize-file-name filename)
                        server-path)
                       :overwrite t)))))

(defgeneric setup-file-upload (system &key &allow-other-keys))

(defmethod setup-file-upload ((system hunch-system)  &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
        (,(intern (frmt "file-upload-~A" (name system)))
         :uri ,(frmt "~Afile-upload" (site-url system))
         :allow-other-keys t)
        nil
      (when (and (boundp 'hunchentoot:*request*)
                 (hunchentoot:get-parameter "datatype"))
        (handle-file (hunchentoot:post-parameter "file_data")))
      "{}")))

(defmethod custom-render-context ((system hunch-system)
                                  context-spec
                                  &key &allow-other-keys)
  (warn "Implement custom-render-context"))

(defgeneric render-context (system context-spec-name &key &allow-other-keys))

(defmethod render-context ((system hunch-system) context-spec-name
                           &key &allow-other-keys)
  (let ((context-spec (get-context-spec-x context-spec-name)))

    (cond ((not context-spec)
           ;;(check-user-access)
           (if (current-user)
               (with-html-string
                 "<!doctype html>"
                 (cl-who:str (render-page system
                                          "Context not defined."
                                          :menu-p t)))
               (with-html-string
                 "<!doctype html>"
                 (cl-who:str "Access Denied"))))
          ((not (empty-p (getx context-spec :collection)))
           (check-user-access)

           (with-html-string
             "<!doctype html>"
             (cl-who:str
              (render-page system
                           (if (or (context-access-p context-spec)
                                   (getx (current-user) :super-user-p))
                               (render-grid
                                (getx context-spec :collection))
                               "Access Denied")
                           :menu-p t))))
          ((not (empty-p (getx context-spec :report)))
           (check-user-access)
           (with-html-string
             "<!doctype html>"
             (cl-who:str
              (render-page system
                           (if (or (context-access-p context-spec)
                                   (getx (current-user) :super-user-p))
                               (render-report
                                :html
                                (digx context-spec :report))
                               "Access Denied")
                           :menu-p t))))
          ((string-equal (getx context-spec :name) "repl")
           (check-user-access)
           (with-html-string
             "<!doctype html>"
             (cl-who:str
              (render-page system
                           (if (or (context-access-p context-spec)
                                   (getx (current-user) :super-user-p))
                               (ajax-render-repl)
                               "Access Denied")
                           :menu-p t))))

          ((string-equal (getx context-spec :name) "import")
           (check-user-access)
           (with-html-string
             "<!doctype html>"
             (cl-who:str
              (render-page system
                           (if (or (context-access-p context-spec)
                                   (getx (current-user) :super-user-p))
                               (ajax-render-import)
                               "Access Denied")
                           :menu-p t))))

          ((string-equal (getx context-spec :name) "export")
           (check-user-access)
           (with-html-string
             (:pre
              (cl-who:esc (if (or (context-access-p context-spec)
                                  (getx (current-user) :super-user-p))
                              (render-export)
                              "Access Denied")))))

          ((string-equal (getx context-spec :name) "file-upload")
           (when (and (boundp 'hunchentoot:*request*)
                      (hunchentoot:get-parameter "datatype"))
             (handle-file (hunchentoot:post-parameter "file_data"))))
          ((string-equal (getx context-spec :name) "set password")
           (with-html-string
             "<!doctype html>"
             (cl-who:str (render-page system
                                      (render-set-password)
                                      :menu-p t))))
          ((string-equal (getx context-spec :name) "login")

           (with-html-string

             (cl-who:str (render-page system
                                      (render-login)
                                      :menu-p nil))))
          ((not (empty-p (getx context-spec :renderer)))
           (eval%
            (getx context-spec :renderer)))
          (t
           (custom-render-context system context-spec)))))

(defmethod setup-context-system ((system hunch-system)
                                 &key &allow-other-keys)
  (eval
   `(hunchentoot:define-easy-handler
        (,(alexandria:symbolicate
           (string-upcase (id-string (name system)))
           '-dynamic-easy-handler)
         :uri ,(frmt "~As-wfx" (site-url system))
         :allow-other-keys t)
        nil

      (with-html-string
        (cl-who:str (render-context *system* (parameter "cs")))))))
