(in-package :cl-wfx)

(defun concrete-type-get-set (element)
  (cond ((listp (getx element :concrete-type))
         (cond ((equalp (getx (getx element :concrete-type) :complex-type)
                        :value-list)
                (getx (getx element :concrete-type) :type))
               ((equalp (getx (getx element :concrete-type) :complex-type)
                        :key-value-list)
                (getx (getx element :concrete-type) :type))
               (t
                (getx (getx element :concrete-type) :complex-type))))
        (t
         (getx element :concrete-type))))

(defmethod getx (document (element cl-naive-store.document-types:element) &key &allow-other-keys)
  (let ((concrete-type (concrete-type-get-set element)))
    (getxe document element concrete-type)))

(defmethod (setf getx) (value document (element cl-naive-store.document-types:element)
                        &key &allow-other-keys)
  (let ((concrete-type (concrete-type-get-set element)))
    (setf (getxe document element concrete-type) value)))

(defun getxe* (document element)
  (let* ((name (getx element :name)))
    (getx document name)))

(defgeneric getxe (document element type &key &allow-other-keys))

(defmethod getxe (document element type &key &allow-other-keys)
  (declare (ignorable type))
  (getxe* document element))

(defun element-type-val (element key)
  (Let ((type (getx element :concrete-type)))
    (when (listp type)
      (getx type key))))

(defun set-getxe* (value document element)
  (let* ((name (getx element :name)))
    (setf (getx document name) value)))

(defun setxe-read* (value document element type-test read-error)
  (let* ((name (getx element :name))
         (*read-eval* nil)
         (final-val))

    (if value
        (if (stringp value)
            (if (not (empty-p value))
                (setf final-val (read-from-string value)))
            (setf final-val value))
        (setf final-val value))

    (if final-val
        (if  (apply type-test (list final-val))
             (setf (getx document name) final-val)
             (error (frmt read-error final-val)))
        (setf (getx document name) final-val))))

(defgeneric validate-xe (document element type value &key &allow-other-keys))

(defmethod validate-xe (document element type value &key &allow-other-keys)
  (declare (ignorable document type))
  (if (digx element :attributes :setf-validate)
      (funcall (digx element :attributes :setf-validate) value)
      (values t nil)))

;;TODO: how to do collection checking, additional parameters/keys
;;when and how to pass
(defmethod (setf getxe) :around (value document element type
                                 &key &allow-other-keys)
  (declare (ignorable value document element type))
  ;;TODO: Figure out what to do in validate-xe
  ;;it is no longer clear if it is needed or what it
  ;;it is trying to do
  (call-next-method))

(defmethod (setf getxe) (value document element type
                         &key &allow-other-keys)
  (declare (ignorable type))
  (setf (getx document (getx element :name)) (frmt "~A" value)))

(defmethod (setf getxe) (value document element (type (eql :symbol))
                         &key &allow-other-keys)
  (setxe-read* value document element #'symbolp  "~S is not a symbol!"))

(defmethod (setf getxe) (value document element (type (eql :keyword)) &key &allow-other-keys)
  (setxe-read* value document element #'keywordp  "~S is not a keyword!"))

(defmethod (setf getxe) (value document element (type (eql :number))
                         &key &allow-other-keys)
  (setxe-read* value document element #'numberp "~R is not a number!"))

(defmethod (setf getxe) (value document element (type (eql :integer))
                         &key &allow-other-keys)
  (setxe-read* value document element #'numberp "~R is not an integer!"))

(defmethod (setf getxe) (value document element (type (eql :date-time))
                         &key &allow-other-keys)
  (let* ((name (getx element :name)))
    (if (stringp value)
        (if (> (length value) 18)
            (setf (getx document name) (local-time:parse-timestring value))
            (if (= (length value) 16)
                (setf (getx document name)
                      (local-time:parse-timestring
                       (format nil "~A~A" value
                               (subseq (format nil "~A" (local-time:now)) 16))))
                (setf (getx document name) value)))
        (setf (getx document name) value))))

(defmethod (setf getxe) (value document element (type (eql :date))
                         &key &allow-other-keys)
  (set-getxe* value document element))

(defmethod (setf getxe) (value document element (type (eql :time))
                         &key &allow-other-keys)
  (set-getxe* value document element))

(defmethod (setf getxe) (value document element (type (eql :boolean))
                         &key &allow-other-keys)

  (let* ((split (split-sequence:split-sequence #\, value))
         (val (if (equalp (car split) "true")
                  t)))
    (set-getxe* element document val)))

(defmethod (setf getxe) (value document element (type (eql :lambda)) &key &allow-other-keys)
  (setxe-read* value document element #'consp "~S is not a cons!"))

(defmethod (setf getxe) (value document element (type (eql :collection))
                         &key &allow-other-keys)

  (let ((name (getx element :name))
        (final-val))
    (if (not (empty-p value))
        (setf final-val value))
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value document element (type (eql :document))
                         &key &allow-other-keys)

  (let ((name (getx element :name))
        (final-val))
    (if (not (empty-p value))
        (setf final-val value))
    (setf (getx document name) final-val)))

(defmethod validate-xe (document element (type (eql :collection-contained-document)) value
                        &key &allow-other-keys)
  (let* ((valid (find value document)))
    (values valid (if (not valid)
                      (frmt "Value ~A not found in ~A" value
                            (digx element :concrete-type :collection))))))

(defmethod validate-xe (document element (type (eql :collection)) value
                        &key &allow-other-keys)

  (let* ((valid (find value document)))

    (values valid (if (not valid)
                      (frmt "Value ~A not found in ~A" value
                            (digx element :concrete-type :collection))))))

(defmethod validate-xe (document element (type (eql :document)) value
                        &key &allow-other-keys)

  (let* ((valid (find value document)))
    (values valid (if (not valid)
                      (frmt "Value ~A not found in ~A" value
                            (digx element :concrete-type :collection))))))

(defmethod (setf getxe) (value document element (type (eql :value-string-list))
                         &key &allow-other-keys)
  (let* ((name (getx element :name))
         (delimiter (if (stringp (digx element :concrete-type :delimiter))
                        (coerce (digx element :concrete-type :delimiter)
                                'character)
                        (coerce (eval (digx element :concrete-type :delimiter))
                                'character)))
         (type (digx element :concrete-type :type))
         (split (split-sequence:split-sequence delimiter value))
         (list))
    (dolist (x split)

      (unless (empty-p x)
        (if (equalp type :keyword)
            (setf list (append list
                               (list (intern (string-upcase
                                              (remove #\: (naive-impl:trim-whitespace x)))
                                             :KEYWORD))))
            (setf list (append list (list (naive-impl:trim-whitespace x)))))))
    (setf (getx document name) list)))

(defmethod validate-xe (value document element (type (eql :value-list))
                        &key &allow-other-keys)

  (let* ((list (or (and (digx element :concrete-type :values-script)
                        (eval (digx element :concrete-type :values-script)))
                   (digx element :concrete-type :elements)))
         (*read-eval* nil)
         (valid))

    (if (functionp list)
        (setf list (funcall (eval (digx element :concrete-type :values-script)) document)))

    (setf valid (find (if (not (or (equalp (digx element :concrete-type :type) :string)
                                   (equalp (digx element :concrete-type :type) :link)
                                   (equalp (digx element :concrete-type :type) :text)))
                          (if (and value (not (empty-p value)))
                              (read-from-string value))
                          value)
                      list :test #'equalp))

    (values valid (if (not valid)
                      (frmt "Value not one of ~S" list)))))

(defmethod (setf getxe) (value document element (type (eql :value-list))
                         &key &allow-other-keys)
  (let* ((name (getx element :name))
         (list (or (and (digx element :concrete-type :values-script)
                        (eval (digx element :concrete-type :values-script)))
                   (digx element :concrete-type :values)))
         (*read-eval* nil)
         (val))

    (if (functionp list)
        (setf list (funcall (eval (digx element :concrete-type :values-script)) document)))

    (setf val (find (if (not (or (equalp (digx element :concrete-type :type) :string)
                                 (equalp (digx element :concrete-type :type) :link)
                                 (equalp (digx element :concrete-type :type) :text)))
                        (if (and value (not (empty-p value)))
                            (read-from-string value))
                        value)
                    list :test #'equalp))

    (setf (getx document name) val)))

(defmethod (setf getxe) (value document element (type (eql :key-value-list))
                         &key &allow-other-keys)
  (setf (getxe (digx element :concrete-type :type) element document) value))

;;TODO: Check for dulplicates?
(defmethod (setf getxe) (value document element (type (eql :collection-documents))
                         &key &allow-other-keys)
  (set-getxe* value document element))

(defmethod (setf getxe) (value document element (type (eql :contained-document))
                         &key &allow-other-keys)
  (let ((name (getx element :name))
        (final-val))

    (setf final-val value)
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value document element (type (eql :collection-contained-document))
                         &key &allow-other-keys)
  (let ((name (getx element :name))
        (final-val))

    (setf final-val value)
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value (type (eql :document)) element (document document)
                         &key &allow-other-keys)

  (let ((name (getx element :name))
        (final-val))
    (if (not (empty-p value))
        (if (document-p value)
            (setf final-val value)
            (error (frmt "~S is not of type ~A!" value
                         (digx element :concrete-type :data-spec)))))
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value (type (eql :contained-document)) element (document document)
                         &key &allow-other-keys)
  (let ((name (getx element :name))
        (final-val))

    (if (document-p value)
        (setf final-val value)
        (if (not (empty-p value))
            (error (frmt "~S is not of type ~A!" value
                         (digx element :concrete-type :data-spec)))
            (setf final-val nil)))
    (setf (getx document name) final-val)))

(defmethod (setf getxe) (value (type (eql :collection-contained-document))
                         element (document document)   &key &allow-other-keys)
  (let ((name (getx element :name))
        (final-val))

    (if (document-p value)
        (setf final-val value)
        (if (not (empty-p value))
            (error (frmt "~S is not of type ~A!" value
                         (digx element :concrete-type :data-spec)))
            (setf final-val nil)))
    (setf (getx document name) final-val)))

(defmethod getxe ((type (eql :lisp-code)) document element &key &allow-other-keys)
  (getxe* document element))

(defmethod getxe ((type (eql :java-script)) document element &key &allow-other-keys)
  (getxe* document element))

(defmethod getxe ((type (eql :css)) document element &key &allow-other-keys)
  (getxe* document element))

(defmethod getxe ((type (eql :html)) document element &key &allow-other-keys)
  (getxe* document element))

(defmethod getxe ((type (eql :text-blob)) document element &key &allow-other-keys)
  (getxe* document element))

;;BLOBS

(defmethod (setf getxe) (value (type (eql :lisp-code)) element document
                         &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
        (setf (blob-raw blob) value)
        (if (not parent-hash)
            (error "Cannot create blob without parent-hash!")
            (setf blob (make-blob  :file-type :text
                                   :file-ext "lisp"
                                   :location ""
                                   :raw value
                                   :parent-accessor (getf element :name)))))
    (set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :css)) document element
                         &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
        (setf (blob-raw blob) value)
        (if (not parent-hash)
            (error "Cannot create blob without parent-hash!")
            (setf blob (make-blob  :file-type :text
                                   :file-ext "css"
                                   :location ""
                                   :raw value
                                   :parent-accessor (getf element :name)))))
    (set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :html)) document element
                         &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
        (setf (blob-raw blob) value)
        (if (not parent-hash)
            (error "Cannot create blob without parent-hash!")
            (setf blob (make-blob  :file-type :text
                                   :file-ext "html"
                                   :location ""
                                   :raw value
                                   :parent-accessor (getf element :name)))))
    (set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :java-script)) document element
                         &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
        (setf (blob-raw blob) value)
        (if (not parent-hash)
            (error "Cannot create blob without parent-hash!")
            (setf blob (make-blob  :file-type :text
                                   :file-ext "js"
                                   :location ""
                                   :raw value
                                   :parent-accessor (getf element :name)))))
    (set-getxe* blob document element)))

(defmethod (setf getxe) (value (type (eql :text-blob)) document element
                         &key parent-hash &allow-other-keys)
  (let ((blob (getx document (getf element :name))))
    (if (blob-p blob)
        (setf (blob-raw blob) value)
        (if (not parent-hash)
            (error "Cannot create blob without parent-hash!")
            (setf blob (make-blob  :file-type :text
                                   :file-ext "txt"
                                   :location ""
                                   :raw value
                                   :parent-accessor (getf element :name)))))
    (set-getxe* blob document element)))
