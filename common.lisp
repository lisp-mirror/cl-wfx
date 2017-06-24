(in-package :cl-wfx)

;;GLOBAL VARS and Shortcut functions ################################
(defvar *sys-license-code* "000000")

(defparameter *data-specs* nil
  "On compile this is used to store system specs temporarily for loading into db.")

(defvar *context* nil
  "Not to be set. Exposes the current context.")

(defvar *module* nil
  "Not to be set. Exposes the current module of the current context.")

(defvar *session* nil
  "Not to be set. Used internally Exposes the current session.")

(defvar *system* nil
  "Global variable designating the current system. 
Notes:
Dont set manually use with-system macro.")

(defvar *theme* nil
  "Not to be set by user. Used by convenience functions.")

(defvar *current-theme* nil
  "Active themearound a widget render, used by convenience functions. Not to be set by user.")

(defvar *widget* nil
  "Active widget around a widget render, used by convenience functions. Not to be set by user.")

(defun current-user ()
  (if (and (boundp '*session*) *session*) 
      (if (user *session*)
	  (user (user *session*)))))

(defun active-user ()
  (if (boundp '*session*)   
      (user *session*)))


;;Data

(defgeneric getx (item identifier &key collection qualifier &allow-other-keys)
  (:documentation "The getx method is to be used to get a piece of data from various structures. The purpose is to enable the use of different data structures, without having to change code. 

Qualifier is an exstra test that can be applied to the objects retrieved from the collection and needs to be a (lambda (object)..return objects) or a function designator that operates in the same fasion. This was not intended to be specialized on lisp types because lisp allready has get functions for its collection types.
key can really be anything, just document your implementation to help out others that want to use it.
of should be of form form (of (eql :what-am-i-getting)) and is just a descriptor to differentiate between getx's.
"))

;;strings #############################
;;FORMAT
(declaim (inline frmt))
(defun frmt (control-string &rest args)
  (apply #'format nil control-string args))

(defun assert-ends-with-/ (string)
  (assert (char= (alexandria:last-elt string) #\/)))

(defun id-string (id)
  (ppcre:regex-replace-all "[^A-Za-z0-9-_:.]"
                           (substitute #\- #\Space id) ""))


(defun plural-name (name)
  (setf name (frmt "~A" name))
  (let ((last-char (char name (1- (length name)))))
    (cond ((char-equal last-char #\s)
	   name)
	  ((char-equal last-char #\y)
	   (alexandria:symbolicate (subseq name 0
					   (1- (length name)))
				   'ies))
	  (t
	   (alexandria:symbolicate name 's)))))



(defun read-symbol-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))
