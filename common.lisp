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

;;#############################STRINGS
;;FORMAT
(declaim (inline frmt))
(defun frmt (control-string &rest args)
  (apply #'format nil control-string args))

;;STRING MANIPULATION
(defun trim-whitespace (string)
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))

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
    (if string
	(if (stringp string)
	    (read-from-string string)
	    string))))


;;#####################VALUE CHECKS

(defun empty-p (value)
  "Checks if value is null or an empty string."
  (or (null value)
      (equal value "")
      (equal (trim-whitespace (princ-to-string value)) "")))

;;#####################DATES

(defvar *time-zone* 0)

(defun decode-iso-date (date)
  (ppcre:register-groups-bind ((#'parse-integer year)
                               (#'parse-integer month)
                               (#'parse-integer day)) ("(\\d{4})-(\\d{1,2})-(\\d{1,2})" date)
    (values year month day)))





(defvar *short-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar *long-months*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defvar *short-months-afrikaans*
  #("Jan" "Feb" "Mrt" "Apr" "Mei" "Jun" "Jul" "Aug" "Sep" "Okt" "Nov" "Des"))

(defvar *long-months-afrikaans*
  #("Januarie" "Februarie" "Maart" "April" "Mei" "Junie"
    "Julie" "Augustus" "September" "Oktober" "November" "Desember"))


(defun month-number (month)
  (let ((position (or (position month *short-months*
                                :test #'equalp)
                      (position month *long-months*
                                :test #'equalp)
                      (position month *short-months-afrikaans*
                                :test #'equalp)
                      (position month *long-months-afrikaans*
                                :test #'equalp))))
    (when position
      (1+ position))))

(defun ensure-parse-integer (value &key (start 0) end)
  (typecase value
    (string
     (multiple-value-bind (integer position)
         (parse-integer value :junk-allowed t
                        :start start :end end)
       (when (= (length value) position)
         integer)))
    (integer value)))

(defun decode-date-string (date)
  (multiple-value-bind (year month day) (decode-iso-date date)
    (if year
        (values year month day)
        (let ((date-split (ppcre:split "[.\\/ -]+" (trim-whitespace date))))
          (and date-split
               (let* ((month-raw (second date-split))
                      (month (or (month-number month-raw)
                                 (ensure-parse-integer month-raw)))
                      (year (ensure-parse-integer (third date-split)))
                      (day (ensure-parse-integer (first date-split))))
                 (values year month day)))))))



(defun decode-date (date &key time-zone)
  (etypecase date
    (string
     (decode-date-string date))
    (integer
     (multiple-value-bind (a b c day month year)
         (decode-universal-time date (or time-zone *time-zone*))
       (declare (ignore a b c))
       (values year month day)))))

(defvar *month-days* #(31 28 31 30 31 30 31 31 30 31 30 31))

(defun leap-year-p (year)
  (cond
    ((zerop (rem year 400)) t)
    ((zerop (rem year 100)) nil)
    ((zerop (rem year 4)) t)))


(defun check-date (date month year)
  ;; Technically, there can be a 31 dec 1899 date, if the time-zone is
  ;; west of GMT, but it's not particularly important.
  (when (and (typep year '(integer 1900))
             (typep month '(integer 1 12))
             (plusp date))
    (let ((days (svref *month-days* (1- month))))
      (cond ((and (= month 2)
                  (leap-year-p year))
             (<= date 29))
            (t
             (<= date days))))))

(defun parse-date (date &key time-zone)
  (etypecase date
    (integer date)
    (string
     (multiple-value-bind (year month date) (decode-date-string date)
       (when (check-date date month year)
         (encode-universal-time 0 0 0 date month year
                                (or time-zone *time-zone*)))))
    (null nil)))
