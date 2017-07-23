(in-package :cl-wfx)

;;GLOBAL VARS and Shortcut functions ################################
(defvar *sys-license-code* "000000")

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


(defun read-no-eval (value)
  (let ((*read-eval* nil))
    (if value
	(if (stringp value)
	    (read-from-string value)
	    (read value)))))


(defun dig-down (place indicators)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))
	     
    (if indicators
	(dig-down next-place indicators)
	next-place)))

(defun set-dig-down (place indicators value)
  (let* ((indicator (pop indicators))
	 (next-place (if indicator
			 (getf place indicator))))
    (if indicators	
	(setf (getf place indicator) 
	      (set-dig-down next-place indicators value))
	(setf (getf place indicator) value))
    place))

(defun dig (place &rest indicators)
  (dig-down place indicators))

(defun (setf dig) (value place &rest indicators)
  (set-dig-down place indicators value))


(defun naive-dig (place indicators)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (equalp (type-of place) 'item)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (equalp (type-of val) 'item)
			 (item-values val)
			 val)))
    	    
    (if indicators
	(naive-dig next-place indicators)
	next-place)))

(defun set-naive-dig (place indicators value)
  (let* ((indicator (pop indicators))
	 (val (if indicator
		  (if (equalp (type-of place) 'item)
		      (getx place indicator)
		      (getf place indicator))))
	 (next-place (if (equalp (type-of val) 'item)
			 (if (cl-naive-store::item-changes val)
				 (cl-naive-store::item-changes val)
				 (setf (cl-naive-store::item-changes val)
				       (copy-list (item-values val))))
			 val)))
    	    
    (if indicators
	(if (equalp (type-of val) 'item)
		(set-naive-dig next-place indicators value)
		(setf (getf place indicator) 
		      (set-naive-dig next-place indicators value)))
	(setf (getf place indicator) value))
    place))

(defun digx (place &rest indicators)
  (naive-dig place indicators))

(defun (setf digx) (value place &rest indicators)
  (set-naive-dig place indicators value))


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

#|
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

|#

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
                               (#'parse-integer day)) 
      ("(\\d{4})-(\\d{1,2})-(\\d{1,2})" date)
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
