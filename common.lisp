(in-package :cl-wfx)

(defparameter *log-shit-p* nil)

(defun log-shit (shit)
  (when *log-shit-p*
    (with-open-file (out "/home/phil/source/shit.log"
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (pprint (frmt "***********************************************************~%~A" shit) out)
      (close out)
      
      ))
  nil)

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



;;#############################STRINGS
;;FORMAT
(declaim (inline frmt))
(defun frmt (control-string &rest args)
  (apply #'format nil control-string args))

(defun format-money-for-export (value &key (include-comma nil))
  (typecase value
    (null "")
    ((or integer single-float ratio float)
     (if include-comma
	 (format nil "~:d" (truncate value))
	 (format nil "~d" (truncate value))
	 ))
    (t
     (princ-to-string value))))

;;STRING MANIPULATION
(defun trim-whitespace (string)
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))

(defun assert-ends-with-/ (string)
  (assert (char= (alexandria:last-elt string) #\/)))

(defun id-string (id)
  (ppcre:regex-replace-all "[^A-Za-z0-9-_:.]"
                           (substitute #\- #\Space (string-downcase id)) ""))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
(when string
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos))))

(defun replace-all-shit (string value-pairs)
  (let ((new-val string))
    (dolist (value-pair value-pairs)
      (setf new-val (replace-all new-val
				 (first value-pair)
				 (second value-pair))))
    new-val))

(defun sanitize-file-name (name)
  (when name
    (string-downcase
     (replace-all-shit name
		       '(("_" "-")
			 ("(" "-")
			 (")" "-")
			 ("'" "-")
			 ("\"" "-")
			 ("," "-")
			 (" " "-"))))))

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

(defun short-month-name (n)
  (when (array-in-bounds-p *short-months* (1- n))
    (aref *short-months* (1- n))))

(defun long-month-name (n)
  (when (array-in-bounds-p *long-months* (1- n))
    (aref *long-months* (1- n))))

(defun build-date (year month day)
  (format nil "~d ~a ~d" day (short-month-name month) year))

(defun build-date-time (year month day hour min sec
                        &optional timezone)
  (declare (ignore timezone))
  (format nil "~d ~a ~d ~@{~2,'0d~^:~}"
          day (short-month-name month) year hour min sec))

(defun format-universal-date (universal-date)
  (when universal-date
    (if (stringp universal-date)
        universal-date
        (multiple-value-bind (a b c day month year)
            (decode-universal-time universal-date
                                   *time-zone*)
          (declare (ignore a b c))
          (build-date year month day)))))

(defun format-date (date)
  (if (typep date 'unsigned-byte)
      (format-universal-date date)
      date))
