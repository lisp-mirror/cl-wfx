(in-package :cl-wfx)

#|
(declaim (inline frmt))
(defun frmt (control-string &rest args)
  (apply #'format nil control-string args))
|#

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

;;Dynamic code evaluation and reading ##########################################3

(defparameter *lambda-functions*
  (list 'cl-wfx:frmt
	'cl-wfx::wfx-query-context-data-item
	'cl-wfx::wfx-query-context-data
	'cl-wfx:with-html
	'cl-wfx:with-html-string
	'cl-wfx::render-report
	'cl-wfx::parameter
	'cl-who:htm
	'cl-who:str
	'cl-who:esc
	'cl-naive-store:getx))

(defun lambda-eval-safe (lambdax)
  (let* ((sandbox-impl::*allowed-extra-symbols*
	    *lambda-functions*)
	  (lambda-result (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))

      (with-output-to-string (s lambda-result)
	(let ((sandbox::*msg-value-prefix* "")
	      (sandbox::*msg-error-prefix* "")
	      (sandbox::*msg-value-formatter* "~{~S~^<br/> ~}")
	      (sandbox::*msg-no-value-message* "Nil"))
	  
	  (sandbox::read-eval-print lambdax  s)
	  lambda-result))))


(defun read-s-expressions (expressions-string)
  (let ((stream (make-string-input-stream expressions-string))
	(expressions))
    
    (when stream
      (loop with expr = nil
	   do
	   (setf expr (read stream nil))
	   (when expr
	     (setf expressions (push expr  expressions)))
	   while expr)
      (close stream))
    
    (reverse expressions)))


(defun read-no-eval (value)
  :documentation "Function to convert string to s-expressions. Any value that is not a string is returned as is. When the value to read is a string it could consist of multiple s-expresssions, the second parameter returned indicates if this is the case or not. If multiple a list of s-expressions is returned else a single s-expression."
  (let ((*read-eval* nil)
	(expressions ))
    (if value
	(if (stringp value)
	    (progn
	      (setf expressions 
		    (read-s-expressions value))
	      (if (> (length expressions) 1)
		  (values expressions t)
		  (values (first expressions))))
	    (values value nil)))))

(defun evaluable-p (s-expression)
  (fboundp (first s-expression)))


(defun log-eval (error results backtrace)

  (when *context*
    (setf (gethash :debug-error (cache *context*))	   
	      error )
    (setf (gethash :debug-results (cache *context*))	   
	  results )
    (setf (gethash :debug-backtrace (cache *context*))	   
	  backtrace 
	  )))



(defun read-eval (expressions-string)
  :documentation "Function converts a string to s-expressions and returns the results of evaluating each s-expression in turn. If the s-expression is not deemed to be evaluatable the expression is returned as is."
  (let ((results)
	(last-expression))
    (handler-case
	(let ((stream (make-string-input-stream expressions-string) ))
	  
	  (when stream
	    (loop with expr = nil
	       do
		 (setf expr (read stream nil))
		 (when expr
		   (setf last-expression expr)
		   (if (evaluable-p expr)
	     	       (push (eval expr) results)
		       (push expr results)))
	       while expr)
	    (close stream))
	 ;; (setf results (reverse results))
	  (values (car results)
		  (cdr results)
		  (list
		   :error nil
		   :backtrace nil)))
      (error (c)
	(log-eval c results (sb-debug:list-backtrace))
	(values c
		results
		(list 
		 :error c
		 :backtrace (append (list (list last-expression)) (sb-debug:list-backtrace))))))))


(defun eval-blob% (blob)
  (read-eval (blob-string-value blob) ))

(defun eval% (object &key package-name)
  (cond  ((and (item-p object) (item-of-type-p object "lambda"))
	    (let ((*package* (or (and package-name (or (find-package package-name)
						       (make-package package-name)))
				 *package*)))
	      (eval-blob% (getx object :code))))
	   ((and (item-p object) (item-of-type-p object "package"))
	    (let ((*package* (or
			      (and package-name (or (find-package package-name)
						    (make-package package-name)))
			      (and (getx object :package)
				   (find-package (frmt "~A" (getx object :package))))
			      (and (getx object :package)
				   (make-package (frmt "~A" (getx object :package))))
			      *package*)))
	      (eval-blob% (getx object :code))))
	   ((stringp object)
	    (read-eval object))
	   (t
	    (handler-case
		
		(eval object)
	      
	      (error (c)
		(log-eval c nil (sb-debug:list-backtrace)))))))


(defun load-blob% (blob)
  (load (make-string-input-stream (blob-string-value blob))))

(defun load% (object &key package)

  (cond  ((and (item-p object) (item-of-type-p object "lambda"))
	  (let ((*package* (or package  *package*)))
	    (load-blob% (getx object :code))))
	 ((and (item-p object) (item-of-type-p object "package"))
	  (let ((*package* (or
			    package
			    (and (getx object :package)
				 (find-package (frmt "~A" (getx object :package))))
			    (and (getx object :package)
				 (make-package (frmt "~A" (getx object :package))))
			    *package*)))
	    (load-blob% (getx object :code))))
	 ((blob-p object)
	  (load-blob% object))
	 ((stringp object)
	  (load object))
	 (t
	  (load object))))

(defun apply% (function arg &rest arguments)
  (handler-case
      (apply function arg arguments)
    (error (c)
      (break "~S" c)
      (log-eval c nil (sb-debug:list-backtrace)))))

(defun funcall% (function &rest arguments)
  (handler-case
      (apply  function (car arguments) (cdr arguments))
    (error (c)
      (log-eval c nil (sb-debug:list-backtrace)))))


;;#############################STRINGS
;;FORMAT


(defun format-money-for-export-no-cents (value &key (include-comma nil))
  (typecase value
    (null "")
    ((or integer single-float ratio float)
     (if include-comma
	 (format nil "~:d" (truncate value))
	 (format nil "~d" (truncate value))
	 ))
    (t
     (princ-to-string value))))

(defun format-money-for-export (value &key (include-comma nil))
  (typecase value
    (null "")
    ((or integer single-float ratio float)
     (if include-comma
	 (format nil "~:d" value)
	 (format nil "~d" value)
	 ))
    (t
     (princ-to-string value))))


(defun frmt-money (value &key (include-comma t))
  (typecase value
        (null "")
        (number
         (multiple-value-bind (quot rem) (truncate (round value 1/100) 100)
           (format nil "~@?.~2,'0d"
                   (if include-comma "~:d" "~d")
                   quot (abs rem))))
        (t
         (princ-to-string value))))

#|
;;STRING MANIPULATION
(defun trim-whitespace (string)
  (string-trim
   '(#\Space #\Newline #\Tab #\Return) string))
|#

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

#|
(defun empty-p (value)
  "Checks if value is null or an empty string."
  (or
   (not value)
   (null value)
   (equal value "")
   (if (stringp value)
       (string-equal value "NIL")
       )
   (equal (trim-whitespace (princ-to-string value)) "")))
|#

(declaim (inline ensure-num))
(defun ensure-num (value)
  :documentation "If there is junk in the value then 0 is returned."
  (let ((final-val 0))
    (if (empty-p value)
	final-val
	(if (stringp value)
	    (setf final-val (read-from-string value))
	    (setf final-val value)))
    (cond ((numberp final-val)
	       final-val)
	      (t
	       0))))

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
  (let ((position (or (position (frmt "~A" month) *short-months*
                                :test #'string-equal)
                      (position (frmt "~A" month) *long-months*
                                :test #'string-equal)
                      (position (frmt "~A" month) *short-months-afrikaans*
                                :test #'string-equal)
                      (position (frmt "~A" month) *long-months-afrikaans*
                                :test #'string-equal))))
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

(defun build-system-date (year month day)
  (format nil "~d-~2,'0d-~2,'0d" year month day))

(defun format-system-universal-date (universal-date)
  (when universal-date
    (if (stringp universal-date)
        universal-date
        (multiple-value-bind (a b c day month year)
            (decode-universal-time universal-date
                                   *time-zone*)
          (declare (ignore a b c))
          (build-system-date year month day)))))

(defun format-system-date (date)
  (if (typep date 'unsigned-byte)
      (format-system-universal-date date)
      date))


(defconstant +24h-secs+ (* 60 60 24))

(defconstant +hour-secs+ (* 60 60))

(defun date-diff (start-date end-date &key return-type)
  (cond ((equal return-type :days)
         (/ (- end-date  start-date) +24h-secs+))
        ((equal return-type :hours)
         (/ (- end-date  start-date) +hour-secs+))
        ((equal return-type :minutes)
         (/ (- end-date  start-date) 60))
        (t
         (- end-date start-date))))


;;########################################list manipulation

