(in-package :cl-wfx)

(defclass data ()
  ()
  (:documentation
"Class that represents back end data store. This can be a db or as simple 
as a hash table.

Regardless of the data mechanism used it needs to implement the following
concepts:

1. Differenciate between system and license data.

   cl-wfx is designed so that any sytem implemented in it can be customized
   at run time. This means that both context specs and data specs can be 
   replaced/shodowed to allow for user customization. This also allows for
   physical data seperation on a licence level, this could be used to implement
   dockers to give each license its own sandbox etc.

2. Implement collections for data-specs. 

   These are just logical groupings of data, like tables in a database or 
   an object collection. We need this to be able to implement a fetch-all (of-type) 
   method for data and to simplify (speed-up) fetching of a specific item or items.

   Further more a data-spec's collection can be of type :system, :license or :merge.
   :system means it only contains system data and can not be replaced/shadowed.
   :license means it only contains license data
   :merge means it contains system and license data and that a shadowing merge
   will be done when data is fetched.

3. Implement entities. 

   Entities are used to represent the end users \"structural\" set up.
   Reporting and security is set up around entities. The system is designed
   to do \"role ups\" ie when viewing a report (or the raw data) the whole structure's
   data is used or only parts of the structure's data depending on the entity selection.

   An example of such a structure is company/organisational tree where you
   have holding company, companies, divisions and departments. For a accounting
   system you might want to look at over all cost or just a department's.

   Another example would be for a social media aggrigation system the media sources
   like facebook, twitter and linkedin could used be as the structure. So when looking
   at the proverbial \"inbox\" you might see all or only some of the activities.

Used: setup-data, tear-down-data."))

(defgeneric setup-data (data &key &allow-other-keys)
  (:documentation
"Method to be used to initialize any data on system startup.

Used : start-sys :after."))

(defgeneric tear-down-data (data &key &allow-other-keys)
  (:documentation  
"Method to be used to teardown any data during system shutdown. 

Used: stop-sys :before"))

(defgeneric fetch-all* (data collection &key result-type &allow-other-keys))

(defun fetch-all (collection &key result-type)  
  (if (and *system* (data *system*) )      
      (funcall 'fetch-all* 
	       (data *system*) 
	       collection 
	       :result-type result-type)))


(defgeneric fetch-items* (data collection &key test result-type &allow-other-keys))

(defun fetch-items (collection &key test result-type)  
  (if (and *system* (data *system*) )      
      (funcall 'fetch-items* 
	       (data *system*) 
	       collection 
	       :test test
	       :result-type result-type)))

(defgeneric fetch-item* (data collection &key test result-type &allow-other-keys))

(defun fetch-item (collection &key test)    
  (if (and *system* (data *system*) )      
      (funcall 'fetch-item* 
	       (data *system*) 
	       collection 
	       :test test)))

(defgeneric merge-equality-function (data &key &allow-other-keys))

(defgeneric get-key-value (item data-spec &key &allow-other-keys)
  (:documentation "Identify the key values from the data structure"))

(defmethod merge-equality-function (data &key &allow-other-keys)

  #'get-key-value)

(defgeneric system-data (data &key &allow-other-keys))

(defmethod system-data (data &key &allow-other-keys)
  (let ((store (is-initialized-system-data data)))
    (when (not store)     
      (setf store (init-system-data data))   
      (setup-data store))
    store))

(defgeneric system-data-items (collection))

(defgeneric init-system-data (data &key &allow-other-keys))

(defgeneric is-initialized-system-data (data &key &allow-other-keys))



(defgeneric license-data (data &key &allow-other-keys))

(defgeneric init-license-data (data &key &allow-other-keys))


(defgeneric is-initialized-license-data (data &key &allow-other-keys))


(defmethod license-data (data &key &allow-other-keys)
  (let ((store (is-initialized-license-data data)))
    (when (not store)     
      (setf store (init-license-data data))      
      (setup-data store))
    store))

(defgeneric license-data-items (collection))

(defun merge-items (sys-items lic-items data-spec result-type)
  (concatenate 
   (or result-type 'list) 
   (remove-if (lambda (doc)
		(find doc sys-items 
		      :test (lambda (doc tdoc)
			      (equalp (funcall (merge-equality-function 
						(data *system*) ) doc data-spec)
				      (funcall (merge-equality-function 
						(data *system*) ) tdoc data-spec)))))
	      sys-items) 
   lic-items))





(defgeneric persist-data (data &key &allow-other-keys)
  (:documentation "Called to perist data."))

(defgeneric delete-data (data &key &allow-other-keys)
  (:documentation "Called to delete data."))



(defgeneric fetch-merge-data (collection))

(defgeneric merge-data-items (data &key merge-eql-func result-type))

(defun fetch-merge-data-items (collection
			       &key (result-type 'vector)
				 merge-eql-func)
  
  (let* ((sys-items (system-data-items collection))
	 (lic-items (license-data-items collection)))
    (merge-data-items (list sys-items lic-items) 
		      :merge-eql-func (or merge-eql-func
					  (merge-equality-function 
					   (data *system*) collection))
		      :result-type result-type)))

(defun data-from-items (items &key test)
  (if test
      (if (hash-table-p items)
	  (maphash
	   (lambda (key item)
	     (declare (ignore key))
	     (when (funcall test item)
	       (return-from data-from-items item)))
	   items)
	  (map
	   'vector
	   (lambda (item)
	     (when (funcall test item)
	       (return-from data-from-items item)))
	   items))))


(defgeneric set-item-val (type item-def item value &key &allow-other-keys)
  (:documentation "Sets item data value according to definition."))

(defgeneric item-val (type item-def data &key &allow-other-keys)
  (:documentation "Retrieves data according to item definition."))

(defgeneric print-item-val (type item-def data &key &allow-other-keys)
  (:documentation "Retrieves and wrap for display data according to item definition."))
