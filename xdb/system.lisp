(in-package :cl-wfx)

(defmethod init-sys-data ((system system) &key &allow-other-keys)
  (let* ((data-folder (format nil "~~/~A/xdb2/" 
			      (system-folder system)))
	 (data (make-instance 'xdb-data
			      :base-path data-folder
			      :data-folder data-folder))
	 (db)
	 )
    (ensure-directories-exist (data-folder data))
    
    (setf db (xdb2:add-db data (list (frmt "~A" (id-string (system-name *system*)))
			     *sys-license-code*)))
    
    (setf (data *system*) 
	  data)
    (setup-data data)))
