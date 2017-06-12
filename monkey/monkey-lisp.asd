(defsystem monkey-lisp
  :name "monkey-lisp"
  :version "0.1"
  :depends-on (:closer-mop)
  :serial t
  :components ((:file "monkey-package")
	      ;; (:file "common")
	       (:file "monkey-lisp")))
