(defsystem monkey-output-lisp
  :name "monkey-output-lisp"
  :version "0.1"
  :depends-on (:closer-mop :monkey-lisp)
  :serial t
  :components ((:file "monkey-output-package")
	      ;; (:file "common")
	       (:file "monkey-output")))
