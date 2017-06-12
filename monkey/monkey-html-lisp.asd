(defsystem monkey-html-lisp
  :name "monkey-html-lisp"
  :version "0.1"
  :depends-on (:closer-mop :monkey-lisp :monkey-output-lisp)
  :serial t
  :components ((:file "monkey-html-package")
	      ;; (:file "common")
	       (:file "monkey-html")))
