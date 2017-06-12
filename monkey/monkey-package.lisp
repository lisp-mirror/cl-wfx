(defpackage :monkey-lisp
  (:nicknames "ml" "m-lisp")
  (:use :common-lisp )
  (:export 
   :processor
   :*processor*
   :*processor-class*
   :self-evaluating-p
   :language-operator-p
   :monkey-sexp-p   
   :monkey-macro-form-p
   :parse-monkey-sexp
   :process-monkey-sexp
   :pre-process-monkey-sexp
   :process-attributes
   :process-attribute
   :process-body
   :post-process-monkey-sexp
   :process-language-operator
   :define-language-operator
   :expand-monkey-macro-form
   :process
   :monkey-lisp
   :with-ml
   ))
