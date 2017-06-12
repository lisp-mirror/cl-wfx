(defpackage :monkey-output-lisp
  (:nicknames "ol" "o-lisp")
  (:use :common-lisp :monkey-lisp)
  (:export
   :*emit-output*
   :printer-class
   :indenter-class   
   :pretty-printer
   :indent-printer
   :tab-width
   :indenting-printer
   :out
   :beginning-of-line-p
   :indentation
   :indenting-p
   :get-pretty-printer
   :get-indent-printer
   :emit
   :emit/no-newlines
   :emit-newline
   :emit-fresh-line
   :indent-if-necessary  
   :output-op-emit
   :output-op-code
   :define-output-operator
   :monkey-output-lisp
   :monkey-output
   :output-processor
   ))
