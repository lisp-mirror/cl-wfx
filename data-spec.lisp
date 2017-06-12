(in-package :cl-wfx)

(defclass data-spec-processor (monkey-lisp:processor)
  ())

#|
;;TODO: make data-spec-processor global?????
(defmacro with-data-spec (&body body)   
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (monkey-lisp:monkey-lisp (:processor-class cl-wfx::data-spec-processor)
       ,@body)))
|#
