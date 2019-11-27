(in-package :cl-wfx)

(defmethod parameter (parameter)
  (or (request-parameter parameter)
      (context-parameter parameter)
      (session-parameter parameter)))
