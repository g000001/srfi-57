;;;; srfi-57.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-57
  :serial t
  :depends-on (:fiveam
               :mbe
               :srfi-9
               :srfi-23
               :srfi-16)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-57")
               (:file "test1")
               (:file "test")
               ))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-57))))
  (load-system :srfi-57)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-57.internal :srfi-57))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
