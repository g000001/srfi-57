;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-57
  (:use)
  (:export
   :define-record-type
   :define-record-scheme
   :record-update
   :record-update!
   :record-compose))

(defpackage :srfi-57.internal
  (:use :srfi-57 :cl :fiveam :srfi-23 :mbe :srfi-16)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :map :lambda :loop :member :assoc :remove-duplicates :union
           :intersection))
