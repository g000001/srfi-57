(cl:in-package :srfi-57.internal)

(define-record-type pt (make-pt x y) pt?
                    (x pt.x pt.x-set!)
                    (y pt.y pt.y-set!))

;; |foo|
;; Simple record schemes.
;; Record schemes don't have constructors.
;; The predicates and accessors are polymorphic.
(define-record-scheme <point :false <point?
                      (x <point.x)
                      (y <point.y))

(define-record-scheme <color :false <color?
                      (hue <color.hue))

;; Concrete instances of the above schemes.
;; Constructors may be declared.
;; Predicates and accessors, when provided, are monomorphic.
(define-record-type (point <point) make-point point?
                    (x point.x)
                    (y point.y))

(define-record-type (color <color) make-color)

(define-record-type (color-point <color <point)
    (make-color-point x y hue) color-point?
    (extra color-point.extra))


;;; |Example of tree data type|
(define-record-scheme <tree :false <tree?)
(define-record-type (node <tree) make-node node?
                    (lhs node.lhs)
                    (rhs node.rhs))
(define-record-type (leaf <tree) make-leaf leaf?
                    (val leaf.val))
(define-function (tree->list tr)
  (cond
    ((leaf? tr) (leaf.val tr))
    ((node? tr) (cons (tree->list (node.lhs tr))
                      (tree->list (node.rhs tr))))))
