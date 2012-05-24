(cl:in-package :srfi-57.internal)

(def-suite srfi-57)

(in-suite srfi-57)

#|(define-record-type point (make-point x y) point?
                    (x get-x set-x!)
                    (y get-y set-y!))|#

#|(test |A simple record|
  (let ((p (make-point 1 2)))
    (is (= 2 (get-y p)))
    (set-y! p 3)
    (is (= 3 (get-y  p)))
    (is-true (point? p))) )|#


;=================================================================================
; Examples:

; A simple record declaration:
#|(define-record-type point (make-point x y) point?
                    (x point.x point.x-set!)
                    (y point.y point.y-set!) )|#

#|(test |A simple record declaration|
  (define-record-type point (make-point x y) point?
                    (x point.x point.x-set!)
                    (y point.y point.y-set!) )
  (let ((p (make-point 1 2)))
    (is-true (point? p))
    (is (= 2 (point.y p)))
    (point.y-set! p 3)
    (is (= 3 (point.y p)))) )|#


; Simple record schemes.
 ; Record schemes don't have constructors.
 ; The predicates and accessors are polymorphic.

(define-record-scheme <point nil <point?
                      (x <point.x)
                      (y <point.y))

(define-record-scheme <color nil <color?
                      (hue <color.hue))

 ; Concrete instances of the above schemes.
 ; Constructors may be declared.
 ; Predicates and accessors, when provided, are monomorphic.

(define-record-type (point <point) make-point point?
                    (x point.x)
                    (y point.y))

(set 'po (make-point 1 2))

(test |Simple record schemes.|
  (is-true (point? (make-point 1 2))))

(define-record-type (color <color) make-color)

(define-record-type (color-point <color <point)
    (make-color-point x y hue) color-point?
    (info color-point.info))

(set 'cp (make-color-point 1 2 'blue))

(test |Record type schemes|
  (is-true (<point?          cp))
  (is-true (<color?          cp))
  (is (= 2 (<point.y         cp)))
  (is (eq 'blue (<color.hue       cp)))
  (is-false (point?           cp))
  (signals (cl:error)
    (point.x          cp))
  (is-true (color-point?     cp))
  (is (eq '<UNDEFINED>
          (color-point.info cp))))

(define-record-type node (make-node left right))
(define-record-type leaf (make-leaf value))

(define-record-type monday)
(define-record-type tuesday :false tuesday?)

(test |Monomorphic update|
  (set 'p (point (x 1) (y 2)))
  (is (= 1 (point.x p)))
  (is (= 7 (point.x (record-update p point (x 7)))))
  (is (= 1 (point.x p))))

(test |Polymorphic update|
  (set 'cp (color-point (hue 'blue) (x 1) (y 2)))
  (is (equal (let ((hue (<color.hue cp))
                   (x (<point.x cp))
                   (y (<point.y cp))
                   (info (color-point.info cp)))
               (list info hue x y))
             '(<UNDEFINED> BLUE 1 2)))
  (is (equal (let* ((cp (record-update cp <point (x 7)))
                    (hue (<color.hue cp))
                    (x (<point.x cp))
                    (y (<point.y cp))
                    (info (color-point.info cp)))
               (list info hue x y))
             '(<UNDEFINED> BLUE 7 2))))

;=================================================================================
; Examples:

; A simple record declaration:
#|(test |A simple record declaration|
  (let ((p (make-pt 1 2)))
    (is-true (pt? p))
    (is (= 2 (pt.y p)))
    (pt.y-set! p 7)
    (is (= 7 (pt.y p)))))|#

#|(test |foo|
  (let ((cp (make-color-point 1 2 'blue)))
    (is-true (<point? cp))
    (is-true (<color? cp))
    (is-true (color-point? cp))
    (signals (cl:error)
      (point.x cp) )
    (is (= 2 (<point.y cp)))
    (is (eq 'blue (<color.hue cp)))
    (is (eq '<UNDEFINED>
            (color-point.extra cp)))))|#

#|(test |Constructing records by field labels|
  (let ((p (point (x 1) (y 2)))
        (cp (color-point (hue 'blue) (x 1) (y 2))))

    (is (equal (show (point (x 1) (y 2)))
               '(POINT (X 1) (Y 2))))))|#

;;;| ;|
;;;| ;|; Monomorphic functional update:
;;;| ;|
;;;| ;|(show
;;;| ;| (record-update p point (x 7)))     ;==> (point (x 7) (y 2))
;;;| ;|(show p)                            ;==> (point (x 1) (y 2))   - original unaffected
;;;| ;|
;;;| ;|; Polymorphic functional update:
;;;| ;|
;;;| ;|(show
;;;| ;| (record-update cp <point (x 7)))   ;==> (color-point (extra <undefined>) (hue blue) (x 7) (y 2))
;;;| ;|(show cp)                           ;==> (color-point (extra <undefined>) (hue blue) (x 1) (y 2))
;;;| ;|
;;;| ;|; In-place update:
;;;| ;|
;;;| ;|(show
;;;| ;| (record-update! cp <point (x 7)))  ;==> color-point (extra <undefined>) (hue blue) (x 7) (y 2))
;;;| ;|(show cp)                           ;==> color-point (extra <undefined>) (hue blue) (x 7) (y 2))
;;;| ;|
;;;| ;|; Use record-compose for updates polymorphic in argument but monomorphic in result type:
;;;| ;|
;;;| ;|(show
;;;| ;| (record-compose (<point cp) (point (x 8))))  ;==> (point (x 8) (y 2))
;;;| ;|(show cp)                                     ;==> (color-point (extra <undefined>) (hue blue) (x 7) (y 2))
;;;| ;|
;;;| ;|; More general record composition example:
;;;| ;|
;;;| ;|(define cp (make-color-point 1 2 'green))
;;;| ;|(define c  (make-color 'blue))
;;;| ;|
;;;| ;|(show
;;;| ;| (record-compose (<point cp)                 ; polymorphic import - only fields x and y of cp taken
;;;| ;|                 (color c)                   ; monomorphic import
;;;| ;|                 (color-point (x 8)          ; override imported field
;;;| ;|                              (extra 'hi))))
;;;| ;|
;;;| ;|                                         ;==> (color-point (extra hi) (hue blue) (x 8) (y 2))
;;;| ;|
;;;| ;|; Small module-functor example:
;;;| ;|
;;;| ;|(define-record-type monoid #f #f
;;;| ;|  (mult monoid.mult)
;;;| ;|  (one  monoid.one))
;;;| ;|
;;;| ;|(define-record-type abelian-group #f #f
;;;| ;|  (add  group.add)
;;;| ;|  (zero group.zero)
;;;| ;|  (sub  group.sub))
;;;| ;|
;;;| ;|(define-record-type ring #f #f
;;;| ;|  (mult ring.mult)
;;;| ;|  (one  ring.one)
;;;| ;|  (add  ring.add)
;;;| ;|  (zero ring.zero)
;;;| ;|  (sub  ring.sub))
;;;| ;|
;;;| ;|(define integer-monoid (monoid (mult *)
;;;| ;|                               (one  1)))
;;;| ;|
;;;| ;|(define integer-group (abelian-group (add  +)
;;;| ;|                                     (zero 0)
;;;| ;|                                     (sub  -)))
;;;| ;|
;;;| ;|(define (make-ring g m)          ; simple "functor"
;;;| ;|  (record-compose (monoid m)
;;;| ;|                  (abelian-group g)
;;;| ;|                  (ring)))
;;;| ;|
;;;| ;|(define integer-ring (make-ring integer-group
;;;| ;|                                integer-monoid))
;;;| ;|
;;;| ;|((ring.add integer-ring) 1 2)    ;==> 3
;;;| ;|
;;;|

#|(test |Example of tree data type|
  (let ((tr (make-node (make-node (make-leaf 1)
                                  (make-leaf 2) )
                       (make-leaf 3) )))
    #|(<tree? tr)|# ;;;;foooo!!
    (is (equal (tree->list tr)
               '((1 . 2) . 3) ))))|#

;;; eof
