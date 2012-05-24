;;;; srfi-57.lisp

(cl:in-package :srfi-57.internal)

(def-suite srfi-57)

(in-suite srfi-57)

;============================================================================================
; IMPLEMENTATION:
;
; Andre van Tonder, 2004.
;
;============================================================================================

(define-syntax define-record-type
  (syntax-rules (:false)
    ((define-record-type . body)
     (parse-declaration :false . body))))

(define-syntax define-record-scheme
  (syntax-rules (:true)
    ((define-record-scheme . body)
     (parse-declaration :true . body))))

(define-syntax parse-declaration
  (syntax-rules (:false)
    ((parse-declaration is-scheme? (name super ***) constructor-clause predicate field-clause ***)
     (build-record 0 constructor-clause (super ***) (field-clause ***) name predicate is-scheme?))
    ((parse-declaration is-scheme? (name super ***) constructor-clause)
     (parse-declaration is-scheme? (name super ***) constructor-clause :false))
    ((parse-declaration is-scheme? (name super ***))
     (parse-declaration is-scheme? (name super ***) :false :false))
    ((parse-declaration is-scheme? name . rest)
     (parse-declaration is-scheme? (name) . rest))))

(define-syntax record-update!
  (syntax-rules ()
    ((record-update! record name (label exp) ***)
     (with ((r (gensym)))
       (meta
        (quasiquote
         (let ((r record))
         ((meta (unquote (name ("setter") label))) r exp)
         ***
         r )))))))

(define-syntax record-update
  (syntax-rules ()
    ((record-update record name (label exp) ***)
     (with ((new (gensym "NEW-")))
       (name ("is-scheme?")
             (meta
              (quasiquote
               (let ((new ((meta (unquote (name ("copier")))) record)))
                 (record-update! new name (label exp) ***) )))
             (record-compose (name record) (name (label exp) ***)) )))))

(define-syntax record-compose
  (syntax-rules ()
    ((record-compose (export-name (label exp) ***))
     (export-name (label exp) ***))
    #|((record-compose (import-name record) *** (export-name (label exp) ***))
     (help-compose 1 (import-name record) *** (export-name (label exp) ***)))|#
    ((record-compose expr ***)
     (help-compose 1 expr ***))))

#|(defmacro record-compose-aux (expr)
  `(help-compose 1 ,@(butlast expr) (last expr)))|#

#|(define-syntax help-compose
  (syntax-rules ()
    ((help-compose 1 (import-name record) import *** (export-name (label exp) ***))
     (meta
      (quasiquote
       (help-compose 2
                     (meta (unquote (intersection
                                     (meta (unquote (export-name ("labels"))))
                                     (meta (unquote (remove-from (meta (unquote (import-name ("labels"))))
                                                                 (label ***)
                                                                 if-free= )))
                                     if-free= )))
                     (import-name record)
                     import ***
                     (export-name (label exp) ***) ))))
    ((help-compose 2
                   (copy-label ***)
                   (import-name record)
                   import ***
                   (export-name . bindings) )
     (with ((r (gensym "help-compose r ")))
       (meta
        (quasiquote
         (let ((r record))
           (record-compose import ***
                           (export-name
                            (copy-label
                             ((meta
                               (unquote (import-name ("getter") copy-label)) )
                              r)) ***
                              . bindings)))))))))|#

;; (help-compose 1 (import-name record) import *** (export-name (label exp) ***))
;; (help-compose 2 (copy-label ***) (import-name record) import *** (export-name . bindings))

(defmacro help-compose (&rest exprs)
  (ecase (car exprs)
    (1 (destructuring-bind (no import-name+record &rest rests)
                           exprs
         (declare (ignore no))
         (destructuring-bind (export-name &rest label-exp)
                             (car (last rests))
           `(meta (quasiquote
                   (help-compose 2
                                 (meta (unquote (intersection
                                                 (meta (unquote (,export-name ("labels"))))
                                                 (meta (unquote (remove-from (meta (unquote (,(car import-name+record) ("labels"))))
                                                                             (label ,@(mapcar #'car label-exp))
                                                                             if-free= )))
                                                 if-free= )))
                                 ,import-name+record
                                 ,@(cdr (butlast rests))
                                 (,export-name ,@label-exp) ))))))
    (2 (destructuring-bind (no copy-labels import-name+record &rest rests)
                           exprs
         (declare (ignore no))
         (destructuring-bind (export-name &rest label-exp)
                             (car (last rests))
           (let ((r (gensym "help-compose r ")))
             `(meta (quasiquote
                     (let ((,r ,(cadr import-name+record)))
                       (record-compose ,@(cdr (butlast rests))
                                       (,export-name
                                        ,@(mapcar (lambda (copy-label)
                                                    `(,copy-label
                                                      ((meta (unquote (,(car import-name+record) ("getter") ,copy-label)) )
                                                       ,r)))
                                                  copy-labels)
                                        ,@label-exp)))))))))))

(define-syntax build-record
  (syntax-rules (:false)
    ((build-record 0 (constructor . pos-labels) . rest)              ; extract positional labels from constructor clause
     (build-record 1 (constructor . pos-labels) pos-labels . rest) )  ;
    ((build-record 0 constructor . rest)                             ;
     (build-record 1 (constructor . :false) () . rest) )                  ;
    ((build-record 1 constructor-clause (pos-label ***) (super ***)
                   ((label . accessors) ***) . rest)
     (meta
      (quasiquote
       (build-record 2
                     constructor-clause
                     (meta (unquote (union (meta (unquote (super ("labels")))) ; compute union of labels from supers,
                                           *** ; constructor clause and field clauses
                                           (pos-label ***)
                                           (label ***)
                                           top..if-free= )))
                     ((label . accessors) ***)
                     (meta  (unquote (union (meta (unquote (super ("supers")))) ; compute transitive union of supers
                                            ***
                                            top..if-free= )))
                     . rest))))
    ((build-record 2 (constructor . pos-labels) labels . rest)      ; insert default constructor labels if not given
     (syntax-if pos-labels
                (build-record 3 (constructor . pos-labels) labels . rest)
                (build-record 3 (constructor . labels)     labels . rest) ))
    ((build-record 3 constructor-clause labels ((label . accessors) ***) . rest)
     (meta
      (quasiquote
       (build-record 4
                     (meta (unquote (remove-from labels ; separate the labels that do not appear in a
                                                 (label ***) ; field clause for next step
                                                 top..if-free= )))
                     ((label . accessors) ***)
                     constructor-clause
                     labels
                     . rest))))
    ((build-record 4
                   (undeclared-label ***)
                   (field-clause ***)
                   (constructor . pos-labels)
                   labels
                   supers
                   name
                   predicate
                   is-scheme? )
     (meta
      (quasiquote
       (build-record 5                                              ; generate identifiers for constructor, predicate
                     is-scheme?                                     ; getters and setters as needed
                     name
                     supers
                     supers
                     labels
                     (meta (unquote (to-identifier constructor)))
                     (meta (unquote (add-temporaries pos-labels))) ; needed for constructor below
                     (meta (unquote (to-identifier predicate)))
                     (meta (unquote (augment-field field-clause)))
                     ***
                     (undeclared-label (meta (unquote (generate-identifier)))
                                       (meta (unquote (generate-identifier))) )
                     *** ))))
    ((build-record 5
                   is-scheme?
                   name
                   (super ***)
                   supers
                   (label ***)
                   constructor
                   ((pos-label pos-temp) ***)
                   predicate
                   (field-label getter setter)
                   *** )

     (with ((copy (gentemp "copy-"))
            (internal-name (gentemp "internal-name-"))
            (maker (gentemp "maker-")))
       (eval-when (:compile-toplevel :load-toplevel :execute) ;; begin
         (syntax-if is-scheme?

                    (eval-when (:compile-toplevel :load-toplevel :execute) ;;begin
                      (define-generic (predicate x) (constantly :false))
                      (define-generic (getter x))
                      ***
                      (define-generic (setter x v))
                      ***
                      (define-generic (copy x)) )

                    (eval-when (:compile-toplevel :load-toplevel :execute) ;; begin
                      #|(eval-when (:compile-toplevel :load-toplevel :execute)
                        ;; remove previous definition FIXME
                        (let ((fields '((predicate)
                                        (name)
                                        (field-label getter setter) ***)))
                          (mapc (lambda (x)
                                  (and (fboundp x)
                                       (fmakunbound x)))
                                (append (mapcar (lambda (x)
                                                  `(setf ,(second x)))
                                                fields)
                                        (mapcan #'cdr fields)))))|#
                      (srfi-9:define-record-type internal-name
                          (maker field-label ***)
                        predicate
                        (field-label getter setter) ***)

                      (define-function constructor
                        (lambda (pos-temp ***)
                          (populate 1 maker (field-label ***) (pos-label pos-temp) ***) ))

                      (extend-predicates supers predicate)
                      (extend-accessors supers field-label predicate getter setter)
                      ***

                      (define-function (copy x)
                        (maker (getter x) ***) )
                      (extend-copiers supers copy predicate)
                      (define-method (show (r predicate))
                            (list 'name
                                  (list 'field-label (getter r))
                                  *** ))))

         (define-syntax name
           (syntax-rules (field-label *** :false)
             ((name ("is-scheme?") sk fk)     (syntax-if is-scheme? sk fk))
             ((name ("predicate") k)          (syntax-apply k predicate))
             ((name ("supers") k)             (syntax-apply k (super *** name)))
             ((name ("labels") k)             (syntax-apply k (label ***)))
             ((name ("pos-labels") k)         (syntax-apply k (pos-label ***)))
             ((name ("getter") field-label k) (syntax-apply k getter))
             ***
             ((name ("getter") other k)       (syntax-apply k :false))
             ((name ("setter") field-label k) (syntax-apply k setter))
             ***
             ((name ("setter") other k)       (syntax-apply k :false))
             ((name ("copier") k)             (syntax-apply k copy))
             ((name . bindings)               (populate 1 maker (field-label ***) . bindings)) )))))))


(define-syntax to-identifier
  (syntax-rules (:false)
    ((to-identifier k)
     (with ((generated-identifier (gensym "to-identifier:g0:")))
       (syntax-apply k generated-identifier)))
    ((to-identifier :false k)
     (with ((generated-identifier (gensym "to-identifier:g1:")))
       (syntax-apply k generated-identifier)))
    ((to-identifier id k)
     (syntax-apply k id))))

(define-syntax augment-field
  (syntax-rules ()
    ((augment-field (label) k)
     (with ((generated-getter (gentemp "augment-field generated-getter "))
            (generated-setter (gentemp "augment-field generated-setter ")))
       (syntax-apply k (label generated-getter generated-setter))))
    ((augment-field (label getter) k)
     (with ((generated-getter (gentemp "augment-field generated-getter "))
            (generated-setter (gentemp "augment-field generated-setter ")))
       (meta (quasiquote
              (label (meta (unquote (to-identifier getter))) generated-setter))
             k)))
    ((augment-field (label getter setter) k)
     (meta (quasiquote
            (label (meta (unquote (to-identifier getter)))
                   (meta (unquote (to-identifier setter))))) k))))

(define-syntax extend-predicates
  (syntax-rules ()
    ((extend-predicates (super ***) predicate)
     (begin
       (meta
        (quasiquote
         (define-method (meta (unquote (super ("predicate"))))
             (predicate)
           (x)
           any? )))
       *** ))))

(define-syntax extend-copiers
  (syntax-rules ()
    ((extend-copiers (super ***) copy predicate)
     (begin
       (meta
        (quasiquote (define-method (meta (unquote (super ("copier"))))
                        (predicate)
                      (x)
                      copy)))
       ***))))

(define-syntax extend-accessors
  (syntax-rules ()
    ((extend-accessors (super ***) label predicate selector modifier)
     (meta
      (quasiquote
       (begin
         (syntax-if (meta (unquote (super ("getter") label)))
                    (define-method (meta (unquote (super ("getter") label)))
                        (predicate)
                      (x)
                      selector )
                    (begin) )
         ***
         (syntax-if (meta (unquote (super ("setter") label)))
                    (define-method (meta (unquote (super ("setter") label)))
                        (predicate any?)
                      (x v)
                      modifier )
                    (begin) )
         *** ))))))

(define-syntax populate
  (syntax-rules ()
    ((populate 1 maker labels . bindings)
     (meta
      (quasiquote
       (populate 2 maker
                 (meta (unquote (order labels bindings ('<undefined>))))))))
    ((populate 2 maker ((label exp) ***))
     (maker exp ***))))

(define-syntax order
  (syntax-rules ()
    ((order (label ***) ((label* . binding) ***) default k)
     (meta
      (quasiquote
       (if-empty? (meta (unquote (remove-from (label* ***)
                                              (label ***)
                                              if-free= )))
                  (order "emit" (label ***) ((label* . binding) ***) default k)
                  (syntax-error "Illegal labels in" ((label* . binding) ***)
                                "Legal labels are" (label ***))))))
    ((order "emit" (label ***) bindings default k)
     (meta
      (quasiquote
       ((label . (meta (unquote (syntax-lookup label
                                               bindings
                                               if-free=
                                               default ))))
        *** ))
      k ))))


;============================================================================================
; Simple generic functions:

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic (name arg ***))
     (define-generic (name arg ***)
       (lambda (arg ***) (error "Inapplicable method:" 'name
                                "Arguments:" (show arg) *** ))))
    ((define-generic (name arg ***) proc)
     (define-function name (make-generic (arg ***) proc)))))

#|(define-syntax define-method
  (syntax-rules ()
    ((define-method (generic (arg pred?) ***) . body)
     (define-method generic (pred? ***) (arg ***) (lambda (arg ***) . body)))
    ((define-method generic (pred? ***) (arg ***) procedure)
     (let ((next (funcall (generic) 'get-proc))
           (proc procedure))
       (funcall (funcall (generic) 'set-proc)
                (lambda (arg ***)
                  (if (and (pred? arg) ***)
                      (funcall proc arg ***)
                      (funcall (funcall next arg ***)) )))))))|#

(defmacro define-method1 (generic (&rest pred?) (&rest arg) procedure)
  ;; (format t "_______ ~A" (list generic pred? arg procedure))
  (let ((next (gensym "NEXT-"))
        (proc (gensym "PROC-")) )
    `(let ((,next (funcall (,generic) 'get-proc))
           (,proc #',(if (and (consp procedure)
                              (eq 'lambda (car procedure)) )
                         `(cl:lambda ,@(cdr procedure))
                         procedure)))
       (funcall (funcall (,generic) 'set-proc)
                (lambda (,@arg)
                  (if (and ,@(mapcar (lambda (p a) `(,p ,a)) ;???
                                     pred?
                                     arg ))
                      (funcall ,proc ,@arg)
                      (funcall (funcall ,next ,@arg)) ))))))

(define-syntax define-method
  (syntax-rules ()
    ((define-method (generic (arg pred?) ***) . body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-method generic (pred? ***) (arg ***) (lambda (arg ***) . body))))
    ((define-method generic (pred? ***) (arg ***) procedure)
     (progn ;; eval-when (:compile-toplevel :load-toplevel :execute)
       (define-method1 generic (pred? ***) (arg ***) procedure)))))

(define-syntax make-generic
  (syntax-rules ()
    ((make-generic (arg arg+ ***) default-proc)
     (with ((proc (gensym "PROC-")))
       (let ((proc default-proc))
         (case-lambda
          ((arg arg+ ***)
           (funcall proc arg arg+ ***) )
          (()
           (lambda (msg)
             (case msg
               ((get-proc) proc)
               ((set-proc) (lambda (new)
                             (set! proc new) )))))))))))

(define-generic (show x)
  (lambda (x) x))

(define-function any? (constantly T))

;============================================================================================
; Syntax utilities:

#|(define-syntax syntax-error
  (syntax-rules ()))|#

(defmacro syntax-error (&rest args)
  (declare (ignore args))
  `(error "syntax-error"))

(define-syntax syntax-apply
  (syntax-rules ()
    ((syntax-apply (f . args) exp ***)
     (f exp *** . args))))

(define-syntax syntax-cons
  (syntax-rules ()
    ((syntax-cons x rest k)
     (syntax-apply k (x . rest)))))

(define-syntax syntax-cons-after
  (syntax-rules ()
    ((syntax-cons-after rest x k)
     (syntax-apply k (x . rest)))))

(define-syntax if-empty?
  (syntax-rules ()
    ((if-empty? () sk fk)      sk)
    ((if-empty? (h . tee) sk fk) fk)))

(define-syntax add-temporaries
  (syntax-rules ()
    ((add-temporaries lst k)                (add-temporaries lst () k))
    ((add-temporaries () lst-temps k)       (syntax-apply k lst-temps))
    ((add-temporaries (h . tee) (done ***) k)
     (with ((temp (gensym "TEMP-")))
       (add-temporaries tee (done *** (h temp)) k)))))

(define-syntax if-free=
  (syntax-rules ()
    ((if-free= x y kt kf)
      (let-syntax
          ((test (syntax-rules (x)
                   ((test x kt* kf*) kt*)
                   ((test z kt* kf*) kf*))))
        (test y kt kf)))))

(define-syntax top..if-free=
  (syntax-rules ()
    ((top..if-free= x y kt kf)
     (with ((if-free=..test (gentemp "IF-FREE=..TEST ")))
       (begin
         (define-syntax if-free=..test
           (syntax-rules (x)
             ((if-free=..test x kt* kf*) kt*)
             ((if-free=..test z kt* kf*) kf*) ))
         (if-free=..test y kt kf) )))))

(define-syntax meta
  (syntax-rules (meta quasiquote unquote)
    ((meta (quasiquote (meta (unquote (function argument ***)))) k)
     (meta (quasiquote (argument ***)) (syntax-apply-to function k)))
    ((meta (quasiquote (a . b)) k)
     (meta (quasiquote a) (descend-right b k)))
    ((meta (quasiquote whatever) k) (syntax-apply k whatever))
    ((meta (quasiquote arg))
     (meta (quasiquote arg) (syntax-id)))))

(define-syntax syntax-apply-to
  (syntax-rules ()
    ((syntax-apply-to (argument ***) function k)
     (function argument *** k))))

(define-syntax descend-right
  (syntax-rules ()
    ((descend-right evaled b k)
     (meta (quasiquote b) (syntax-cons-after evaled k)))))

(define-syntax syntax-id
  (syntax-rules ()
    ((syntax-id arg) arg)))

(define-syntax remove-duplicates
  (syntax-rules ()
    ((remove-duplicates lst compare? k)
     (remove-duplicates lst () compare? k))
    ((remove-duplicates () done compare? k)
     (syntax-apply k done))
    ((remove-duplicates (h . tee) (d ***) compare? k)
     (if-member? h (d ***) compare?
                 (remove-duplicates tee (d ***) compare? k)
                 (remove-duplicates tee (d *** h) compare? k)))))

(define-syntax syntax-filter
  (syntax-rules ()
    ((syntax-filter () (if-p? arg ***) k)
     (syntax-apply k ()))
    ((syntax-filter (h . tee) (if-p? arg ***) k)
     (if-p? h arg ***
            (syntax-filter tee (if-p? arg ***) (syntax-cons-after h k))
            (syntax-filter tee (if-p? arg ***) k)))))

(define-syntax if-member?
  (syntax-rules ()
    ((if-member? x () compare? sk fk)
     fk)
    ((if-member? x (h . tee) compare? sk fk)
     (compare? x h
               sk
               (if-member? x tee compare? sk fk)))))

(defmacro remove-duplicates1 (&rest args)
  `(remove-duplicates ,(apply #'append (butlast args 2))
                      ,@(last args 2)))

;;(union (1 2 3 4) (1 2 3 4) eq list)
#|(define-syntax union ;orig
  (syntax-rules ()
    ((union (x ...) ... compare? k)
     (remove-duplicates (x ... ...) compare? k))))|#

(define-syntax union
  (syntax-rules ()
    ((union x ***)
     (remove-duplicates1 x ***))))

(define-syntax intersection
  (syntax-rules ()
    ((intersection list1 list2 compare? k)
     (syntax-filter list1 (if-member? list2 compare?) k))))

(define-syntax remove-from
  (syntax-rules ()
    ((remove-from list1 list2 compare? k)
     (syntax-filter list1 (if-not-member? list2 compare?) k))))

(define-syntax if-not-member?
  (syntax-rules ()
    ((if-not-member? x list compare? sk fk)
     (if-member? x list compare? fk sk))))

(define-syntax generate-identifier
  (syntax-rules ()
    ((generate-identifier k)
     (with ((generated-identifier (gentemp "GENERATED-IDENTIFIER-")))
       (syntax-apply k generated-identifier)))))

(define-syntax syntax-if
  (syntax-rules (:false)
    ((syntax-if :false sk fk)    fk)
    ((syntax-if other sk fk) sk)))

(define-syntax syntax-lookup
  (syntax-rules ()
    ((syntax-lookup label () compare fail k)
     (syntax-apply k fail))
    ((syntax-lookup label ((label* . value) . bindings) compare fail k)
     (compare label label*
              (syntax-apply k value)
              (syntax-lookup label bindings compare fail k)))))

;;; eof
