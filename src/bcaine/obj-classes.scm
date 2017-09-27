;; ok, so, what do we want:

;; (class-of *instance or instance-like*) => its class

;; (class-of 3)              => #t
;; (class-of *cat instance*) => <cat>
;; (class-of <cat>)          => <class>
;; (class-of <class>)        => <class>
;; (class-of <instance>)     => <class>

;; with primitive classes:
;; (class-of 'foo)           => <symbol>
;; (class-of <symbol>)       => <primitive-class>
;; (class-of <primitive-class>) => <class>

;; now given a class, the inheritance chain is the list of classes the class
;; inherits from

;; given an instance, the instance *is* also all of the things in
;; the inheritance chain of its class

;; so if you have #<instance of cat>, it's a <cat> and an <instance> and #t
;; if you have <class>, it's a <class> and #t

;; (class-inheritance-chain <cat>)      => (<cat> <instance> #t)
;; (class-inheritance-chain <instance>) => (<instance> #t)
;; (class-inheritance-chain <class>)    => (<class> #t)
;; (class-inheritance-chain <symbol>)   => (<symbol> <primitive-value> #t)

(set! class-inheritance-chain (lambda () (list #t)))
(define-generic (class-name obj))

;; basics:

(define *primitive-classes* '())

(define (add-primitive-class! predicate allocator class)
  (set! (class-int-allocator class) (or allocator (class-int-allocator class)))
  (set! *primitive-classes*
        (cons (cons predicate class) *primitive-classes*)))

(set! class-of
      (lambda (obj)
        (let iter ((pairs *primitive-classes*))
          (or (and (pair? pairs) ((caar pairs) obj) (cdar pairs))
              (and (null? pairs) #t)
              (iter (cdr pairs))))))

(defstruct class
  int-name
  int-inheritance-chain
  int-slot-prototypes

  ;; primitive class stuff
  (int-allocator basic-class-allocator)
)

(define (basic-class-allocator class)
  (make-instance
   'class class
   'slots (map slot-prototype->slot (class-int-slot-prototypes class))))

(define <class> (make-class 'int-name '<class>
                            'int-inheritance-chain '()
                            'int-slot-prototypes '()))

(set! (class-int-inheritance-chain <class>) (list <class> #t))

(add-primitive-class! class? #f <class>)

(set! class-inheritance-chain
      (let ((old-class-inheritance-chain class-inheritance-chain))
        (lambda (obj)
          (if (class? obj)
              (class-int-inheritance-chain obj)
              (old-class-inheritance-chain obj)))))

(define-method (class-name (obj <class>))
  (class-int-name obj))

(define-method (class-name (obj #t))
  #t)

(defstruct instance
  class
  (slots '()))

(set! class-of
      (let ((old-class-of class-of))
        (lambda (obj)
          (if (instance? obj)
              (instance-class obj)
              (old-class-of obj)))))

(define <instance> (make-class 'int-name '<instance>
                               'int-inheritance-chain '()
                               'int-slot-prototypes '()))

(set! (class-int-inheritance-chain <instance>) (list <instance> #t))

(define <primitive-value>
  (make-class 'int-name '<primitive-value>
              'int-inheritance-chain '()
              'int-slot-prototypes '()))

(set! (class-int-inheritance-chain <primitive-value>)
      (list <primitive-value> #t))

(define <primitive-class>
  (make-class 'int-name '<primitive-class>
              'int-inheritance-chain '()
              'int-slot-prototypes '()))

(set! (class-int-inheritance-chain <primitive-class>)
      (list <primitive-class> <class> #t))

(set! class-of
      (let ((old-class-of class-of))
        (lambda (obj)
          (if (eq? obj <primitive-class>)
              <primitive-class>
              (old-class-of obj)))))

(define-generic (class-instance-root-classes class))
;; class-instance-root-classes

;; when you create an instance of a class, the root classes
;; (for class-inheritance-chain, for example) are defined by
;; class-instance-root-classes

;; for <class> this should be (<instance> #t)

(define-method (class-instance-root-classes (class <class>))
  (list <instance> #t))

(define-method (class-instance-root-classes (class <primitive-class>))
  (list <primitive-value> #t))

;; the protocol

(define-generic (create-class name metaclass superclasses slots))

(define (initial-inheritance-chain mc superclasses)
  (delete-duplicates
   (append superclasses
           (concatenate
            (map (compose cdr class-inheritance-chain) superclasses))
           (class-instance-root-classes mc))))

(define (superclass-slot-prototypes mc superclasses slots)
  (let ((existing-slots
         (concatenate (map class-int-slot-prototypes superclasses))))
    (append slots
            (filter
             (lambda (slot)
               (not (find (lambda (other-slot)
                            (eq? (slot-prototype-name other-slot)
                                 (slot-prototype-name slot)))
                          slots)))
             existing-slots))))

(define-method (create-class (name #t) (mc <class>) (sc #t) (slots #t))
  (let ((res (make-class
              'int-name name
              'int-inheritance-chain (initial-inheritance-chain mc sc)
              'int-slot-prototypes (superclass-slot-prototypes mc sc slots))))
    (set! (class-int-inheritance-chain res)
          (cons res (class-int-inheritance-chain res)))
    res))

;; slot access

(define-generic (o-slot-ref obj slot-name))
(define-generic (o-slot-set! obj slot-name value))

(set! (setter o-slot-ref) o-slot-set!)

(define (find-slot obj slot-name)
  (let ((res-pair (find-tail (lambda (pair) (eq? slot-name (car pair)))
                             (instance-slots obj))))
    (unless res-pair (error "No such slot" obj slot-name))
    (car res-pair)))

(define-method (o-slot-ref (obj <instance>) (slot-name #t))
  (let ((pair (find-slot obj slot-name)))
    (when (eq? (cdr pair) +uninitialized+)
          (error "Uninitialized slot" obj slot-name))
    (cdr pair)))

(define-method (o-slot-set! (obj <instance>) (slot-name #t) (value #t))
  (let ((pair (find-slot obj slot-name)))
    (set! (cdr pair) value)))

(define-method ((setter o-slot-ref) (obj <instance>) (slot-name #t) (value #t))
  (o-slot-set! obj slot-name value))

;; making instances

(define-generic (allocate class))
(define-generic (initialize obj . args))

;; allocate creates an instance of the class in question

(define (make class . args)
  (let ((res (allocate class)))
    (apply initialize res args)
    res))

(define +uninitialized+ (list #t))

(defstruct slot-prototype
  name
  (init-func (lambda () +uninitialized+)))

(define (slot-prototype->slot sp)
  (cons (slot-prototype-name sp) +uninitialized+))

(define-method (allocate (class <class>))
  ((class-int-allocator class) class))

(define-method (initialize (obj <instance>) . args)
  (for-each
   (lambda (slot)
     (o-slot-set!
      obj
      (slot-prototype-name slot) ((slot-prototype-init-func slot))))
   (fold (lambda (arg-pair slots)
           (unless (length>=? arg-pair 2)
                   (error "Unmatched arguments" obj args))
           (o-slot-set! obj (car arg-pair) (cadr arg-pair))
           (remove (lambda (slot)
                     (eq? (car arg-pair) (slot-prototype-name slot)))
                   slots))
         (class-int-slot-prototypes (class-of obj))
         (chop args 2))))

(define-syntax define-class
  (er-macro-transformer
   (lambda (def rename compare)

     (define (bail-with-error msg . args)
       (apply error
              (with-output-to-string
                (lambda ()
                  (printf "In expanding ~s, there was an error: ~s"
                          def msg)))
              args))

     (define (call-with-slot-info slot func)
       (let* ((slot-name (or (and (pair? slot) (symbol? (car slot)) (car slot))
                             (bail-with-error "Bad slot definition")))
              (slot-rest (cdr slot))

              (pairs (map (lambda (entry)
                            (unless (= (length entry) 2)
                                    (bail-with-error "Bad slot definition"))
                            (unless (member
                                     (car entry)
                                     '(accessor: initform: getter: setter:))
                                    (bail-with-error "Bad slot info"
                                                     entry))
                            (cons (car entry) (cadr entry)))
                          (chop slot-rest 2))))
         (func slot-name
               (assoc 'initform: pairs)
               (assoc 'getter: pairs)
               (assoc 'setter: pairs)
               (assoc 'accessor: pairs))))

     (define (slot->slot-prototype slot)
       (call-with-slot-info
        slot
        (lambda (slot-name initform-pair
                           getter-pair setter-pair accessor-pair)
          (let* ((r-make-slot-prototype (rename 'make-slot-prototype))
                 (r-lambda (rename 'lambda))
                 (r-quote (rename 'quote))
                 (r-+uninitialized+ (rename '+uninitialized+)))
            `(,r-make-slot-prototype
              (,r-quote name) (,r-quote ,slot-name)
              (,r-quote init-func)
                (,r-lambda ()
                  ,(if initform-pair
                       (cdr initform-pair)
                       r-+uninitialized+)))))))

     (define (slot->methods class-name slot)
       (call-with-slot-info
        slot
        (lambda (slot-name initform-pair
                           getter-pair setter-pair accessor-pair)
          (let* ((r-define-method (rename 'define-method))
                 (r-setter (rename 'setter))
                 (r-set! (rename 'set!))
                 (r-o-slot-ref (rename 'o-slot-ref))
                 (r-quote (rename 'quote))
                 (r-begin (rename 'begin))
                 )
            `(,r-begin
              ,(if getter-pair
                   `(,r-define-method (,(cdr getter-pair) (x ,class-name))
                      (,r-o-slot-ref x (,r-quote ,slot-name)))
                   `(,r-begin))
              ,(if setter-pair
                   `(,r-define-method (,(cdr setter-pair)
                                       (x ,class-name) (val #t))
                      (,r-set! (,r-o-slot-ref x (,r-quote ,slot-name)) val))
                   `(,r-begin))
              ,(if accessor-pair
                   `(,r-define-method (,(cdr accessor-pair) (x ,class-name))
                      (,r-o-slot-ref x (,r-quote ,slot-name)))
                   `(,r-begin))
              ,(if accessor-pair
                   `(,r-define-method ((,r-setter ,(cdr accessor-pair))
                                       (x ,class-name) (val #t))
                      (,r-set! (,r-o-slot-ref x (,r-quote ,slot-name)) val))
                   `(,r-begin)))))))

     ;; (define-class <foo> (<bar> <baz>) metaclass: <metaclass>
     ;;  (
     ;;   (slot-a accessor: slot-a-accessor initform: initform
     ;;           getter: slot-a-getter     setter: slot-a-setter)
     ;;   (slot-b accessor: slot-b-accessor initform: initform
     ;;           getter: slot-b-getter     setter: slot-b-setter)
     ;;  )
     ;; )

     ;; =>

     ;; (begin
     ;;   (define <foo> (create-class
     ;;                  '<foo>
     ;;                  <metaclass>
     ;;                  (list <bar> <baz>)
     ;;                  (list (make-slot-prototype
     ;;                         'name 'slot-a
     ;;                         'init-func (lambda () initform)
     ;;                        (make-slot-prototype
     ;;                         'name 'slot-b
     ;;                         'init-func (lambda () initform))))

     ;;   (define-method (slot-a-accessor (x <foo>))
     ;;     (o-slot-ref x 'slot-a))

     ;;   (define-method ((setter slot-a-accessor) (x <foo>) (val #t))
     ;;     (set! (o-slot-ref x 'slot-a) val))

     ;;   (define-method (slot-a-getter (x <foo>))
     ;;     (o-slot-ref x 'slot-a))

     ;;   (define-method (slot-b-setter (x <foo>) (val #t))
     ;;     (set! (o-slot-ref x 'slot-b) val))

     ;;   (define-method (slot-b-accessor (x <foo>))
     ;;     (o-slot-ref x 'slot-b))

     ;;   (define-method ((setter slot-b-accessor) (x <foo>) (val #t))
     ;;     (set! (o-slot-ref x 'slot-b) val))

     ;;   (define-method (slot-b-getter (x <foo>))
     ;;     (o-slot-ref x 'slot-b))

     ;;   (define-method (slot-b-setter (x <foo>) (val #t))
     ;;     (set! (o-slot-ref x 'slot-b) val))
     ;; )

     (let* (
            (class-name (or (and (>= (length def) 2) (cadr def))
                            (bail-with-error "Missing class name")))

            (superclasses (or (and (>= (length def) 3)
                                   (caddr def))
                              (bail-with-error "Superclass list missing")))
            (_ (or (and (list? superclasses) (every identifier? superclasses))
                   (bail-with-error "Bad superclasses")))

            (post-superclasses (cdddr def))

            (post-metaclass
             (if (and (>= (length post-superclasses) 2)
                      (eq? (car post-superclasses) 'metaclass:))
                 (cddr post-superclasses)
                 post-superclasses))

            (metaclass (or (if (eq? post-metaclass post-superclasses)
                               (rename '<class>)
                               (and (symbol? (cadr post-superclasses))
                                    (cadr post-superclasses)))
                           (bail-with-error "Bad metaclass")))

            (slots (or (and (= (length post-metaclass) 1)
                            (list? (car post-metaclass))
                            (car post-metaclass))
                       (and (null? post-metaclass)
                            '())
                       (bail-with-error "Bad slots")))

            ;; renames
            (r-begin        (rename 'begin))
            (r-define       (rename
                             (if (template-testing?) 'mock-define 'define)))
            (r-create-class (rename 'create-class))
            (r-list         (rename 'list))
            (r-quote        (rename 'quote))
            )
       `(,r-begin
         (,r-define ,class-name
                    (,r-create-class (,r-quote ,class-name)
                                     ,metaclass
                                     (,r-list . ,superclasses)
                                     (,r-list .
                                      ,(map slot->slot-prototype slots))))
         . ,(map (lambda (slot) (slot->methods class-name slot)) slots))))))

;; =============================================================================
;; primitive classes
;; =============================================================================

(define-syntax define-primitive-class
  (er-macro-transformer
   (lambda (def rename compare)

     (define (bail-with-error msg . args)
       (apply error
              (with-output-to-string
                (lambda ()
                  (printf "In expanding ~s, there was an error: ~s"
                          def msg)))
              args))

     ;; (define-primitive-class class (superclass-a) metaclass: mclass predicate: predicate allocator: allocator)

     ;; =>

     ;; (begin
     ;;   (define-class class (superclass-a) metaclass: mclass)
     ;;   (add-primitive-class! predicate allocator class)
     ;; )

     ;; mclass defaults to <primitive-class>

     (let* (
            (class-name (or (and (>= (length def) 2) (cadr def))
                            (bail-with-error "Missing class name")))
            (post-class-name (cddr def))

            (superclasses (or (and (pair? post-class-name)
                                   (list? (car post-class-name))
                                   (car post-class-name))
                              (bail-with-error "Bad superclasses")))
            (post-superclasses (cdr post-class-name))
            (_ (unless (pair? post-superclasses)
                       (bail-with-error "Missing predicate")))

            (pair-args
             (map (lambda (pair)
                    (if (identifier? (car pair))
                        (cons (identifier->symbol (car pair)) (cdr pair))
                        pair))
                  (chop post-superclasses 2)))

            (metaclass
             (or (and-let* ((entry (alist-ref 'metaclass: pair-args))
                            (_ (= (length entry) 1)))
                   (car entry))
                 '<primitive-class>))

            (predicate
             (or (and-let* ((entry (alist-ref 'predicate: pair-args))
                            (_ (= (length entry) 1)))
                   (car entry))
                 (error "Missing predicate")))

            (allocator
             (and-let* ((entry (alist-ref 'allocator: pair-args))
                        (_ (= (length entry) 1)))
               (car entry)))

            ;; redefs
            (r-begin               (rename 'begin))
            (r-define-class        (rename 'define-class))
            (r-add-primitive-class! (rename 'add-primitive-class!))
            )
       `(,r-begin
         (,r-define-class ,class-name ,superclasses metaclass: ,metaclass)
         (,r-add-primitive-class! ,predicate ,allocator ,class-name))))))

;; =============================================================================
;; tests
;; =============================================================================

(import (chibi test))

(define (test-obj-classes)

  (test-group "classes"
              #f)
)
