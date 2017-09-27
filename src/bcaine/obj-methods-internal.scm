;; =============================================================================
;; specific methods
;; =============================================================================

(defstruct method
  arg-count ; either a number or (+ number)
  arg-classes
  func)

(define (correct-method-arg-count? meth args)
  (if (number? (method-arg-count meth))
      (= (method-arg-count meth) (length args))
      (>= (length args) (cadr (method-arg-count meth)))))

(define (same-matching-method? a b)
  (let ((a-classes (method-arg-classes a))
        (b-classes (method-arg-classes b)))
    (and (= (length a-classes) (length b-classes))
         (every eq? a-classes b-classes)
         #t)))

(define (method-matches-class-chains? meth chains)
  (and (every eq? (method-arg-classes meth) (map class-chain-current chains))
       #t))

(define (call-method meth args)
  (unless (correct-method-arg-count? meth args)
          (error "Bad argument count for method" meth args))
  (apply (method-func meth) args))

;; =============================================================================
;; generic procedures
;; =============================================================================

(defstruct generic-procedure
  name
  typed-arg-count
  (primary-methods '())
  (around-methods '())
  (before-methods '())
  (after-methods '()))

;; ok, so
;; we take the classes of the arguments
;; then we look for a match
;; if there is no match, we advance the classes and try again

(define (top-class? x) (eq? x #t))

(define next-method-current-cont
  (make-parameter
   (lambda ()
     (error (string-append
             "This currently executing method does not"
             " have access to call-next-method" )))))

(define (call-next-method)
  ((next-method-current-cont)))

(define (split-generic-procedure-args proc args)
  (let ((arg-count (generic-procedure-typed-arg-count proc)))
    (when (< (length args) arg-count)
          (error "Not enough args" proc args))
    (values (take args arg-count)
            (drop args arg-count))))

(define (class-chain-current x) (car x))
(define (class-chain-rest x) (cdr x))
(define (class-chain-top? x) (top-class? (class-chain-current x)))
(define (class-chain-almost-empty? x) (= 1 (length x)))

(define (advance-arg-inheritance-chains original current)
  (cond
   ((null? original) '())
   ((every class-chain-almost-empty? current) original)
   ((= (length current) 1)
    (list (class-chain-rest (car current))))
   (else
    (if (every class-chain-almost-empty? (cdr current))
        (cons (class-chain-rest (car current)) (cdr original))
        (cons (car current)
              (advance-arg-inheritance-chains (cdr original) (cdr current)))))))

(define (matching-method methods chains)
  (find (lambda (method) (method-matches-class-chains? method chains)) methods))

(define (lambda->default-method func arg-count)
  (make-method 'arg-count '(+ 0)
               'arg-classes (make-list arg-count #t)
               'func func))

(define (lambda->wrapped-generic-procedure func arg-count)
  (let ((res (wrap-generic-procedure
              (make-generic-procedure 'typed-arg-count arg-count))))
    (update-wrapped-proc! res 'primary
                          (lambda->default-method func arg-count))
    res))

(define class-inheritance-chain #f)
(define class-of #f)

(define (generic-procedure-original-chains proc args reverse-chains?)
  (let-values (((typed rest) (split-generic-procedure-args proc args)))
    (map (compose (if reverse-chains? reverse identity)
                  (lambda (x)
                    (if (eq? x #t)
                        (list #t)
                        (class-inheritance-chain x)))
                  class-of)
         typed)))

(define (next-matching-method methods original current)
  (let ((meth (matching-method methods current)))
    (if meth
        (values meth current)
        (if (every class-chain-almost-empty? current)
            (values #f current)
            (next-matching-method
             methods original
             (advance-arg-inheritance-chains original current))))))

(define (iter-method-chain proc methods args reverse? func)
  (let ((original (generic-procedure-original-chains proc args reverse?)))
    (let iter ((current original))
      (let ((meth (matching-method methods current)))
        (let-values (((done? ret)
                      (if meth
                          (func meth current)
                          (values #f #f))))
          (cond
           (done? ret)
           ((every class-chain-almost-empty? current) #f)
           (else
            (iter (advance-arg-inheritance-chains original current)))))))))

(define (call-before-methods proc args)
  (unless (null? (generic-procedure-before-methods proc))
    (iter-method-chain
     proc (generic-procedure-before-methods proc) args #f
     (lambda (method current)
       (define (done!) (values #t #f))
       (define (continue!) (values #f #f))
       (call-method method args)
       (if (every class-chain-top? current)
           (done!)
           (continue!))))))

(define (call-after-methods proc args)
  (unless (null? (generic-procedure-after-methods proc))
    (iter-method-chain
     proc (generic-procedure-after-methods proc) args #t
     (lambda (method current)
       (define (done!) (values #t #f))
       (define (continue!) (values #f #f))
       (call-method method args)
       (if (every class-chain-almost-empty? current)
           (done!)
           (continue!))))))

(define (call-primary-methods proc args)
  (let ((original (generic-procedure-original-chains proc args #f))
        (primary (generic-procedure-primary-methods proc)))
    (let iter ((current original))
      (let-values (((method current)
                    (next-matching-method primary original current)))
        (call/cc
         (lambda (return)
           (parameterize ((next-method-current-cont
                           (lambda ()
                             (return
                              (iter
                               (advance-arg-inheritance-chains
                                original current))))))
           (if method
               (return (call-method method args))
               (error "No matching primary method" proc args)))))))))

(define (call-around-methods proc args)
  (let ((original (generic-procedure-original-chains proc args #f))
        (around (generic-procedure-around-methods proc)))
    (let iter ((current original)
               (looped? #f))
      (let-values (((method current)
                    (next-matching-method around original current)))
        (parameterize ((next-method-current-cont
                        (lambda ()
                          (let ((next (advance-arg-inheritance-chains
                                       original current)))
                            (iter next
                                  (every eq? next original))))))
          (if (and method (not looped?))
              (call-method method args)
              (call-primary-methods proc args)))))))

(define (call-primary-around-methods proc args)
  (call-around-methods proc args))

(define (apply-generic-procedure proc args)
  (call-before-methods proc args)
  (let ((res (call-primary-around-methods proc args)))
    (call-after-methods proc args)
    res))

;; =============================================================================
;; wrapped generic procedures
;; =============================================================================

(define *name->proc* (make-hash-table equal? hash))
(define *wrapped->name* (make-hash-table eq? hash-by-identity))

(define (wipe-all-methods!)
  (set! *name->proc* (make-hash-table equal? hash))
  (set! *wrapped->name* (make-hash-table eq? hash-by-identity)))

(define (add-generic-procedure! name proc)
  (hash-table-set! *name->proc* name proc))

(define (generic-procedure-by-name name)
  (hash-table-ref/default *name->proc* name #f))

(define (add-wrapped-generic-procedure! wrapped name)
  (hash-table-set! *wrapped->name* wrapped name))

(define (name-by-wrapped-generic-procedure wrapped)
  (hash-table-ref/default *wrapped->name* wrapped #f))

(define (wrapped-generic-procedure-proc func)
  (and-let* ((name (name-by-wrapped-generic-procedure func))
             (proc (generic-procedure-by-name name)))
    proc))

(define (add-setter-to-wrapped-if-necessary proc func)
  (let ((proc-name (generic-procedure-name proc)))
    ;; if we're already dealing with a setter, just return the func as is
    (if (not (symbol? proc-name))
        func
        (let ((existing-setter-proc
               (generic-procedure-by-name (list 'setter proc-name))))
          (if existing-setter-proc
              (getter-with-setter
               func
               (wrap-generic-procedure existing-setter-proc))
              func)))))

(define (wrap-generic-procedure proc)
  (let* ((res (lambda args (apply-generic-procedure proc args)))
         (res (add-setter-to-wrapped-if-necessary proc res)))
    (add-generic-procedure! (generic-procedure-name proc) proc)
    (add-wrapped-generic-procedure! res (generic-procedure-name proc))
    res))

(define (qualifier-getter qualifier)
  (cond
   ((eq? qualifier 'primary) generic-procedure-primary-methods)
   ((eq? qualifier 'around) generic-procedure-around-methods)
   ((eq? qualifier 'before) generic-procedure-before-methods)
   ((eq? qualifier 'after) generic-procedure-after-methods)
   (else
    #f)))

(define (update-wrapped-proc! func qualifier method)
  (let ((proc (wrapped-generic-procedure-proc func))
        (getter (qualifier-getter qualifier)))
    ((setter getter)
     proc
     (let iter ((existing (getter proc))
                (front '())
                (found? #f))
       (cond
        ((and (null? existing) found?) front)
        ((null? existing) (cons method front))
        ((same-matching-method? method (car existing))
         (append front (cdr existing) (list method)))
        (else
         (iter (cdr existing) (cons (car existing) front) #f)))))
    func))

(define (wrapped-generic-procedure? proc)
  (and (procedure? proc)
       (wrapped-generic-procedure-proc proc)
       #t))

(define (wrapped-generic-procedure->alist proc)
  (map (lambda (pair)
         (if (member (car pair)
                     '(primary-methods
                       around-methods before-methods after-methods))
             `(,(car pair) . ,(map method->alist (cdr pair)))
             pair))
       (generic-procedure->alist (wrapped-generic-procedure-proc proc))))

(define (valid-wrapped-generic-procedure? x)
  (and (wrapped-generic-procedure? x)
       (valid-generic-procedure? (wrapped-generic-procedure-proc x))))

(define-syntax handle-exceptions
  (syntax-rules ()
    ((handle-exceptions exn handler body ...)
     (call/cc
      (lambda (ret)
        (with-exception-handler
         (lambda (exn) (ret handler))
         (lambda () body ...)))))))

(define (valid-generic-procedure? x)
  (and (generic-procedure? x)
       (handle-exceptions
        exn #f
        (generic-procedure-typed-arg-count x)
        #t)))

(define (make-wrapped-generic-procedure name arg-count)
  (wrap-generic-procedure
   (make-generic-procedure 'name name 'typed-arg-count arg-count)))

(define (ensure-wrapped-generic-procedure name arg-count)
  (or (and-let* ((proc (generic-procedure-by-name name)))
        (wrap-generic-procedure proc))
      (make-wrapped-generic-procedure name arg-count)))

;; =============================================================================
;; tests
;; =============================================================================

(import (chibi test))

(define (test-obj-methods-internal)
  (test-group
   "methods-internal"

   (test-group
    "generic-procedure"

    (wipe-all-methods!)

    (test-assert
        "ensure-wrapped-generic-procedure returns procedure"
      (procedure? (ensure-wrapped-generic-procedure 'foo '(+ 3))))

    (wipe-all-methods!)

    (test-assert
        "ensure-wrapped-generic-procedure returns same procedure twice"
      (let* ((first  (ensure-wrapped-generic-procedure 'bar '(+ 3)))
             (second (ensure-wrapped-generic-procedure 'bar '(+ 3))))
        (eq? (wrapped-generic-procedure-proc first)
             (wrapped-generic-procedure-proc second))))

    (wipe-all-methods!)

    (test-assert
        "ensure-wrapped-generic-procedure returns different procedure after wiping"
      (let* ((first  (ensure-wrapped-generic-procedure 'bar '(+ 3)))
             (_      (wipe-all-methods!))
             (second (ensure-wrapped-generic-procedure 'bar '(+ 3))))
        (not
         (eq? (wrapped-generic-procedure-proc first)
              (wrapped-generic-procedure-proc second)))))

    (wipe-all-methods!)

    (test-assert
        "if the original generic has a setter, a second generic for the same name should too"
      (let* ((first (ensure-wrapped-generic-procedure 'bar 3))
             (first-setter (ensure-wrapped-generic-procedure '(setter bar) 4))
             (second (ensure-wrapped-generic-procedure 'bar 3)))
        (equal?
         '(setter bar)
         (name-by-wrapped-generic-procedure (setter second)))))

    )

   (newline)))
