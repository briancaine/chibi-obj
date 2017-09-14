(import (chibi ast))

(define-syntax define-method
  (er-macro-transformer
   (lambda (def rename compare)

     (define (bail-with-error msg . args)
       (apply error
              (with-output-to-string
                (lambda ()
                  (printf "In expanding ~s, there was an error: ~s"
                          def msg)))
              args))

     (define (rough-take-while pred lst)
       (let iter ((lst lst)
                  (prefix '()))
         (if (or (null? lst) (not (pair? lst))
                 (not (pred (car lst))))
             (reverse prefix)
             (iter (cdr lst) (cons (car lst) prefix)))))

     (define (rough-drop-while pred lst)
       (if (or (null? lst) (not (pair? lst)) (not (pred (car lst))))
           lst
           (rough-drop-while pred (cdr lst))))

     (define (length+ lst)
       (if (list? lst)
           (length lst)
           (let iter ((lst lst) (count 0))
             (if (symbol? lst)
                 `(+ ,count)
                 (iter (cdr lst) (add1 count))))))
     ;; (define-method (foo qualifier (arg1 typea) (arg2 typeb) arg3 . arg4)
     ;;   body ...)

     ;; =>

     ;; (define foo
     ;;   (update-wrapped-proc!
     ;;    (ensure-wrapped-generic-procedure 'foo '(+ 3))
     ;;    'qualifier
     ;;    (make-method
     ;;     'arg-count '(+ 3)
     ;;     'arg-classes (list typea typeb)
     ;;     'func (lambda (arg1 arg2 arg3 . arg4) body ...)
     ;;    )
     ;;  )
     ;; )

     ;; and...


     ;; (define-method ((setter foo) qualifier
     ;;                 (arg1 typea) (arg2 typeb) arg3 . arg4)
     ;;   body ...)

     ;; =>

     ;; (update-wrapped-proc!
     ;;  (ensure-wrapped-generic-procedure '(setter foo) '(+ 3))
     ;;  'qualifier
     ;;  (make-method
     ;;   'arg-count '(+ 3)
     ;;   'arg-classes (list typea typeb)
     ;;   'func (lambda (arg1 arg2 arg3 . arg4) body ...)
     ;;  )
     ;; )

     (let* ((_ (or (>= (length def) 3)
                   (bail-with-error "bad syntax")))

            (define-method (car def))
            (spec (cadr def))
            (body-list (cddr def))

            (method-name (or (and (pair? spec)
                                  (car spec))
                             (bail-with-error "No method name")))

            (_ (or (symbol? method-name)
                   (and (list? method-name)
                        (= (length method-name) 2)
                        (or
                         ;; todo: this equal? rename setter biz might be
                         ;;       worthless
                         (equal? (car method-name) (rename 'setter))
                         (equal? (car method-name) 'setter)
                         (and (syntactic-closure? (car method-name))
                              (equal?
                               (syntactic-closure-expr (car method-name))
                               'setter))))
                   (bail-with-error "Bad method-name")))

            (setter? (list? method-name))
            (getter-name-for-setter (and setter? (cadr method-name)))

            (has-qualifier?
             (and (pair? (cdr spec)) (symbol? (cadr spec))
                  (member (cadr spec) '(primary around before after))))
            (qualifier
             (if has-qualifier?
                 (cadr spec)
                 'primary))
            (post-qualifier-spec (if has-qualifier? (cddr spec) (cdr spec)))

            (typed-args (rough-take-while pair? post-qualifier-spec))
            (rest-args (rough-drop-while pair? post-qualifier-spec))

            (_ (or (every (lambda (x) (and (list? x) (= (length x) 2)))
                          typed-args)
                   (bail-with-error "Bad typed args")))

            (_ (or (let iter ((args rest-args))
                     (if (pair? args)
                         (and (symbol? (car args))
                              (iter (cdr args)))
                         (or (null? args) (symbol? args))))
                   (bail-with-error "Bad rest args")))

            (lambda-args (append (map car typed-args) rest-args))
            (arg-classes (map cadr typed-args))
            (arg-count (length+ lambda-args))
            (classed-arg-count (length arg-classes))

            ;; renamed symbols
            (r-define (rename 'define))
            (r-set! (rename 'set!))
            (r-setter (rename 'setter))
            (r-ensure-generic-variable (rename 'ensure-generic-variable))
            (r-ensure-generic-procedure (rename 'ensure-generic-procedure))
            (r-ensure-wrapped-generic-procedure
             (rename 'ensure-wrapped-generic-procedure))
            (r-quote (rename 'quote))
            (r-update-wrapped-proc! (rename 'update-wrapped-proc!))
            (r-list (rename 'list))
            (r-lambda (rename 'lambda))
            (r-begin (rename 'begin))
            (r-temp (rename 'temp))
            (r-make-method (rename 'make-method))

            (update-wrapped-proc-expansion
             `(,r-update-wrapped-proc!
               (,r-ensure-wrapped-generic-procedure
                (,r-quote ,method-name) (,r-quote ,arg-count))
               (,r-quote ,qualifier)
               (,r-make-method
                (,r-quote arg-count) (,r-quote ,arg-count)
                (,r-quote arg-classes) ,(cons r-list arg-classes)
                (,r-quote func) (,r-lambda ,lambda-args . ,body-list))))
            )
       (if setter?
           `(,r-set! (,r-setter ,getter-name-for-setter)
                     ,update-wrapped-proc-expansion)
           `(,r-define ,method-name ,update-wrapped-proc-expansion))))))
;; we want:
;; (define-generic name a b c)
;;
;; (define name (ensure-wrapped-generic-procedure 'name (syntax-length . rest)))

(define-syntax syntax-length
  (er-macro-transformer
   (lambda (def rename compare)

     ;; (syntax-length)
     ;; =>
     ;; 0

     ;; (syntax-length a b c)
     ;; =>
     ;; 3

     ;; (syntax-length a b . c)
     ;; =>
     ;; (+ 2)

     (let* ((_ (or (pair? def)
                   (bail-with-error "bad syntax")))

            (syntax-length (car def))
            (body          (cdr def))
            )
       (let iter ((body  body)
                  (count 0))
         (cond
          ((null? body) count)
          ((pair? body) (iter (cdr body) (+ count 1)))
          (else         `(+ ,count))))))))

(define-syntax define-generic
  (syntax-rules ()
    ((define-generic ((setter name) . rest))
     (set! (setter name)
           (ensure-wrapped-generic-procedure
            '(setter name) (syntax-length . rest))))
    ((define-generic (name . rest))
     (define name
       (ensure-wrapped-generic-procedure 'name (syntax-length . rest))))))

;; =============================================================================
;; tests
;; =============================================================================

(import (chibi test))

(define (test-obj-methods-syntax)
  #f)
