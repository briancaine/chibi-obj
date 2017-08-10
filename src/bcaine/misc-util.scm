(define (with-output-to-string func)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (func)
      (get-output-string port))))

(define (with-output-to-port port func)
  (parameterize ((current-output-port port))
    (func)))

(define (with-input-from-port port func)
  (parameterize ((current-input-port port))
    (func)))

(define (with-input-from-string str func)
  (with-input-from-port (open-input-string str) func))

(define (void) (if #f #f))

(define (identity x) x)

(define (compose . args)
  (cond
   ((null? args) identity)
   ((null? (cdr args)) (car args))
   (else (lambda func-args
           ((car args) (apply (apply compose (cdr args)) func-args))))))

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define (alist-ref key lst . rest)
  (let ((default (if (null? rest) #f (car rest))))
    (cond
     ((null? lst) default)
     ((equal? (caar lst) key) (cdar lst))
     (else (alist-ref key (cdr lst) default)))))

(define (add1 x) (+ 1 x))
(define (sub1 x) (- x 1))

(define (length>=? lst count)
  (when (negative? count) (error "Can't have a negative length"))
  (if (zero? count)
      #t
      (if (null? lst)
          #f
          (length>=? (cdr lst) (sub1 count)))))

(define (chop lst n)
  (let iter ((res '())
             (lst lst))
    (if (length>=? lst n)
        (iter (cons (take lst n) res)
              (drop lst n))
        (append (reverse res) (if (null? lst) '() (list lst))))))

(define (void? x)
  (eq? x (void)))

(define-syntax handle-exceptions
  (syntax-rules ()
    ((handle-exceptions exn handler body ...)
     (call/cc
      (lambda (ret)
        (with-exception-handler
         (lambda (exn) (ret handler))
         (lambda () body ...)))))))

(define (pretty-print obj)
  (show #t (pretty obj))
  (void))
