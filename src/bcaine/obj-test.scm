(define-class <animal> ()
  ((name accessor: name)))
(define-class <dog> (<animal>))
(define-class <cat> (<animal>))

(define-class <lion> (<cat>))

(define interactions '())

(define-method (greet (greeter <animal>) (greetee <animal>))
  (set! interactions (cons '(interacts) interactions)))

(define-method (greet (greeter <cat>) (greetee <dog>))
  (set! interactions (cons '(hisses) interactions))
  (call-next-method))

(define-method (greet (greeter <dog>) (greetee <cat>))
  (set! interactions (cons '(sniffs) interactions))
  (call-next-method))

(define (test-high-level-tests)

  (test-group "high-level-tests"

   (test-group
    "classes"

    (test-assert
        "class-of"

      (begin
        (define-class <animal> ())
        (eq? <animal> (class-of (make <animal>)))))

    )

   (test-group
    "slots"

    (test-error
     "uninitialized slot access throws error"
     (name (make <animal>)))

    (test-assert
        "able to initialize slot from make call"
      (equal? (name (make <animal> 'name "Grouch")) "Grouch"))

    (test-assert
        "able to assign to slots"
      (let ((animal (make <animal>)))
        (set! (name animal) "Flapper")
        (equal? (name animal) "Flapper"))))

   (test-group
    "methods"

    (test-assert
        "method call order"


      (begin

        (set! interactions '())

        (greet (make <dog>) (make <cat>))

        (equal? interactions '((interacts) (sniffs)))))

    )

   )
)

(define (main args)
  (test-group
   "(bcaine obj)"
   (test-obj-methods-internal)
   (test-obj-methods-syntax)
   (test-obj-classes)
   (test-obj-default-classes)
   (test-high-level-tests)
   (newline)
   (test-exit)))
