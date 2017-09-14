(define (main args)
  (test-group
   "(bcaine obj)"
   (test-obj-methods-internal)
   (test-obj-methods-syntax)
   (test-obj-classes)
   (test-obj-default-classes)
   (newline)
   (test-exit)))
