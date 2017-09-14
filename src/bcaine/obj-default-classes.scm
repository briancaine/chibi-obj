(define-primitive-class <number> () predicate: number?)
(define-primitive-class <string> () predicate: string?)
(define-primitive-class <vector> () predicate: vector?)
(define-primitive-class <symbol> () predicate: symbol?)
(define-primitive-class <pair> () predicate: pair?)
(define-primitive-class <null> () predicate: null?)

;; =============================================================================
;; tests
;; =============================================================================

(import (chibi test))

(define (test-obj-default-classes)
  #f)
