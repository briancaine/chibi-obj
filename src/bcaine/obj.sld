(define-library (bcaine obj)
  (import (chibi)
          (bcaine defstruct) (bcaine format) (bcaine chicken-misc)
          (srfi 1) (srfi 2) (srfi 17) (srfi 39) (srfi 69)
          (chibi ast)
          (only (scheme base) call/cc when unless let-values)
          (scheme cxr))
  (include "obj-methods-internal.scm")
  (include "obj-methods-syntax.scm")
  (include "obj-classes.scm")
  (include "obj-default-classes.scm")
  (export wipe-all-methods!
          define-generic
          define-method
          define-class
          define-primitive-class
          <number> <string> <symbol> <vector> <pair> <null>
          <class> <instance>
          <primitive-value> <primitive-class>
          make initialize allocate
          o-slot-ref o-slot-set!
))
