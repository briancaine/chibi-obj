(define-library (bcaine chicken-misc)
 (import (srfi 9) (srfi 1) (srfi 6) (scheme base) (meta))
 (include "chicken-misc.scm")
 (export with-output-to-string with-output-to-port void void?
         identity symbol-append compose alist-ref add1 sub1 length>=?
         chop handle-exceptions))
