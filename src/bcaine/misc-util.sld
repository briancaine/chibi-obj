(define-library (bcaine misc-util)
 (import (srfi 9) (srfi 1) (srfi 6) (scheme base) (meta)
         (chibi show) (chibi show pretty))
 (include "misc-util.scm")
 (export with-output-to-string with-output-to-port
         with-input-from-string with-input-from-port
         void void?
         identity symbol-append compose alist-ref add1 sub1 length>=?
         chop handle-exceptions
         pretty-print))
