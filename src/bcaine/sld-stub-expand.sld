(define-library (bcaine sld-stub-expand)
  (import (chibi) (srfi 1) (only (scheme base) unless when))
  (import (bcaine format) (bcaine misc-util) (chibi match) (chibi pathname))
  (include "sld-stub-expand.scm"))
