(define (read-all)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-all)))))

(define (define-c-struct-exports def)
  (match def
    (('define-c-struct name) '())
    (('define-c-struct name 'predicate: predicate . rest)
     (cons predicate
           (define-c-struct-exports `(define-c-struct ,name . ,rest))))
    (('define-c-struct name
       'constructor: (constructor . constructor-args) . rest)
     (cons constructor
           (define-c-struct-exports `(define-c-struct ,name . ,rest))))
    (('define-c-struct name (field-type field-name getter . possible-setter) .
       rest)
     (unless (member (length possible-setter) '(0 1))
       (error "Bad c-struct-exportsa" def))
     (append (if (pair? possible-setter)
                 (list (car possible-setter))
                 '())
             (list getter)
             (define-c-struct-exports `(define-c-struct ,name . ,rest))))
    (_
     (error "bad c-struct-exportsb" def))))

(define (define-c-union-exports un)
  (match un
    (('define-c-union name) '())
    (('define-c-union name 'predicate: predicate . rest)
     (cons predicate
           (define-c-union-exports `(define-c-union ,name . ,rest))))
    (('define-c-union name
       'constructor: (constructor . constructor-args) . rest)
     (cons constructor
           (define-c-union-exports `(define-c-union ,name . ,rest))))
    (('define-c-union name (field-type field-name getter . possible-setter) .
       rest)
     (unless (member (length possible-setter) '(0 1))
       (error "Bad c-union-exports" un))
     (append (if (pair? possible-setter)
                 (list (car possible-setter))
                 '())
             (list getter)
             (define-c-union-exports `(define-c-union ,name . ,rest))))
    (_
     (error "bad c-union-exports" un))))

(define (exports-tree tree)
  (match tree
    (('c-include foo) '())
    (('begin . items)
     (concatenate (map exports-tree items)))
    (('define-c-const type const)
     (list const))
    (('define-c return-type func-name . rest)
     (list func-name))
    (('define-c-struct . rest)
     (define-c-struct-exports `(define-c-struct . ,rest)))
    (('define-c-union . rest)
     (define-c-union-exports `(define-c-union . ,rest)))
    (('c-declare . whichever) '())
    ((a . rest)
     (append (exports-tree a) (exports-tree rest)))
    ('()
     '())))

(define (declaration-expander filename)
  (lambda (declaration)
    (match declaration
      (((or 'import 'export 'begin 'include 'include-ci 'include-shared) . rest)
       (list declaration))
      (('include+export-stub stub-basename)
       (let ((stub-filename
              (make-path (path-directory filename)
                         (string-append stub-basename ".stub"))))
         `((include-shared ,stub-basename)
           (export ,@(exports-tree (with-input-from-file stub-filename read-all))))))
      (_
       (error "Unrecognized library declaration" filename declaration)))))

(define (expand-lib filename lib)
  (match lib
    (('define-library . rest) lib)
    (('define-unprocessed-library name . declarations)
     `(define-library ,name
        ,@(concatenate (map (declaration-expander filename) declarations))))
    (_ (error "Bad library" lib))))

(define (expand-sld-file filename)
  (let ((base-dir (path-directory filename))
        (contents (with-input-from-file filename read-all)))
    (for-each
     (lambda (lib) (pretty-print (expand-lib filename lib)))
     contents)))

(define (main args)
  (match args
    ((_ input-file)
     (expand-sld-file input-file))
    (_
     (printf "Bad args: ~s\n" args)
     1)))

