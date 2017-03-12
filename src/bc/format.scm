'(begin
(import (scheme base) (scheme char) (meta))
(import (chibi char-set) (chibi char-set full))
(import (srfi 9) (srfi 6))
(import (misc))
)

(define (with-output-to-string func)
  (let ((port (open-output-string)))      
    (parameterize ((current-output-port port))
      (func)
      (get-output-string port))))

(define (consume-whitespace lst)
  (if (or (null? lst) (char-set-contains? char-set:whitespace (car lst)))
      lst
      (consume-whitespace (cdr lst))))

(define (display-format str . args)
  (let iter ((chars (string->list str))
             (args args))
    (if (null? chars)
        (void)
        (let* ((next (car chars))
               (chars (cdr chars)))
          (if (null? chars)
              (display next)
              (let* ((second (car chars))
                     (chars (cdr chars))

                     (prefix (string-downcase (string next second))))
                (cond
                 ((member prefix '("~%" "~n")) (newline))
                 ((equal? prefix "~s")
                  (write (car args))
                  (iter chars (cdr args)))
                 ((equal? prefix "~a")
                  (write (car args))
                  (iter chars (cdr args)))
                 ((equal? prefix "~\n")
                  (iter (consume-whitespace chars) args))
                 (else (display next)
                       (iter (cons second chars) args)))))))))

(define (format dest str . args)
  (cond
   ((port? dest)
    (with-output-to-port dest (lambda () (apply display-format str args))))
   ((equal? dest #t)
    (apply format (current-output-port) str args))
   ((equal? dest #f)
    (with-output-to-string
      (lambda ()
        (apply format (current-output-port) str args))))
   ((string? dest)
    (apply format #f str args))
   ((number? dest)
    (apply format #t str args))
   (else
    (error "Bad dest" dest str args))))

(define (printf . args)
  (apply format #t args))

(define (sprintf . args)
  (apply format #f args))
