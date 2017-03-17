(define (make-empty raw-maker slot-info)
  (apply raw-maker (make-list (length slot-info) #f)))

(define (iter-slot-info+args slot-info args handle done)
  (define slot-name car)
  (define (iter unfound-items current-args)
    (cond
     ((and (pair? current-args) (pair? (cdr current-args)))
      (let ((slot (find (lambda (slot)
                          (eq? (slot-name slot) (car current-args)))
                        unfound-items)))
        (if slot
            (begin
              (handle slot current-args)
              (iter (delete slot unfound-items)
                    (cddr current-args)))
            (warn "missing slot" (car current-args)))))
     ((null? current-args)
      (done unfound-items))
     (else (error "Bad maker arguments"))))
  (iter slot-info args))

(define (maker raw-maker slot-info)
  (lambda args
    (let ((res (make-empty raw-maker slot-info))

          (slot-name car)
          (slot-getter cadr)
          (slot-setter caddr)
          (default-thunk cadddr))

      (define (handle-unfound-items! items)
        (for-each (lambda (item)
                    (set! ((slot-getter item) res) ((default-thunk item))))
                  items))

      (iter-slot-info+args
       slot-info args
       (lambda (slot current-args)
         (set! ((slot-getter slot) res) (cadr current-args)))
       handle-unfound-items!)

      res)))

(define (updater raw-maker slot-info)
  (lambda (existing . args)
    (let ((res (make-empty raw-maker slot-info))

          (slot-name car)
          (slot-getter cadr)
          (slot-setter caddr)
          (default-thunk cadddr))

      (for-each (lambda (slot)
                  (set! ((slot-getter slot) res) ((slot-getter slot) existing)))
                slot-info)

      (iter-slot-info+args
       slot-info args
       (lambda (slot current-args)
         (set! ((slot-getter slot) res) (cadr current-args)))
       identity)

      res)))

(define (to-alister slot-info)
  (let ((slot-name car)
        (slot-getter cadr)
        (slot-setter caddr)
        (default-thunk cadddr))
    (lambda (obj)
      (map (lambda (slot) `(,(slot-name slot) . ,((slot-getter slot) obj)))
           slot-info))))

(define (from-alister raw-maker slot-info)
  (let ((slot-name car)
        (slot-getter cadr)
        (slot-setter caddr)
        (default-thunk cadddr))
    (lambda (lst)
      (apply (maker raw-maker slot-info)
             (concatenate
              (map (lambda (pair) (list (car pair) (cdr pair))) lst))))))

;(define expand-defstruct (let () ; debug
(define-syntax defstruct
  (er-macro-transformer
   (lambda (exp rename compare)

     (define (bail-with-error msg . args)
       (apply error
              (with-output-to-string
                (lambda ()
                  (printf "In expanding ~s, there was an error ~s"
                          exp msg)))
              args))

     ;; should turn
     ;; (defstruct type a (b 3) c) =>

     ;; (begin
     ;;   ;; these initial defs are to keep the bug from cropping up
     ;;   (define int-type-a-set! #f)
     ;;   (define int-type-b-set! #f)
     ;;   (define int-type-c-set! #f)
     ;;   ;; the rest is normal code
     ;;   (define-record-type :type
     ;;    (int-make-type a b c)
     ;;    type?
     ;;    (a int-type-a int-type-a-set!)
     ;;    (b int-type-b int-type-b-set!)
     ;;    (c int-type-c int-type-c-set!)
     ;;   )
     ;;   (define type-a (getter-with-setter int-type-a int-type-a-set!))
     ;;   (define type-a-set! int-type-a-set!)
     ;;   (define type-b (getter-with-setter int-type-b int-type-b-set!))
     ;;   (define type-b-set! int-type-b-set!)
     ;;   (define type-c (getter-with-setter int-type-c int-type-c-set!))
     ;;   (define type-c-set! int-type-c-set!)
     ;;   (define int-type-info
     ;;     (list (list 'a type-a-get type-a-set! (lambda () #f))
     ;;           (list 'b type-b-get type-b-set! (lambda () 3))
     ;;           (list 'c type-c-get type-c-set! (lambda () #f))))
     ;;   (define make-type (maker int-make-type int-slot-info))
     ;;   (define update-type (updater int-make-type int-slot-info))
     ;; )

     (let* ((defstruct (or (and (pair? exp) (car exp))
                           (bail-with-error "Bad defstruct macro")))

            (type-name (or (and (pair? exp) (pair? (cdr exp)) (cadr exp))
                           (bail-with-error "Bad defstruct macro" exp)))
            (slots (cddr exp))
            (_ (for-each (lambda (potential-slot)
                           (if (or (symbol? potential-slot)
                                   (and (list? potential-slot)
                                        (= (length potential-slot) 2)))
                               #t
                               (bail-with-error "Bad slot" potential-slot)))
                         slots))

            (type-tag       (symbol-append ': type-name))
            (predicate      (symbol-append type-name '?))
            (type-maker     (symbol-append 'make- type-name))
            (type-updater   (symbol-append 'update- type-name))
            (int-type-maker (symbol-append 'int-make- type-name))
            (to-alist       (symbol-append type-name '->alist))
            (from-alist     (symbol-append 'alist-> type-name))

            (int-type-info-var (symbol-append 'int- type-name '-info))

            ;; slot info accessors
            (slot-name     car)
            (slot-getter   cadr)
            (slot-setter   caddr)
            (default-value cadddr)

            (slot-info (map (lambda (slot)
                              (let ((slot-name (if (symbol? slot)
                                                   slot
                                                   (car slot)))
                                    (default (if (symbol? slot)
                                                 #f
                                                 (cadr slot))))
                                `(,slot-name
                                  ,(symbol-append type-name '- slot-name)
                                  ,(symbol-append type-name '- slot-name '-set!)
                                  ,default)))
                            slots))
            (int-slot-info (map (lambda (info)
                                  `(,(slot-name info)
                                    ,(symbol-append 'int- (slot-getter info))
                                    ,(symbol-append 'int- (slot-setter info))
                                    ,(default-value info)))
                                slot-info))

            ;; renames
            (r-begin              (rename 'begin))
            (r-define             (rename 'define))
            (r-apply              (rename 'apply))
            (r-define-record-type (rename 'define-record-type))
            (r-getter-with-setter (rename 'getter-with-setter))
            (r-quote              (rename 'quote))
            (r-list               (rename 'list))
            (r-maker              (rename 'maker))
            (r-updater            (rename 'updater))
            (r-lambda             (rename 'lambda))
            (r-eval               (rename 'eval))
            (to-alister           (rename 'to-alister))
            (from-alister         (rename 'from-alister))
            )

       (define (expand-getters-setters info int-info)
         `(,r-begin
           (,r-define ,(slot-getter info)
                      (,r-getter-with-setter
                       ,(slot-getter int-info)
                       ,(slot-setter int-info)))
           (,r-define ,(slot-setter info) ,(slot-setter int-info))))

       `(,r-begin

         ;; define-record-type
         (,r-define-record-type ,type-tag
          (,int-type-maker . ,(map slot-name int-slot-info))
          ,predicate
          . ,(map (lambda (slot)
                    `(,(slot-name slot)
                      ,(slot-getter slot) ,(slot-setter slot)))
                  int-slot-info))

         ,@(map expand-getters-setters
                slot-info
                int-slot-info)

         ;; maker and updater
         (,r-define ,int-type-info-var
           (,r-list
            ,@(map (lambda (info)
                     `(,r-list (,r-quote ,(slot-name info))
                               ,(slot-getter info)
                               ,(slot-setter info)
                               (,r-lambda () ,(default-value info))))
                   slot-info)))

         (,r-define ,type-maker (,r-maker ,int-type-maker ,int-type-info-var))
         (,r-define ,type-updater
                    (,r-updater ,int-type-maker ,int-type-info-var))

         (,r-define ,to-alist (,to-alister ,int-type-info-var))
         (,r-define ,from-alist
                    (,from-alister ,int-type-maker ,int-type-info-var)))))))
