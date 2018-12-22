(import  (nanopass) ) 

  (define (msg s)
    (newline)
    (display s)
    (newline))

    
  (define-syntax define-constant
    (lambda (x)
      (syntax-case x ()
        ((_ ctype x y)
        (and (identifier? #'ctype) (identifier? #'x))
        #'(eval-when (compile load eval)
            (putprop 'x '*constant-ctype* 'ctype)
            (putprop 'x '*constant* y)))
        ((_ x y)
        (identifier? #'x)
        #'(eval-when (compile load eval)
            (putprop 'x '*constant* y))))))


  (define lookup-constant
    (let ([flag (box #f)])
        (lambda (x)
          (unless (symbol? x)
              ($oops 'lookup-constant "~s is not a symbol" x))
          (let ([v (getprop x '*constant* flag)])
              (when (eq? v flag)
                ($oops 'lookup-constant "undefined constant ~s" x))
              v))))


(define-record-type primref
  (nongenerative #{primref a0xltlrcpeygsahopkplcn-2})
  (sealed #t)
  (fields name flags arity))


    (define sorry!
      (lambda (who str . arg*)
        ($oops 'compiler-internal "~@[~a: ~]~?" who str arg*)))

    (define maybe-source-object?
      (lambda (x)
        (or (eq? x #f) (source-object? x))))

    (define rcd?
      (lambda (x)
        (or (record-constructor-descriptor? x) #t))) ; rcd should be retricted to rcd or ctrcd

    (define exact-integer?
      (lambda (x)
        (and (integer? x) (exact? x))))

    (meta-cond
      [(= 30 (fixnum-width))
      (define target-fixnum? fixnum?)
      (define target-bignum? bignum?)]
      [(< 30 (fixnum-width))
      (define target-fixnum?
        (lambda (x)
          (and (fixnum? x)
                (fx<= (constant most-negative-fixnum) x (constant most-positive-fixnum)))))
      (define target-bignum?
        (lambda (x)
          (or (bignum? x)
              (and (fixnum? x)
                    (not (fx<= (constant most-negative-fixnum) x (constant most-positive-fixnum)))))))]
      [else
      (define target-fixnum?
        (lambda (x)
          (or (fixnum? x)
              (and (bignum? x)
                    (<= (constant most-negative-fixnum) x (constant most-positive-fixnum))))))
      (define target-bignum?
        (lambda (x)
          (and (bignum? x)
                (not (<= (constant most-negative-fixnum) x (constant most-positive-fixnum))))))])

    (define $prelex?
      (lambda (x)
        (prelex? x)))

    (define datum?
      (lambda (x)
        #t))

    (define convention?
      (lambda (x)
        (symbol? x)))


  (define-record-type var
    (fields (mutable index) (mutable spillable-conflict*) (mutable unspillable-conflict*))
    (nongenerative)
    (protocol (lambda (new) (lambda () (new #f #f #f)))))
 (define-record-type (uvar $make-uvar uvar?)
    (parent var)
    (fields 
      name
      source
      type
      conflict*
      (mutable flags)
      (mutable info-lambda)
      (mutable location)
      (mutable move*)
      (mutable degree)
      (mutable iii)           ; inspector info index
      (mutable ref-weight)    ; must be a fixnum!
      (mutable save-weight)   ; must be a fixnum!
      (mutable live-count)    ; must be a fixnum!
     )
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (pargs->new)
        (lambda (name source type conflict* flags)
          ((pargs->new) name source type conflict* flags #f #f '() #f #f 0 0 0)))))




    (define-record-type preinfo
      (nongenerative #{preinfo e23pkvo5btgapnzomqgegm-2})
      (fields src (mutable sexpr))
      (protocol
        (lambda (new)
          (case-lambda
            [() (new #f #f)]
            [(src) (new src #f)]
            [(src sexpr) (new src sexpr)]))))

    (define-record-type preinfo-lambda
      (nongenerative #{preinfo-lambda e23pkvo5btgapnzomqgegm-4})
      (parent preinfo)
      (sealed #t)
      (fields libspec (mutable name) flags)
      (protocol
        (lambda (pargs->new)
          (case-lambda
            [() ((pargs->new) #f #f 0)]
            [(src) ((pargs->new src) #f #f 0)]
            [(src sexpr) ((pargs->new src sexpr) #f #f 0)]
            [(src sexpr libspec) ((pargs->new src sexpr) libspec #f 0)]
            [(src sexpr libspec name) ((pargs->new src sexpr) libspec name 0)]
            [(src sexpr libspec name flags) ((pargs->new src sexpr) libspec name flags)]))))

    ; language of foreign types
    (define-language Ltype 
      (nongenerative-id #{Ltype czp82kxwe75y4e18-1})
      (terminals
        (exact-integer (bits))
        ($ftd (ftd))
        )
      (Type (t)
        (fp-integer bits)
        (fp-unsigned bits)
        (fp-void)
        (fp-scheme-object)
        (fp-u8*)
        (fp-u16*)
        (fp-u32*)
        (fp-fixnum)
        (fp-double-float)
        (fp-single-float)
        (fp-ftd ftd)
        (fp-ftd& ftd)
        )
        )

    (define arity?
      (lambda (x)
        (or (eq? x #f)
            (for-all fixnum? x))))

    (define maybe-string? (lambda (x) (or (eq? x #f) (string? x))))

    ;source language used by the passes leading up to the compiler or interpreter
    (define-language Lsrc
      (nongenerative-id #{Lsrc czsa1fcfzdeh493n-3})
      (terminals
        (preinfo (preinfo))
        ($prelex (x))
        (datum (d))
        (record-type-descriptor (rtd))
        (rcd (rcd))
        (source-object (src))
        (maybe-source-object (maybe-src))
        (Ltype (arg-type result-type))        => unparse-Ltype
        (fixnum (interface index flags level))
        (arity (arity))
        (box (box))
        (convention (conv))
        (maybe-string (name))
        (symbol (sym type))
        (primref (pr)))
      (Expr (e body rtd-expr)
        pr
        (moi)
        (ref maybe-src x)                                     => x
        (quote d)
        (if e0 e1 e2)
        (seq e0 e1)
        (set! maybe-src x e)                                  => (set! x e)
        (pariah)
        (case-lambda preinfo cl ...)                          => (case-lambda cl ...)
        (letrec ([x* e*] ...) body)
        (letrec* ([x* e*] ...) body)
        (call preinfo e0 e1 ...)                              => (e0 e1 ...)
        (record-type rtd e)
        (record-cd rcd rtd-expr e)
        (immutable-list (e* ...) e)
        (record rtd rtd-expr e* ...)
        (record-ref rtd type index e)
        (record-set! rtd type index e1 e2)
        (cte-optimization-loc box e)
        (foreign (conv* ...) name e (arg-type* ...) result-type)
        (fcallable (conv* ...) e (arg-type* ...) result-type)
        (profile src)                                         => (profile)
        ; used only in cpvalid
        (cpvalid-defer e))
      (CaseLambdaClause (cl)
        (clause (x* ...) interface body)                      => [(x* ...) interface body]))

    (define-language-node-counter count-Lsrc Lsrc)

    (define-language L1
      (terminals
        (uvar (x))
        (datum (d))
        (source-object (src))
        (info (info))
        (fixnum (interface))
        (primref (pr))
      )
      (entry CaseLambdaExpr)
      (Expr (e body)
        le
        x
        pr
        (quote d)
        (call info e0 e1 ...)                                 => (e0 e1 ...)
        (if e0 e1 e2)
        (seq e0 e1)
        (set! x e)
        (letrec ([x le] ...) body)
        (moi)                                                 => "moi"
        (foreign info e)
        (fcallable info e)
        (profile src)                                         => (profile)
        (pariah)
      )
      (CaseLambdaExpr (le)
        (case-lambda info cl ...)                             => (case-lambda cl ...)
      )
      (CaseLambdaClause (cl)
        (clause (x* ...) interface body)
      ))



    (define tracer              ; potentially not thread-safe, but currently unused
      (let ([ls '()])
        (case-lambda
          [() ls]
          [(x)
          (cond
            [(or (null? x) (not x)) (set! ls '())]
            [(eq? x #t) (set! ls (get-passes))]
            [(valid-pass? x) (set! ls (cons x ls))]
            [(list? x) (for-each tracer x)]
            [else (errorf 'tracer "invalid trace list or pass name: ~s" x)])])))

            


    



  (module (get-passes pass xpass) ; pass-time?
      (define-syntax passes-loc (make-compile-time-value (box '())))
      (define-syntax get-passes
        (lambda (x)
          (lambda (r)
            (syntax-case x ()
              [(_) #`(unbox (quote #,(datum->syntax #'* (r #'passes-loc))))]))))
      (module (pass)
        (define ir-printer
          (lambda (unparser)
            (lambda (val*)
              (safe-assert (not (null? val*)))
              (pretty-print (flatten-seq (unparser (car val*)))))))
        (define values-printer
          (lambda (val*)
            (if (null? val*)
                (printf "no output\n")
                (pretty-print (car val*)))))
        (define-syntax pass
          (syntax-rules ()

            [(_ (pass-name ?arg ...) ?unparser)
            (identifier? #'pass-name)
            (let ([pass-name (pass-name ?arg ...)])
              (msg "case 1")
              (lambda args (xpass pass-name (ir-printer ?unparser) args)))]

            [(_ pass-name ?unparser)
            (identifier? #'pass-name)
            (begin 
            ; (msg "---")
            ;  (msg #`args)
              (lambda args (xpass pass-name (ir-printer ?unparser) args)))
            ]

            [(_ (pass-name ?arg ...))
            (identifier? #'pass-name)
            (let ([pass-name (pass-name ?arg ...)])
              (msg "case 3")
              (lambda args (xpass pass-name values-printer args)))]

            [(_ pass-name)
            (identifier? #'pass-name)
            (begin (msg "case 3")
                    (lambda args (xpass pass-name values-printer args))
            )
            ]
            
            
            )))


            
      (module (xpass ) ;pass-time?
        ; (define-threaded pass-time?)
        (define $xpass
          (lambda (printer pass-name pass arg*)
            (let-values ([val* (let ([th (lambda () (apply pass arg*))])
                                (if #f #f (th)))])
              (when (memq pass-name (tracer))
                (printf "output of ~s:\n" pass-name)
                (printer val*))
              (apply values val*)))
              )
        (define-syntax xpass
          (lambda (x)
            (syntax-case x ()
              [(_ pass-name ?printer ?args)
              
              (lambda (r)
                (let ([loc (r #'passes-loc)])
                  ;  (msg  r )
                    (set-box! loc (cons (datum pass-name) (unbox loc)))
                  )
                  ; 1 )]))))
                 #`($xpass ?printer 'pass-name pass-name ?args))]))))
      (define flatten-seq
        (lambda (x)
          (define helper
            (lambda (x*)
              (if (null? x*)
                  '()
                  (let ([x (car x*)])
                    (if (and (pair? x) (eq? (car x) 'seq))
                        (append (helper (cdr x)) (helper (cdr x*)))
                        (cons (flatten-seq x) (helper (cdr x*))))))))
          (cond
            [(null? x) '()]
            [(and (pair? x) (eq? (car x) 'seq))
            (let ([x* (helper (cdr x))])
              (if (fx= (length x*) 1)
                  (car x*)
                  (cons 'begin x*)))]
            [(and (pair? x) (eq? (car x) 'quote)) x]
            [(list? x) (map flatten-seq x)]
            [else x])))
  



  (define-pass cpnanopass : Lsrc (ir) -> L1 ()
        (definitions
          (define-syntax with-uvars
            (syntax-rules ()
              [(_ (x* id*) b1 b2 ...)
               (and (identifier? #'x*) (identifier? #'id*))
               (let ([uvar* (map prelex->uvar id*)] [name* (map prelex-name id*)])
                 (dynamic-wind
                   (lambda () (for-each prelex-name-set! id* uvar*))
                   (lambda () (let ([x* uvar*]) b1 b2 ...))
                   (lambda () (for-each prelex-name-set! id* name*))))]))
          (define extract-uvar
            (lambda (id)
              (let ([x (prelex-name id)])
                (unless (uvar? x)
                ; (unless #f
                  (sorry! 'extract-uvar "~s is not a uvar" x))
                x))))
        (CaseLambdaExpr : Expr (ir x) -> CaseLambdaExpr ()
          [(case-lambda ,preinfo (clause (,x** ...) ,interface* ,body*) ...)
           (let ([info (make-info-lambda (preinfo-src preinfo) (preinfo-sexpr preinfo) (preinfo-lambda-libspec preinfo) interface*
                         (preinfo-lambda-name preinfo) (preinfo-lambda-flags preinfo))])
             (when x (uvar-info-lambda-set! x info))
             `(case-lambda ,info
                ,(map (lambda (x* interface body)
                        (with-uvars (uvar* x*)
                          (in-context CaseLambdaClause
                            `(clause (,uvar* ...) ,interface ,(Expr body)))))
                   x** interface* body*) ...))]
          [(case-lambda ,preinfo ,cl* ...)
           (sorry! who "found unreachable clause" ir)])
        (Expr : Expr (ir) -> Expr ()
          [(ref ,maybe-src ,x) (extract-uvar x)]
          [(set! ,maybe-src ,x ,[e]) `(set! ,(extract-uvar x) ,e)]
          [(case-lambda ,preinfo ,cl* ...) (CaseLambdaExpr ir #f)]
          [(letrec ([,x* ,e*] ...) ,body)
           (with-uvars (uvar* x*)
             (let ([e* (map CaseLambdaExpr e* uvar*)])
               `(letrec ([,uvar* ,e*] ...) ,(Expr body))))]
          [(call ,preinfo ,e ,[e*] ...)
           `(call ,(make-info-call (preinfo-src preinfo) (preinfo-sexpr preinfo) (fx< (optimize-level) 3) #f #f)
              ,(Expr e) ,e* ...)]
          [(foreign (,conv* ...) ,name ,[e] (,arg-type* ...) ,result-type)
           (let ([info (make-info-foreign conv* arg-type* result-type)])
             (info-foreign-name-set! info name)
             `(foreign ,info ,e))]
          [(fcallable (,conv* ...) ,[e] (,arg-type* ...) ,result-type)
           `(fcallable ,(make-info-foreign conv* arg-type* result-type) ,e)])
        (CaseLambdaExpr ir #f))



(define (parse s)
  (with-output-language (Lsrc Expr)
    (cond
      [(or (fixnum? s)  (symbol? s)) `,s]
      )))




 ( (pass cpnanopass unparse-L1)  1  ))
