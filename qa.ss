(import  (nanopass) ) 
  (module ( pass xpass) ; pass-time?
      (define-syntax passes-loc (make-compile-time-value (box '())))
      
      (module (pass)

        (define values-printer
          (lambda (val*)
            (if (null? val*)
                (printf "no output\n")
                (pretty-print (car val*)))))
        (define-syntax pass
          (syntax-rules ()
            [(_ pass-name)
            (identifier? #'pass-name)
                    (lambda args (xpass pass-name values-printer args))]
            )))


            
      (module (xpass ) 
        (define $xpass
          (lambda (printer pass-name pass arg*)
            (let-values ([val* (let ([th (lambda () (apply pass arg*))])
                                (if pass-time? ($pass-time pass-name th) (th)))])
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
                  1
                  ;  (set-box! loc (cons (datum pass-name) (unbox loc)))
                  )
                #`($xpass ?printer 'pass-name pass-name ?args))]))))
      )

  

  (pass x) 
