
(define (Random:module-initialize module-name enter-module find-module eval-env)

  (define path-real   (Pident (ident-create "real")))
  (define real-type   (Tcon (Tpath path-real) '()))

  (define path-bot   (Pident (ident-create "bot")))
  (define bot-type   (Tcon (Tpath path-bot) '()))

  (define path-sigfun   (Pdot (Pident (ident-create "Signal")) "sigfun"))
  (define sigfun-type    (Tcon (Tpath path-sigfun) '()))

  (define-values (type-variables reset-type-variables
				 find-type-variable instance typerepr
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type string-type bot-type
				 )
    (core-utils))

  (let (
	(sig
	 (append

	  (map 
	   (lambda (name)
	     (Value_sig (ident-create name)
			(make-valtype '() (arrow-type sigfun-type (arrow-type sigfun-type sigfun-type)))))
	   '("int"))

	  (map 
	   (lambda (name)
	     (Value_sig (ident-create name)
			(make-valtype '() (arrow-type sigfun-type sigfun-type))))
	   '("exponential"))

	  (map 
	   (lambda (name)
	     (Value_sig (ident-create name)
			(make-valtype '() (arrow-type bot-type sigfun-type))))
	   '("uniform" "normal"))



	  ))

	(struct 
	 (append

	  (map (lambda (name) (datacon 'random name 2))
	       '(int)
	       )

	  (map (lambda (name) (datacon 'random name 1))
	       '(exponential)
	       )

	  (map (lambda (name) (datacon 'random name 1))
	       '(uniform normal)
	       )


	  ))
	)
    
    (let* ((modname (ident-create module-name))
	   (msig    (Signature sig))
	   (mdef    (Module_def modname (Structure struct))))
      (enter-module modname msig)
      (eval-env (mod-eval-cbv (eval-env) (list mdef)))
      )
    ))
  
    


