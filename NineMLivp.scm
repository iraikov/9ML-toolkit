
(define (IVP:module-initialize module-name enter-module find-module eval-env)

  (define ident-ivp        (ident-create "ivp"))
  (define path-ivp         (Pident ident-ivp))
  (define ivp-type         (Tcon (Tpath path-ivp) '()))

  (define path-real        (Pident (ident-create "real")))
  (define real-type        (Tcon (Tpath path-real) '()))

  (define path-label       (Pident (ident-create "label")))
  (define label-type       (Tcon (Tpath path-label) '()))

  (define path-diagram   (Pdot (Pident (ident-create "Diagram")) "diagram"))
  (define diagram-type   (Tcon (Tpath path-diagram) '()))
  
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
	    (list

	     (Type_sig ident-ivp (make-typedecl (make-kind 0) #f))
	     
	     (Value_sig (ident-create "initial") 
			(make-valtype `() 
			      (arrow-type diagram-type
                                     (arrow-type label-type
				      (arrow-type label-type 
                                                  ivp-type)))))

	     (Value_sig (ident-create "run") 
			(make-valtype `() 
			      (arrow-type diagram-type
                                     (arrow-type label-type
				      (arrow-type label-type 
					  (arrow-type real-type
					     (arrow-type real-type
							 ivp-type))))))))
	     ))
	  
	  (struct 
	   (append
	    (list 
	     
	     (Type_def ident-ivp (make-kind 0) 
		       (make-deftype '() (Tcon (Tpath path-ivp) '()) ))
	     
	     (datacon 'ivp 'initial 3)
	     (datacon 'ivp 'run 5)
	     
	     )))
	  )
    
      (let ((modname (ident-create module-name)))
	(enter-module modname  (Signature sig))
	(eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
	)
      ))

