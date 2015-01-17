
(define (AlgebraicSystem:module-initialize module-name enter-module find-module eval-env )
  
  (define path-sigfun   (Pdot (Pident (ident-create "Signal")) "sigfun"))
  (define sigfun-type   (Tcon (Tpath path-sigfun) '()))
  
  (define ident-alsys   (ident-create "alsys"))
  (define path-alsys    (Pident ident-alsys))
  (define alsys-type    (Tcon (Tpath path-alsys) '()))

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
	  (list
	   (Type_sig ident-alsys (make-typedecl (make-kind 0) #f))

	   (Value_sig (ident-create "EQUATION") 
		      (make-valtype '() (arrow-type label-type (arrow-type sigfun-type alsys-type))))
	   
	   (Value_sig (ident-create "RELATION") 
		      (make-valtype '() (arrow-type label-type (arrow-type label-type (arrow-type sigfun-type (arrow-type alsys-type alsys-type))))))

	   (Value_sig (ident-create "UNION") 
		      (make-valtype '() (arrow-type alsys-type (arrow-type alsys-type alsys-type))))

	   ))


	(struct
	 (list

	  (Type_def ident-alsys (make-kind 0) 
		    (make-deftype '() (Tcon (Tpath path-alsys) '()) ))

	  (datacon 'alsys 'EQUATION 2)
	  (datacon 'alsys 'RELATION 4)
	  (datacon 'alsys 'UNION 2)

	 ))
	  
	)

    (let ((modname (ident-create module-name)))
      (enter-module modname (Signature sig))
      (eval-env (mod-eval-cbv (eval-env) (list (Module_def modname (Structure struct)))))
    )
  ))


