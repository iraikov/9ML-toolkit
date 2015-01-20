;;
;;  A parser for NineML + syntactic sugar.
;;
;;  Based on the code and paper by Xavier Leroy (2000): A modular
;;  module system. Journal of Functional Programming, 10, pp 269-303
;;  doi:10.1017/S0956796800003683
;;
;;
;; Copyright 2010-2014 Ivan Raikov
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module 9ML-parse

	(parse parse-sexpr-macro parse-string-expr parse-sym-expr make-signal-expr
	 nineml-xmlns-base parse-al-sxml-component parse-al-sxml)

	(import scheme chicken)
	(require-library srfi-1 srfi-13 data-structures extras)
	(import
	 (only srfi-1 concatenate fold combine any every unzip2 filter-map partition delete-duplicates cons* lset-difference)
	 (only srfi-13 string-null?)
	 (only data-structures conc ->string alist-ref)
	 (only extras fprintf pp))

	(require-extension matchable)
	(require-extension sxpath sxpath-lolevel)
	(require-extension static-modules miniML miniMLsyntax)

	
	(require-library sxml-transforms)
	(import (prefix sxml-transforms sxml:))

	(include "SXML.scm")


  (define-values (type-variables reset-type-variables
				 find-type-variable instance typerepr
				 begin-def end-def newvar generalize
				 make-deftype make-valtype make-kind
				 binop ternop path-star path-list path-arrow
				 star-type list-type arrow-type label-type string-type bot-type
				 )
    (core-utils))


(define (safe-car x) (and (pair? x) (car x)))


(define-syntax tok
  (syntax-rules ()
    ((tok loc t) (make-lexical-token (quasiquote t) loc #f))
    ((tok loc t l) (make-lexical-token (quasiquote t) loc l))))



(define-record-type sexpr-macro
  (make-sexpr-macro label text)
  sexpr-macro? 
  (label sexpr-macro-label)
  (text sexpr-macro-text))



(define-record-type algebraic-eqn
  (make-algebraic-eqn quantity rhs)
  algebraic-eqn? 
  (quantity algebraic-eqn-quantity)
  (rhs algebraic-eqn-rhs))


(define-record-type ode-eqn
  (make-ode-eqn indep dep tstep rhs)
  ode-eqn? 
  (indep ode-eqn-indep)
  (dep   ode-eqn-dep)
  (tstep ode-eqn-tstep)
  (rhs   ode-eqn-rhs))


(define-record-type relation
  (make-relation quantity var rhs)
  relation? 
  (quantity relation-quantity)
  (var      relation-var)
  (rhs      relation-rhs))


(define (ode-eqn-or-relation? x)
  (or (ode-eqn? x) (relation? x)))

(define (algebraic-eqn-or-relation? x)
  (or (algebraic-eqn? x) (relation? x)))



(define sexpr-macro-hooks (make-parameter '()))


(define (register-macro-hook label hook)
  (assert (procedure? hook))
  (if (not (symbol? label))
      (error 'register-macro-hook "hook label must be a symbol" label))
  (if (assoc label (sexpr-macro-hooks))
      (error 'register-macro-hook "hook already exists" label))
  (sexpr-macro-hooks (cons (cons label hook) (sexpr-macro-hooks)))
  )


(define (parse-sexpr-macro x)
  (if (sexpr-macro? x)
      (let ((label (sexpr-macro-label x)))
	(if (not label)
	    (let ((default-handler (cdr (assoc 'default (sexpr-macro-hooks)))))
	      (default-handler x))
	    (cond ((assoc label (sexpr-macro-hooks)) =>
		   (lambda (v) ((cdr v) (sexpr-macro-text x))))
		  (else
		   (error 'parse-sexpr-macro "cannot find handler for macro" label))
		  )))
	))



(include "NineML.grm.scm")
(include "NineML.l.scm")
(include "expr-parser.scm")


(define (make-parse-error loc)
  (lambda (msg #!optional arg)
    (let ((loc-str (or (and loc (if (list? loc) (conc " " loc " ") (conc " (" loc ") "))) "")))
      (cond  [(not arg) (error loc-str msg)]
	     [(lexical-token? arg)
	      (error (conc "line " (let ((src (lexical-token-source arg)))
				     (if (source-location? src) 
					 (source-location-line (lexical-token-source arg))
					 src)) ": " msg)
		     loc-str
		     (conc (lexical-token-category arg) 
			   (if (lexical-token-value arg) (conc " " (lexical-token-value arg)) "")))]
	     [else (error loc-str (conc msg arg))]
	     ))))

(define lexer-error error)


(define (parse loc s)
  (cond ((port? s)   (lexer-init 'port s))
	((string? s) (lexer-init 'string s))
	(else (error 'parse "bad argument type; not a string or port" s)) )
   (parser lexer (make-parse-error loc)))

(define empty             (Pident (ident-create "empty")))

(define list-cons         (Pident (ident-create "cons")))
(define list-null         (Pident (ident-create "null")))

(define diagram-pure         (Longid (Pdot (Pident (ident-create "Diagram")) "PURE")))
(define diagram-group        (Longid (Pdot (Pident (ident-create "Diagram")) "GROUP")))
(define diagram-assign       (Longid (Pdot (Pident (ident-create "Diagram")) "ASSIGN")))
(define diagram-ode          (Longid (Pdot (Pident (ident-create "Diagram")) "ODE")))
(define diagram-sequence     (Longid (Pdot (Pident (ident-create "Diagram")) "SEQUENCE")))
(define diagram-union         (Longid (Pdot (Pident (ident-create "Diagram")) "UNION")))
(define diagram-on            (Longid (Pdot (Pident (ident-create "Diagram")) "ON")))
(define diagram-transient     (Longid (Pdot (Pident (ident-create "Diagram")) "TRANSIENT")))
(define diagram-rtransition   (Longid (Pdot (Pident (ident-create "Diagram")) "RTRANSITION")))
(define diagram-relation      (Longid (Pdot (Pident (ident-create "Diagram")) "RELATION")))
(define diagram-identity      (Longid (Pdot (Pident (ident-create "Diagram")) "IDENTITY")))

(define alsys-relation      (Longid (Pdot (Pident (ident-create "AlgebraicSystem")) "RELATION")))
(define alsys-equation      (Longid (Pdot (Pident (ident-create "AlgebraicSystem")) "EQUATION")))
(define alsys-union         (Longid (Pdot (Pident (ident-create "AlgebraicSystem")) "UNION")))

(define signal-realconst     (Longid (Pdot (Pident (ident-create "Signal")) "realconst")))
(define signal-boolconst     (Longid (Pdot (Pident (ident-create "Signal")) "boolconst")))
(define signal-boolsig       (Longid (Pdot (Pident (ident-create "Signal")) "boolsig")))
(define signal-realsig       (Longid (Pdot (Pident (ident-create "Signal")) "realsig")))
(define signal-realparam     (Longid (Pdot (Pident (ident-create "Signal")) "realparam")))
(define signal-signal        (Longid (Pdot (Pident (ident-create "Signal")) "signal")))

(define (make-group rhs-list)
  (let ((n (length rhs-list)))
    (cond ((= n 1)  (car rhs-list))
	  ((= n 2)  (Apply (Apply diagram-group (car rhs-list)) (cadr rhs-list)))
	  (else     (make-group
		     (list (make-group (list (car rhs-list) (cadr rhs-list)) )
			   (make-group (cddr rhs-list))))))))

(define (make-list value-list)
  (let recur ((value-list (reverse value-list)) 
	      (value (Longid (Pident (ident-create "null")))))
    (if (null? value-list) value
	(recur (cdr value-list) 
	       (Apply (Apply (Longid (Pident (ident-create "cons"))) (car value-list)) 
		      value)))
    ))


(define (make-relations relation-list value)
  (if (null? relation-list) value
      (let ((relation (car relation-list)))
	(Apply
	 (Apply
	  (Apply
	   (Apply diagram-relation (Const `(label ,(relation-quantity relation))))
	   (Const `(label ,(relation-var relation))))
	  (relation-rhs relation))
	 (make-relations (cdr relation-list) value)))
      ))

(define (make-alsys-relations relation-list value)
  (if (null? relation-list) value
      (let ((relation (car relation-list)))
	(Apply
	 (Apply
	  (Apply
	   (Apply alsys-relation (Const `(label ,(relation-quantity relation))))
	   (Const `(label ,(relation-var relation))))
	  (relation-rhs relation))
	 (make-relations (cdr relation-list) value)))
      ))


(define (op->signal-function op)
  (let ((name (case op
		((+)   "add")
		((*)   "mul")
		((/)   "div")
		((>)   "gt")
		((<)   "lt")
		((>=)  "gte")
		((<=)  "lte")
		(else (->string op)))))
    (Longid (Pdot (Pident (ident-create "Signal")) name))))
    

(define (op->random-function op)
  (define opmap '((random.int     . "int")
                  (random.uniform . "uniform")
                  (random.normal  . "normal")
                  (random.poisson . "poisson")
                  (random.exponential . "exponential")))
  (let ((name op))
    (Longid (Pdot (Pident (ident-create "Random")) 
                  (alist-ref name opmap)))))
    


(define (op->relation op)
  (Apply
   (Longid (Pdot (Pident (ident-create "Signal")) "relation"))
   (Const `(label ,op))))

(define (signal-operation? op)
  (case op
    ((add mul div gt gte lte neg cosh tanh log ln exp) #t)
    (else #f)))

(define (random-operation? op)
  (case op
    ((random.int random.normal random.uniform random.poisson random.exponential) #t)
    (else #f)))

    
(define (make-pure sf) (Apply diagram-pure sf))


(define (make-signal-expr expr #!key (subst '()) (argument #f))

  (let recur ((expr expr))

    (cond ((number? expr) 
           (Apply signal-realconst (Const `(real ,expr))))

          ((symbol? expr) 
           (case expr 
             ((false) (Apply signal-boolconst (Const `(bool #f))))
             ((true)  (Apply signal-boolconst (Const `(bool #t))))
             (else 
              (let ((v (alist-ref expr subst)))
                (cond (v 
                       (make-signal-expr v subst: '() argument: argument))
                      
                      ((equal? argument expr)
                       (Apply signal-signal (Const `(label ,expr))))
                      
                      (else (Longid (Pident (ident-create (->string expr))))))))
             ))

	(else
	 (match expr

		(('- a)  
		 (Apply (op->signal-function "neg") (recur a)))

		(('- a b)  
		 (Apply (Apply (op->signal-function "sub") 
                               (recur a))
			(recur b)))
		
		(('if a b c)  
		 (Apply
		  (Apply (Apply (op->signal-function "if")
				(recur a))
			 (recur b))
		  (recur c)))
		
		(((and op (? symbol?)) a b)
		 (Apply
		  (Apply (op->signal-function op) 
			 (recur a))
		  (recur b)))
		
		(((and op (? symbol?)) a)
		 (cond ((signal-operation? op)
                        (Apply (op->signal-function op) 
                               (recur a)))
                       ((random-operation? op)
                        (Apply (op->random-function op) 
                               (recur a)))
                       (else
                        (Apply (op->relation op) 
                               (recur a)))))

		(((and op (? symbol?)))
		 (cond
                  ((random-operation? op)
                   (Apply (op->random-function op) (Longid empty)))
                  (else
                   (error 'make-signal-expr "invalid signal expression" expr))))
		
		(else (error 'make-signal-expr "invalid signal expression" expr))))
	))
)

(define (parse-sexpr-eqn x)
  (match x
	 (((or 'D 'd) (dep indep tstep) '= . rhs)
	  (let ((rhs   (parse-string-expr (->string rhs))))
	    (make-ode-eqn indep dep tstep (make-signal-expr rhs))))

	 (((and quantity (? symbol?)) (var) '= . rhs)
	  (let ((rhs (parse-string-expr (->string rhs))))
	    (make-relation quantity var (make-signal-expr rhs))))

	(((and quantity (? symbol?))  '= . rhs)
	 (let ((rhs  (parse-string-expr (->string rhs))))
	   (make-algebraic-eqn quantity (make-signal-expr rhs))))

	(else
	 (error 'parse-sexpr-eqn "invalid equation" x))
	))
		    

(define (make-ode-eqn-expr eqn)
  (and (ode-eqn? eqn) 
       (let ((rhs (ode-eqn-rhs eqn))
	     (dep (ode-eqn-dep eqn))
	     (indep (ode-eqn-indep eqn))
	     (tstep (ode-eqn-tstep eqn)))
	 (Apply
	  (Apply
	   (Apply
	    (Apply diagram-ode (make-list (list (Longid (Pident (ident-create (->string dep)))))))
	    (Longid (Pident (ident-create (->string indep)))))
	   (Longid (Pident (ident-create (->string tstep)))))
	  (make-pure rhs))
	 )))


(define (make-relation-expr eqn)
  (let ((rhs (relation-rhs eqn))
	(var (relation-var eqn))
	(quantity (relation-quantity eqn)))
    (Apply
     (Apply
      (Apply diagram-relation (Const `(label ,quantity)))
      (Const `(label ,var)))
     (make-pure rhs))
    ))


(define (make-algebraic-eqn-expr eqn)
  (let ((rhs (algebraic-eqn-rhs eqn))
	(quantity (algebraic-eqn-quantity eqn)))
    (Apply
     (Apply diagram-assign (make-list (list (Const `(label ,quantity)))))
     (make-pure rhs))
    ))


(define (make-algebraic-eqn-lst-expr eqlst)
  (and (not (null? eqlst))
      (let ((qs (map (lambda (x) (Const `(label ,(algebraic-eqn-quantity x)))) eqlst)))
        (Apply (Apply diagram-assign (make-list qs))
               (make-group (map make-pure (map algebraic-eqn-rhs eqlst)))))))


(define (make-ode-eqn-lst-expr eqlst)
  (let ((tsteps (delete-duplicates (map ode-eqn-tstep eqlst)))
	(indeps (delete-duplicates (map ode-eqn-indep eqlst)))
	(deps   (map  ode-eqn-dep eqlst)))
    (match (list deps indeps tsteps)
	   (((dep . _) (indep) (tstep))
	    (Apply
	     (Apply
	      (Apply 
	       (Apply diagram-ode (make-list (map (lambda (x) (Longid (Pident (ident-create (->string x))))) deps)))
	       (Longid (Pident (ident-create (->string indep)))))
	      (Longid (Pident (ident-create (->string tstep)))))
	     (make-group (map make-pure (map ode-eqn-rhs eqlst)))))
	   (else (error 'make-ode-eqn-lst-expr "invalid system of ODE equations" eqlst)))))


(define (make-dae-eqn-lst-expr eqlst)
  (let-values (((relations ode-eqs) (partition relation? eqlst)))
    (let ((tsteps (delete-duplicates (map ode-eqn-tstep ode-eqs)))
	  (indeps (delete-duplicates (map ode-eqn-indep ode-eqs)))
	  (deps   (map ode-eqn-dep ode-eqs)))
      (match (list deps indeps tsteps)
	     (((dep . _) (indep) (tstep))
	      (Apply
	       (Apply
		(Apply
		 (Apply diagram-ode (make-list (map (lambda (x) (Longid (Pident (ident-create (->string x))))) deps)))
		 (Longid (Pident (ident-create (->string indep)))) )
		(Longid (Pident (ident-create (->string tstep)))))
	       (make-relations relations (make-group (map make-pure (map ode-eqn-rhs ode-eqs))))))
	     
	     (else (error 'parse-NineML-equation-sexpr-macro "invalid system of DAE equations" eqlst))
	     ))))


(define (make-on-assignments assignments trigger-name)
  (Apply
   (Apply diagram-on assignments) 
   (Apply
    (Apply signal-realsig (Const `(label ,trigger-name)))
    (Apply signal-realconst (Const `(real -1.0))))))


(define (make-rtransition state-name relations aliases
                          ode-variables1 ode-rhss1 trigger-name1 trigger-rhs1 assign-variables1 assign-rhss1 
                          c-assign-variables1 c-assign-rhss1 e1
                          ode-variables2 ode-rhss2 trigger-name2 trigger-rhs2 assign-variables2 assign-rhss2 
                          c-assign-variables2 c-assign-rhss2 e2)
  
  (define (rewrite-trigger rhs)
    (match rhs
           (('>= x y) `(- ,x ,y))
           (('> x y)  `(- ,x ,y))
           (('<= x y) `(- ,y ,x))
           (('< x y)  `(- ,y ,x))
           (else rhs)))


  (let (
        (assignments1
         (make-algebraic-eqn-lst-expr
          (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
               assign-variables1 assign-rhss1)
          ))
        
        (assignments2
         (make-algebraic-eqn-lst-expr
          (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
               assign-variables2 assign-rhss2)))

        (c-assignments1
         (make-algebraic-eqn-lst-expr
          (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
               c-assign-variables1 c-assign-rhss1)
           ))
        

        (c-assignments2
         (make-algebraic-eqn-lst-expr
          (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
               c-assign-variables2 c-assign-rhss2)
           ))

        (odes1 
         (if (null? relations)
                         
             (make-ode-eqn-lst-expr
              (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))
                   ode-variables1 ode-rhss1))
             
             (make-dae-eqn-lst-expr
              (append relations
                      (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))
                           ode-variables1 ode-rhss1)))
             ))

        (odes2
         (and (not (null? ode-variables2))
              (if (null? relations)
                  
                  (make-ode-eqn-lst-expr
                   (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))
                        ode-variables2 ode-rhss2))
                  
                  (make-dae-eqn-lst-expr
                   (append relations
                           (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))
                                ode-variables2 ode-rhss2)))
                  )))
         

        )


    (let* (
           (trsys1 
            (Apply
             
             (Apply
              
              (Apply
               
               (Apply
                
                (Apply
                 
                 (Apply
                  
                  (Apply diagram-rtransition
                         
                         (let* ((sys1 (if assignments1
                                          (Apply
                                           (Apply diagram-sequence odes1)
                                           assignments1)
                                          odes1))
                                (sys2 (if e1 (make-event e1 sys1 aliases) sys1)))


                  sys2))
      
                  (let* ((sys1 (if (null? ode-variables2)
                                   
                                   (if (null? relations) 
                                       assignments2
                                       (make-relations relations assignments2))
                                   
                                   (if assignments2
                                       (Apply 
                                        (Apply diagram-sequence odes2)
                                        assignments2)
                                       odes2)))
                         
                         (sys2 (if e2 (make-event e2 sys1 aliases) sys1)))

                    sys2))
                 
                 (Apply
                  (Apply signal-realsig (Const `(label ,trigger-name1)))
                  (Apply signal-realconst (Const `(real -1.0)))))
                
                (Apply
                 (Apply diagram-assign (make-list (list (Const `(label ,trigger-name1)))))
                 (make-pure (make-signal-expr (rewrite-trigger trigger-rhs1) subst: aliases))) )
               
               (Apply
                (Apply signal-realsig (Const `(label ,trigger-name2)))
                (Apply signal-realconst (Const `(real -1.0)))) )

              (Apply
               (Apply diagram-assign (make-list (list (Const `(label ,trigger-name2)))))
               (make-pure (make-signal-expr (rewrite-trigger trigger-rhs2) subst: aliases))) )
             
             (Apply
              (Apply signal-boolsig (Const `(label ,state-name)))
              (Apply signal-boolconst (Const `(bool #f)))))
            
            )

           (trsys2
             (if c-assignments1
                 (Apply (Apply diagram-sequence trsys1)
                        (make-on-assignments c-assignments1 trigger-name1))
                 trsys1))

           (trsys3 (if c-assignments2
                       (Apply (Apply diagram-sequence trsys2) 
                              (make-on-assignments c-assignments2 trigger-name2))
                       trsys2))
           )
      trsys3))
  )


(define (make-transient 
         relations aliases
         ode-variables ode-rhss assign-variables assign-rhss
         trigger-name trigger-rhs 
         ode-variables1 ode-rhss1 assign-variables1 assign-rhss1)

  (define (rewrite-trigger rhs)
    (match rhs
           (('>= x y) `(- ,x ,y))
           (('> x y)  `(- ,x ,y))
           (('<= x y) `(- ,y ,x))
           (('< x y)  `(- ,y ,x))
           (else rhs)))

    (let ((assignments
           (make-algebraic-eqn-lst-expr
            ((lambda (x) (if (null? ode-variables)
                             (cons (make-algebraic-eqn 't (make-signal-expr '(+ t h))) x) x))
             (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
                  assign-variables assign-rhss))))

          (assignments1
           (make-algebraic-eqn-lst-expr
            ((lambda (x) (if (null? ode-variables1)
                             (cons (make-algebraic-eqn 't (make-signal-expr 't)) x) x))
             (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
                  assign-variables1 assign-rhss1))))
      


          (odes 
           (and (not (null? ode-variables))
                (if (null? relations)
                    
                    (make-ode-eqn-lst-expr
                     (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))  
                          ode-variables ode-rhss))
                    
                    (make-dae-eqn-lst-expr
                     (append relations
                             (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))  
                                  ode-variables ode-rhss)))
                    )))

          (odes1 
           (and (not (null? ode-variables1))
                (if (null? relations)
                    
                    (make-ode-eqn-lst-expr
                     (map (lambda (var rhs) 
                            (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))  
                          ode-variables1 ode-rhss1))
                    
                    (make-dae-eqn-lst-expr
                     (append relations
                             (map (lambda (var rhs) 
                                    (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))  
                                  ode-variables1 ode-rhss1)))
                    )))
          )

      (Apply
       
       (Apply
        
        (Apply
         
         (Apply diagram-transient
                
                (if (null? ode-variables)
                    
                    assignments

                    (if assignments
                        (Apply
                         (Apply diagram-sequence odes)
                         assignments)
                        odes)))
         
         (if (null? ode-variables1)
             
             assignments1
             
             (Apply
              (Apply
               diagram-sequence odes1)
              assignments1)
             
             ))
        
       (Apply
        (Apply signal-realsig (Const `(label ,trigger-name)))
        (Apply signal-realconst (Const `(real -1.0)))) )

       
       (Apply
        (Apply diagram-assign (make-list (list (Const `(label ,trigger-name)))))
        (make-pure (make-signal-expr (rewrite-trigger trigger-rhs) subst: aliases))) )
      
      ))


(define (make-event e r aliases)
  (let* (
         (e-state-assignments ((sxpath `(nml:StateAssignment)) e))
         (e-assign-variables (map (lambda (x) 
                                    (string->symbol (sxml:attr  x 'variable))) 
                                  e-state-assignments))
         (e-assign-rhss      (map (lambda (x)
                                    (parse-string-expr 
                                     (sxml:kidn-cadr 'nml:MathInline x) 
                                     'parse-al-sxml-dynamics))
                                  e-state-assignments))
         (e-port             (string->symbol (or (sxml:attr e 'src_port)
                                                 (sxml:attr e 'port))))
         )

    (Apply
     (Apply
      (Apply
       (Apply diagram-transient r)
       (make-algebraic-eqn-lst-expr
        (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
             e-assign-variables e-assign-rhss)))
      (Apply
       (Apply signal-realsig (Const `(label ,e-port)))
       (Apply signal-realconst (Const `(real 0.0)))))
            
     (Apply
      (Apply diagram-assign (make-list (list (Const `(label ,e-port)))))
      (make-pure (make-signal-expr e-port subst: aliases))))
    ))


(define (make-alsys-union eq-list)
  (let ((n (length eq-list)))
    (cond ((= n 1)  (car eq-list))
	  ((= n 2)  (Apply (Apply alsys-union (car eq-list)) (cadr eq-list)))
	  (else     (make-alsys-union
		     (list (make-alsys-union (list (car eq-list) (cadr eq-list)) )
			   (make-alsys-union (cddr eq-list))
                           ))
                    )
          ))
    )


(define (make-alsys-eqn-lst-expr eqlst)
  (and (not (null? eqlst))
       (let ((qs (map (lambda (x) (Const `(label ,(algebraic-eqn-quantity x)))) eqlst))
             (rhss (map algebraic-eqn-rhs eqlst)))
         (make-alsys-union
          (map (lambda (q rhs) (Apply (Apply alsys-equation q) rhs)) qs rhss)
          ))
       ))


(define (parse-NineML-equation-sexpr-macro mac)
  (if (not (sexpr-macro? mac))
      (error 'parse-NineML-equation-sexpr-macro "invalid macro expression" mac))
  
  (let ((lst (sexpr-macro-text mac)))


    (match lst

	 (((? symbol?) . rest)

	  (let ((eqn (parse-sexpr-eqn lst)))

	    (cond ((ode-eqn? eqn)   (make-ode-eqn-expr eqn))

		  ((relation? eqn) (make-relation-expr eqn))

		  ((algebraic-eqn? eqn) (make-algebraic-eqn-expr eqn))
		  
		  )))


	 (((? pair?) . rest)

	  (let ((eqlst (map parse-sexpr-eqn lst)))

	    (cond ((every algebraic-eqn-or-relation? eqlst) 
		   (make-algebraic-eqn-lst-expr eqlst))

		 ((every ode-eqn? eqlst)
		  (make-ode-eqn-lst-expr eqlst))

		 ((every ode-eqn-or-relation? eqlst)
		  (make-dae-eqn-lst-expr eqlst))
			  
		 (else
		  (error 'parse-NineML-equation-sexpr-macro "invalid system of equations" eqlst)))))
		
	(else (error 'parse-NineML-equation-sexpr-macro "invalid equational expression" lst))
	))
  )


(define (parse-list-sexpr-macro text)
  (let recur ((text (reverse text)) 
	      (lst list-null))
    (if (null? text) lst
	(recur (cdr lst) (Apply list-cons (parse (->string (car text))) lst)))
    ))




(define nineml-xmlns-base "http://nineml.net/9ML/")

(define (parse-al-sxml-dynamics sxml)
  (let (
        (state-variables  ((sxpath `(// nml:StateVariable)) sxml))
	(regimes          ((sxpath `(// nml:Regime)) sxml))
        (relations        ((sxpath `(// nml:Relation)) sxml))
        (aliases          ((sxpath `(// nml:Alias)) sxml))
        )


;; TODO: ensure that parameters and state variables are consistent in the equations

    (if (pair? regimes)
	(cond

	 ((= (length regimes) 1)
          (let ((r (car regimes)))
            (let (
                  (time-derivatives   ((sxpath `(nml:TimeDerivative)) r))
                  (on-conditions      ((sxpath `(nml:OnCondition)) r))
                  (on-events          ((sxpath `(nml:OnEvent)) r))
                  (state-assignments  ((sxpath `(nml:StateAssignment)) r))
                  )

              (if (> (length on-conditions) 1)
                  (error 'parse-al-sxml-dynamics "multiple on-conditions blocks in regime are not supported" r))

              (if (> (length on-events) 1)
                  (error 'parse-al-sxml-dynamics "multiple on-events in regime are not supported" r))
              
              (if (and (null? time-derivatives) (null? state-assignments))
                  (error 'parse-al-sxml-dynamics "regime does not contain time derivative blocks or assignments" r))
              
              (let*
                  (
                    (ode-variables    (map (lambda (x) 
                                             (string->symbol (sxml:attr x 'variable )))
                                           time-derivatives))
                    
                    (ode-rhss         (map (lambda (x)
                                             (parse-string-expr 
                                              (sxml:kidn-cadr 'nml:MathInline x )
                                              'parse-al-sxml-dynamics))
                                           time-derivatives))

                    (assign-variables (map (lambda (x) 
                                             (string->symbol (sxml:attr  x 'variable))) 
                                           state-assignments))

                    (assign-rhss      (map (lambda (x)
                                             (parse-string-expr 
                                              (sxml:kidn-cadr 'nml:MathInline x) 
                                              'parse-al-sxml-dynamics))
                                           state-assignments))
                    

                    (relations        (map (lambda (x)
                                             (let ((quantity (sxml:attr x 'name))
                                                   (var      (sxml:attr x 'argument))
                                                   (rhs      (parse-string-expr 
                                                              (sxml:kidn-cadr 'nml:MathInline x )
                                                              'parse-al-sxml-dynamics)))
                                               (make-relation (string->symbol quantity)
                                                              (string->symbol var)
                                                              (make-signal-expr  rhs argument: (string->symbol var)))
                                            ))
                                           relations))

                    (aliases        (map (lambda (x)
                                             (let ((quantity (sxml:attr x 'name))
                                                   (rhs      (parse-string-expr 
                                                              (sxml:kidn-cadr 'nml:MathInline x )
                                                              'parse-al-sxml-dynamics)))
                                               `(,(string->symbol quantity) .
                                                 ,rhs)
                                               ))
                                         aliases))

                    (on-event (and (not (null? on-events))
                                   (car on-events)))
                    )
                
                

                (if (null? on-conditions)

                    (let ((odes 
                           (map (lambda (var rhs) (make-ode-eqn 't var 'h (make-signal-expr rhs subst: aliases)))  
                                ode-variables ode-rhss))
                          
                          (assignments
                           (and (not (null? assign-variables))
                                (make-algebraic-eqn-lst-expr
                                 (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs subst: aliases))  )
                                      assign-variables assign-rhss))))

                          )

                      ((lambda (sys)
                         (if on-event
                             (make-event on-event sys aliases)
                             sys))

                       ((lambda (rels+odes)
                          (if assignments
                              
                              (Apply
                               (Apply diagram-sequence rels+odes)
                               assignments)
                              
                              rels+odes))
                        
                        (if (null? relations)
                            (make-ode-eqn-lst-expr odes)
                            (make-dae-eqn-lst-expr (append relations odes))
                            ))
                       
                       ))
                    
                    (let ((c (car on-conditions)))

                      (let (
                            ( trigger (sxml:kidn-cadr 'nml:Trigger c))
                            ( event-out (sxml:kidn 'nml:EventOut c))
                            ( state-assignments1 ((sxpath `(nml:StateAssignment)) c))
                            ( time-derivatives1 ((sxpath `(nml:TimeDerivative)) c))
                            )
                        
                        (let ((ode-variables1 (map (lambda (x) 
                                                     (string->symbol (sxml:attr x 'variable )))
                                                   time-derivatives1))
                              
                              (ode-rhss1      (map (lambda (x)
                                                     (parse-string-expr 
                                                      (sxml:kidn-cadr 'nml:MathInline x )
                                                      'parse-al-sxml-dynamics))
                                                   time-derivatives1))
                              )
                          
                          (if (not trigger) (error 'parse-al-sxml-dynamics "on-condition without trigger" c))
                          (if (not event-out) (error 'parse-al-sxml-dynamics "on-condition without event-out" c))
                          
                          (let ((trigger-rhs (parse-string-expr 
                                              (sxml:text trigger) 
                                              'parse-al-sxml-dynamics))
                                (trigger-name (string->symbol (sxml:attr event-out 'port )))
                                (assign-variables1 (map (lambda (x) 
                                                         (string->symbol (sxml:attr  x 'variable))) 
                                                       state-assignments1))
                                (assign-rhss1      (map (lambda (x)
                                                          (parse-string-expr 
                                                           (sxml:kidn-cadr 'nml:MathInline x) 
                                                           'parse-al-sxml-dynamics))
                                                        state-assignments1)))
                            
                            ((lambda (sys)
                               (if on-event 
                                   (make-event on-event sys aliases)
                                   sys))
                                   
                             (make-transient relations aliases 
                                             ode-variables ode-rhss assign-variables assign-rhss
                                             trigger-name trigger-rhs 
                                             ode-variables1 ode-rhss1 assign-variables1 assign-rhss1))
                            
                            ))
                        ))
                    ))
              ))
          )

	 ((= (length regimes) 2)

          (let ((rs regimes)
                (state-name (gensym 'st)))

            (let (
                  (time-derivatives  (map (sxpath `(nml:TimeDerivative)) rs))
                  (on-conditions     (map (sxpath `(nml:OnCondition)) rs))
                  (on-events         (map (sxpath `(nml:OnEvent)) rs))
                  )
              
              (for-each 
               (lambda (r cs evs)
                 (cond 
                  ((null? cs)
                   (error 'parse-al-sxml-dynamics "regime does not contain on-conditions blocks" rs))
                  ((> (length cs) 1)
                   (error 'parse-al-sxml-dynamics "multiple on-conditions blocks in regime are not supported" r))
                  ((> (length evs) 1)
                   (error 'parse-al-sxml-dynamics "multiple on-events in regime are not supported" r))
                  ))
               rs on-conditions on-events)
              

              (if (every (lambda (x) (null? x)) time-derivatives)
                  (error 'parse-al-sxml-dynamics "regime list does not contain time derivative blocks" rs))

              (let (
                    (relations
                     (map (lambda (x)
                            (let ((quantity (sxml:attr x 'name))
                                  (var      (sxml:attr x 'argument))
                                  (rhs      (parse-string-expr 
                                             (sxml:kidn-cadr 'nml:MathInline x )
                                             'parse-al-sxml-dynamics)))
                              (make-relation (string->symbol quantity)
                                             (string->symbol var)
                                             (make-signal-expr rhs argument: (string->symbol var) rhs))
                              ))
                          relations))

                    (regimes
                     (map
                      (lambda (r time-derivatives on-conditions on-events)

                        (let ((ode-variables (map (lambda (x) 
                                                    (string->symbol (sxml:attr x 'variable )))
                                                  time-derivatives))
                                  
                              (ode-rhss      (map (lambda (x)
                                                    (parse-string-expr 
                                                     (sxml:kidn-cadr 'nml:MathInline x )
                                                     'parse-al-sxml-dynamics))
                                                  time-derivatives))

                              (state-assignments ((sxpath `(nml:StateAssignment)) r))
                              
                              (c (and (not (null? on-conditions)) (car on-conditions)))
                              )
                            
                          (let (
                                ( trigger (sxml:kidn-cadr 'nml:Trigger c))
                                ( event-out (sxml:kidn 'nml:EventOut c))
                                )
                            
                            (if (not trigger) 
                                (error 'parse-al-sxml-dynamics "on-condition without trigger" c))

                            (if (not event-out) 
                                (error 'parse-al-sxml-dynamics "on-condition without event-out" c))
                            
                            (let* ((trigger-name (string->symbol (sxml:attr event-out 'port )))
                                   
                                   (trigger-rhs (parse-string-expr 
                                                 (sxml:text trigger) 
                                                 'parse-al-sxml-dynamics))
                                   
                                   (c-state-assignments ((sxpath `(nml:StateAssignment)) c))
                                   
                                   (assign-variables (map (lambda (x) 
                                                            (string->symbol (sxml:attr  x 'variable)))
                                                          state-assignments))

                                   (c-assign-variables (map (lambda (x) 
                                                              (string->symbol (sxml:attr  x 'variable)))
                                                            c-state-assignments))
                                   
                                   (assign-rhss      (map (lambda (x)
                                                            (parse-string-expr 
                                                             (sxml:kidn-cadr 'nml:MathInline x) 
                                                             'parse-al-sxml-dynamics))
                                                          state-assignments))

                                   (c-assign-rhss      (map (lambda (x)
                                                              (parse-string-expr 
                                                               (sxml:kidn-cadr 'nml:MathInline x) 
                                                               'parse-al-sxml-dynamics))
                                                            c-state-assignments))

                                   )
                              (list ode-variables ode-rhss trigger-name trigger-rhs assign-variables assign-rhss
                                    c-assign-variables c-assign-rhss
                                    (and (not (null? on-events)) (car on-events)))
                              
                              ))
                        ))
                      regimes time-derivatives on-conditions on-events))

                    )

                (match-let ((((ode-variables1 ode-rhss1 trigger-name1 trigger-rhs1 assign-variables1 assign-rhss1 
                                              c-assign-variables1 c-assign-rhss1 e1)
                              (ode-variables2 ode-rhss2 trigger-name2 trigger-rhs2 assign-variables2 assign-rhss2 
                                              c-assign-variables2 c-assign-rhss2 e2))
                             regimes))

                      (make-rtransition state-name relations aliases
                                        ode-variables1 ode-rhss1 trigger-name1 trigger-rhs1 assign-variables1 assign-rhss1 
                                        c-assign-variables1 c-assign-rhss1 e1
                                        ode-variables2 ode-rhss2 trigger-name2 trigger-rhs2 assign-variables2 assign-rhss2 
                                        c-assign-variables2 c-assign-rhss2 e2))
                
                ))
            ))


         ((> (length regimes) 2)
          (error 'parse-al-sxml-dynamics "maximum of two regimes is supported" sxml))
         
         )
        
        (error 'parse-al-sxml-dynamics "no regimes found in component" )

        )
    ))
  
  

(define (parse-al-sxml-alsys sxml)
  (let (
        (state-variables   ((sxpath `(// nml:StateVariable)) sxml))
	(state-assignments ((sxpath `(// nml:Equation)) sxml))
        (relations         ((sxpath `(// nml:Relation)) sxml))
        )

    (let*
        (
         (assign-variables (map (lambda (x) 
                                  (string->symbol (sxml:attr x 'variable)))
                                state-assignments))
         
         (assign-rhss      (map (lambda (x)
                                  (parse-string-expr 
                                   (sxml:kidn-cadr 'nml:MathInline x) 
                                   'parse-al-sxml-alsys))
                                state-assignments))
                    

         (relations        (map (lambda (x)
                                   (let ((quantity (sxml:attr x 'name))
                                         (var      (sxml:attr x 'argument))
                                         (rhs      (parse-string-expr 
                                                   (sxml:kidn-cadr 'nml:MathInline x )
                                                   'parse-al-sxml-alsys)))
                                     (make-relation (string->symbol quantity)
                                                    (string->symbol var)
                                                    (make-signal-expr rhs argument: (string->symbol var)))
                                     ))
                                 relations))

         (assignments
          (and (not (null? assign-variables))
               (make-alsys-eqn-lst-expr
                (map (lambda (var rhs) (make-algebraic-eqn var (make-signal-expr rhs))  )
                     assign-variables assign-rhss))))
         )
  
      (make-alsys-relations relations assignments)

      ))
  )



(define (parse-al-sxml-component sxml)

  (define pair (Longid (Pident (ident-create "pair"))))

  (let* (
         (name         (sxml:attr sxml 'name))
         (dynamics     (safe-car ((sxpath `(// nml:Dynamics)) sxml)))
         (alsys        (safe-car ((sxpath `(// nml:AlgebraicSystem)) sxml)))
         (parameters   ((sxpath `(// nml:Parameter))  sxml))
         (ports        (filter-map
                        (lambda (x)
                          (let ((mode (sxml:attr x 'mode)))
                            (and (or (not mode) (not (string=? mode "send"))) x)))
                        ((sxpath `(// (*or* 
                                       nml:AnalogSendPort 
                                       nml:AnalogReceivePort 
                                       nml:AnalogReducePort 
                                       nml:EventSendPort
                                       nml:EventReceivePort
                                       )))  sxml)))
         (states       ((sxpath `(// nml:StateVariable)) dynamics))
         (connection-rule (safe-car ((sxpath `(// nml:ConnectionRule)) sxml)))
         )

    (cond

     (connection-rule 

      (let ((connection-body
             (Apply
              (Apply pair (Const `(label stdlib)))
              (Apply
               (Apply pair
                      (Const `(string ,(sxml:attr connection-rule 'standardLibrary))))
               (Longid (Pident (ident-create "empty"))))))
            (connection-args
             (map (lambda (x) (sxml:attr x 'name)) 
                  (append (reverse ports)
                          (reverse parameters))))
            )
        (Value_def (ident-create name) 
                   (let recur ((args connection-args) (ax connection-body))
                     (if (null? args) (Function (ident-create "_") ax)
                         (recur (cdr args) (Function (ident-create (car args)) ax)))))
             ))
        

     (dynamics 

      (let ((dynamics-body (parse-al-sxml-dynamics dynamics))
            
            (dynamics-args
             (cons* "h" "t" (map (lambda (x) (sxml:attr x 'name)) 
                                 (append (reverse states)
                                         (reverse ports)
                                         (reverse parameters)))))
            )
        
        (Value_def (ident-create name) 
                   (let recur ((args dynamics-args) (ax dynamics-body))
                     (if (null? args) ax
                         (recur (cdr args) (Function (ident-create (car args)) ax)))))
        ))
     
     (alsys
      
      (let ((alsys-body (parse-al-sxml-alsys alsys))
            
            (alsys-args
             (map (lambda (x) (sxml:attr x 'name)) 
                  (append (reverse ports)
                          (reverse parameters))))
                 )
        
             (Value_def (ident-create name) 
                        (let recur ((args alsys-args) (ax alsys-body))
                          (if (null? args) ax
                              (recur (cdr args) (Function (ident-create (car args)) ax)))))
             ))
     
     (else
      (error 'parse-al-sxml-component "component class does not contain dynamics or a linear system"))

     )
    ))
           


(define (parse-al-sxml al-sxml)
  (let ((al-sxml-defs ((sxpath `(// nml:ComponentClass))  al-sxml)) )

    (map parse-al-sxml-component al-sxml-defs)

    ))

(register-macro-hook 'default parse-NineML-equation-sexpr-macro)
(register-macro-hook 'list parse-list-sexpr-macro)

)
