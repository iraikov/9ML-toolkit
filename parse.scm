;;
;; A parser for NineML.
;;
;; Copyright 2010-2015 Ivan Raikov
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

	(parse-string-expr parse-sym-expr make-signal-expr
	 nineml-xmlns-base parse-al-sxml-component parse-al-sxml
         parse-sxml-unit parse-sxml-dimension)

	(import scheme chicken)
	(require-library srfi-1 srfi-4 srfi-13 data-structures extras)
	(import
	 (only srfi-1 concatenate fold combine any every unzip2 filter-map partition delete-duplicates cons* lset-difference)
         (only srfi-4 s32vector)
	 (only srfi-13 string-null?)
	 (only data-structures conc ->string alist-ref)
	 (only extras fprintf pp))

	(require-extension lalr-driver salt unitconv matchable)
	(require-extension sxpath sxpath-lolevel)
	(require-library sxml-transforms)
	(import (prefix sxml-transforms sxml:))

	(include "SXML.scm")

        (require-extension 9ML-types)

(define (safe-car x) (and (pair? x) (car x)))


(define-syntax tok
  (syntax-rules ()
    ((tok loc t) (make-lexical-token (quasiquote t) loc #f))
    ((tok loc t l) (make-lexical-token (quasiquote t) loc l))))


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

(define nineml-xmlns-base "http://nineml.net/9ML/")


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
    (string->symbol (conc "signal." name))
    ))
    

(define (signal-operation? op)
  (case op
    ((add mul div gt gte lte neg cosh tanh log ln exp) #t)
    (else #f)))


(define (random-operation? op)
  (case op
    ((random.int random.normal random.uniform random.unifrange random.poisson random.exponential) #t)
    (else #f)))

    

(define (make-signal-expr expr #!key (subst '()) (argument #f))

  (let recur ((expr expr))

    (cond ((number? expr) 
           (constant 'number expr unitless))

          ((symbol? expr) 
           (case expr 
             ((false) (constant 'bool #f))
             ((true)  (constant 'bool #t))
             (else 
              (let ((v (alist-ref expr subst)))
                (cond (v (make-signal-expr v subst: '() argument: argument))
                      
                      ((equal? argument expr)
                       `(signal ,(string->symbol (->string expr))))
                      
                      (else (string->symbol (->string expr))))))))

	(else
	 (match expr

		(('- a)  
		 `(,(op->signal-function "neg") ,(recur a)))

		(('- a b)  
		 `(,(op->signal-function "sub") ,(recur a) ,(recur b)))
		
		(('if a b c)  
		 `(,(op->signal-function "if") ,(recur a) ,(recur b) ,(recur c)))
		
		(((and op (? symbol?)) a b)
		 (cond ((signal-operation? op)
                        `(,(op->signal-function op)  ,(recur a) ,(recur b)))
                       ((random-operation? op)
                        `(,op ,(recur a) ,(recur b)))
                       (else
                        `(,op ,(recur a) ,(recur b)))))
		
		(((and op (? symbol?)) a)
		 (cond ((signal-operation? op)
                        `(,(op->signal-function op)  ,(recur a)))
                       ((random-operation? op)
                        `(,op ,(recur a)))
                       (else
                        `(,op ,(recur a)))))

		(((and op (? symbol?)))
		 (cond
                  ((random-operation? op)
                   `(,op (constant 'empty)))
                  (else
                   (error 'make-signal-expr "invalid signal expression" expr))))
		
		(else (error 'make-signal-expr "invalid signal expression" expr))))
	))
)


(define (subst-expr expr subst)

  (let recur ((expr expr))

    (cond ((number? expr)  expr)

          ((symbol? expr) 
           (let ((v (alist-ref expr subst)))
             (or v expr)))

          ((pair? expr) (map recur expr))

          (else expr)
          ))
)





(define (parse-al-sxml-dynamics formals sxml)

  ;; TODO: ensure that parameters and state variables are consistent in the equations

  (define (rewrite-trigger rhs)
    (match rhs
           ((x '>= y) `(,x - ,y))
           ((x '> y)  `(,x - ,y))
           ((x '<= y) `(,y - ,x))
           ((x '< y)  `(,y - ,x))
           (else rhs)))

  (let (
        (state-variables  ((sxpath `(// nml:StateVariable)) sxml))
	(regimes          ((sxpath `(// nml:Regime)) sxml))
        (relations        ((sxpath `(// nml:Relation)) sxml))
        (constants        ((sxpath `(// nml:Constant)) sxml))
        (aliases          ((sxpath `(// nml:Alias)) sxml))
        )

    (let* (
          (state-names (map (lambda (x) (string->symbol (sxml:attr x 'name))) state-variables))
          (ode-state-names 
           (delete-duplicates
            (fold
             (lambda (regime lst)
               (let (
                     (time-derivatives  ((sxpath `(nml:TimeDerivative)) regime))
                     )
                 (append
                  (map (lambda (x) (string->symbol (sxml:attr x 'variable ))) time-derivatives)
                  lst)
                 ))
             '() regimes)))

          (relations-decls
           (map (lambda (x)
                  (let ((quantity (string->symbol (sxml:attr x 'name)))
                        (var      (sxml:attr x 'argument))
                        (rhs      (parse-string-expr 
                                   (sxml:kidn-cadr 'nml:MathInline x )
                                   'parse-al-sxml-dynamics)))
                    `(fun (,quantity ,var) = ,rhs)
                    ))
                relations))

          (subst-env
           (filter-map
            (lambda (x)
              (let ((quantity (string->symbol (sxml:attr x 'name)))
                    (rhs      (parse-string-expr 
                               (sxml:kidn-cadr 'nml:MathInline x )
                               'parse-al-sxml-dynamics)))
                (and (not (assoc quantity formals))
                     `(,quantity . ,rhs))
                ))
            aliases))
                      
          (assign-eqs
           (filter-map
            (lambda (x)
              (let ((quantity (string->symbol (sxml:attr x 'name)))
                    (rhs      (parse-string-expr 
                               (sxml:kidn-cadr 'nml:MathInline x )
                               'parse-al-sxml-dynamics)))
                (and (assoc quantity formals) 
                    `((reduce (+ ,quantity)) = ,rhs))
                ))
            aliases))
           
          (constant-decls
           (map (lambda (x)
                  (let ((name  (string->symbol (sxml:attr x 'name)))
                        (units (sxml:attr x 'units))
                        (rhs   (parse-string-expr 
                                (sxml:text x)
                                'parse-al-sxml-dynamics)))
                    (if units
                        `(define ,name = constant (unit ,(string->symbol units)) ,rhs)
                        `(define ,name = constant  ,rhs))
                    ))
                constants))


          (regimes-decls 
           (fold
            (lambda (regime lst)
              (let (
                    (regime-name       (string->symbol (sxml:attr regime 'name )))
                    (time-derivatives  ((sxpath `(nml:TimeDerivative)) regime))
                    (on-events         ((sxpath `(nml:OnEvent)) regime))
                    (on-conditions     ((sxpath `(nml:OnCondition)) regime))
                    )
                (let* (
                      (ode-decls
                       (match-let (((vars decls)
                                    (fold (match-lambda*
                                           ((x (vars decls))
                                            (let ((var (string->symbol (sxml:attr x 'variable )))
                                                  (rhs (subst-expr
                                                        (parse-string-expr 
                                                         (sxml:kidn-cadr 'nml:MathInline x )
                                                         'parse-al-sxml-dynamics)
                                                        subst-env)))
                                              (list (cons var vars)
                                                    (cons `((der (,var)) = ,rhs) decls)))))
                                          '(() ()) time-derivatives)))
                                  (match-let (((vars decls)
                                               (fold (match-lambda*
                                                      ((var (vars decls))
                                                       (if (member var vars)
                                                           (list vars decls)
                                                           (if (member var ode-state-names)
                                                               (list (cons var vars)
                                                                     (cons `((der (,var)) = UNITZERO) decls))
                                                               (list vars decls)
                                                               ))
                                                       ))
                                                     `(,vars ,decls) state-names)))
                                             decls)))
                      (event-decls 
                       (map (lambda (e)
                              (let*
                                  (
                                   (e-state-assignments
                                    ((sxpath `(nml:StateAssignment)) e))
                                   (e-assign-variables
                                    (map (lambda (x) 
                                           (string->symbol (sxml:attr  x 'variable))) 
                                         e-state-assignments))
                                   (e-assign-rhss
                                    (map (lambda (x)
                                           (subst-expr
                                            (parse-string-expr 
                                             (sxml:kidn-cadr 'nml:MathInline x) 
                                             'parse-al-sxml-dynamics)
                                            subst-env))
                                         e-state-assignments))
                                   (e-port
                                    (string->symbol (or (sxml:attr e 'src_port)
                                                        (sxml:attr e 'port))))
                                   )
                                `(event ,e-port ,(map (if (> (length regimes) 1)
                                                          (lambda (var rhs) `(,var := if ,regime-name then ,rhs else ,var))
                                                          (lambda (var rhs) `(,var := ,rhs)))
                                                      e-assign-variables e-assign-rhss))
                                ))
                            on-events))
                      
                      (transition-decls
                       (map
                        (lambda (c)
                          (let (
                                ( trigger (sxml:kidn-cadr 'nml:Trigger c))
                                ( event-output ((lambda (x) (or (and x (string->symbol (sxml:attr x 'port)))
                                                                (gensym 'event)))
                                                (sxml:kidn 'nml:OutputEvent c)))
                                ( target-regime ((lambda (x) (or (and x (string->symbol x)) regime-name))
                                                 (sxml:attr c 'target_regime)) )
                                )
                            
                            (if (not trigger) 
                                (error 'parse-al-sxml-dynamics "on-condition without trigger" c))
                            
                            (if (not event-output) 
                                (error 'parse-al-sxml-dynamics "on-condition without output event" c))
                            
                            (let* (
                                   (trigger-rhs (rewrite-trigger
                                                 (parse-string-expr 
                                                  (sxml:text trigger) 
                                                  'parse-al-sxml-dynamics)))
                                   (c-state-assignments ((sxpath `(nml:StateAssignment)) c))
                                   
                                   (c-assign-variables (map (lambda (x) 
                                                              (string->symbol (sxml:attr  x 'variable)))
                                                            c-state-assignments))
                                   
                                   (c-assign-rhss      (map (lambda (x)
                                                              (subst-expr
                                                               (parse-string-expr 
                                                                (sxml:kidn-cadr 'nml:MathInline x) 
                                                                'parse-al-sxml-dynamics)
                                                               subst-env))
                                                            c-state-assignments))
                                   )
                              `(,event-output 
                                ,target-regime ,trigger-rhs 
                                ,(map (lambda (var rhs) `(,var := ,rhs))
                                      c-assign-variables c-assign-rhss))
                              
                              ))
                          )
                        on-conditions))
                      
                      )

                  ;;(pp `(transition-decls . ,transition-decls) (current-error-port))

                  (append 
                   constant-decls
                   assign-eqs
                   (if (null? on-conditions)
                       (append ode-decls event-decls)
                       (cons `(structural-event 
                               ,regime-name
                               ,ode-decls
                               . ,transition-decls)
                             event-decls)) lst)
                  ))
              )
            '() regimes))
          )
                                 
      `(
        (state-names . ,state-names)
        (ode-state-names . ,ode-state-names)
        (decls . ,(append relations-decls regimes-decls))
        )

      ))
  )
  
  

(define (parse-al-sxml-alsys sxml)
  (let (
        (state-variables   ((sxpath `(// nml:StateVariable)) sxml))
	(state-assignments ((sxpath `(// nml:Equation)) sxml))
        (relations         ((sxpath `(// nml:Relation)) sxml))
        )

    (let*
        (
          (relations-decls
           (map (lambda (x)
                  (let ((quantity (sxml:attr x 'name))
                        (var      (sxml:attr x 'argument))
                        (rhs      (parse-string-expr 
                                   (sxml:kidn-cadr 'nml:MathInline x )
                                   'parse-al-sxml-dynamics)))
                    `(fun (,quantity ,var) = ,rhs)
                    ))
                relations))

          (assign-variables
           (map (lambda (x) 
                  (string->symbol (sxml:attr x 'variable)))
                state-assignments))
          
          (assign-rhss
           (map (lambda (x)
                  (parse-string-expr 
                   (sxml:kidn-cadr 'nml:MathInline x) 
                   'parse-al-sxml-alsys))
                state-assignments))

          (assign-decls
           (map (lambda (var rhs) `(,var = ,rhs))
                assign-variables assign-rhss))

         )
  
      (append relations assign-decls)

      ))
  )



(define (parse-al-sxml-component sxml)

  (let* (
         (name         (string->symbol (sxml:attr sxml 'name)))
         (dynamics     (safe-car ((sxpath `(// nml:Dynamics)) sxml)))
         (alsys        (safe-car ((sxpath `(// nml:AlgebraicSystem)) sxml)))
         (parameters   ((sxpath `(// nml:Parameter))  sxml))
         (input-ports (filter-map
                       (lambda (x)
                         (let ((mode (sxml:attr x 'mode)))
                           (and (or (not mode) (not (string=? mode "send"))) x)))
                       ((sxpath `(// (*or* 
                                      nml:AnalogReceivePort 
                                      nml:AnalogReducePort 
                                      nml:EventReceivePort
                                      )))  sxml)))
         (output-ports (filter-map
                        (lambda (x)
                          (let ((mode (sxml:attr x 'mode)))
                            (and (or (not mode) (not (string=? mode "send"))) x)))
                        ((sxpath `(// (*or* 
                                       nml:AnalogSendPort 
                                       nml:EventSendPort
                                       )))  sxml)))
         (ports        (append input-ports output-ports))
         (states       ((sxpath `(// nml:StateVariable)) dynamics))
         (connection-rule (safe-car ((sxpath `(// nml:ConnectionRule)) sxml)))
         (random-dist (safe-car ((sxpath `(// nml:RandomDistribution)) sxml)))
         )

    (cond

     (dynamics 
      (let* (
             (dynamics-formals
              (delete-duplicates
               (map (lambda (x) 
                      (let ((name (sxml:attr x 'name))
                            (dimension (sxml:attr x 'dimension)))
                        (cons (string->symbol name)
                              (or (and dimension (string->symbol dimension))
                                  'unitless))))
                    (append (reverse ports)
                            (reverse parameters)))))
             (dynamics-info (parse-al-sxml-dynamics dynamics-formals dynamics))
             (dynamics-body (alist-ref 'decls dynamics-info))
             (dynamics-env `((states . ,(alist-ref 'state-names dynamics-info))
                             (ode-states . ,(alist-ref 'ode-state-names dynamics-info))
                             (outputs . ,(map (lambda (x) 
                                                (let ((name (sxml:attr x 'name)))
                                                  (string->symbol name)))
                                              output-ports))
                             (inputs . ,(map (lambda (x) 
                                               (let ((name (sxml:attr x 'name)))
                                                 (string->symbol name)))
                                             input-ports))
                             ))
             (dynamics-body (alist-ref 'decls dynamics-info))
             )
        (make-dynamics-node name
                            dynamics-formals 
                            dynamics-env 
                            dynamics-body)
        ))
     
     (connection-rule 
      (let (
            (connection-stdlib
             (string->symbol (or (sxml:attr connection-rule 'standard_library)
                                 (sxml:attr connection-rule 'standardLibrary))))
            (connection-formals
             (map (lambda (x) (string->symbol (sxml:attr x 'name)))
                  (append (reverse ports)
                          (reverse parameters))))
            )
        (make-connection-rule-node name connection-formals connection-stdlib)
        ))
        
     (random-dist 
      (let (
            (random-stdlib
             (string->symbol (or (sxml:attr random-dist 'standard_library)
                                 (sxml:attr random-dist 'standardLibrary))))
            (dist-formals
             (map (lambda (x) (string->symbol (sxml:attr x 'name)))
                  (reverse parameters)))
            )
        (make-random-dist-node name dist-formals random-stdlib)
        ))
        

     (alsys
      (let (
            (alsys-body (parse-al-sxml-alsys alsys))
            
            (alsys-formals
             (map (lambda (x) (string->symbol (sxml:attr x 'name)))
                  (append (reverse ports)
                          (reverse parameters))))
            )
        
        (make-alsys-node name alsys-formals alsys-body)
        ))
     
     (else
      (error 'parse-al-sxml-component "component class does not contain dynamics or a linear system"))

     )
    ))
           


(define (parse-al-sxml al-sxml)
  (let ((al-sxml-defs ((sxpath `(// nml:ComponentClass))  al-sxml)) )

    (map parse-al-sxml-component al-sxml-defs)

    ))


(define (parse-sxml-unit sxml dim-env)
  (let* (
         (unit-symbol     (string->symbol (sxml:attr sxml 'symbol)))
         (unit-power      (string->number (sxml:attr sxml 'power)))
         (dimension-name  (string->symbol (sxml:attr sxml 'dimension)))
         (dimension-assoc (assv dimension-name dim-env))
         )
    (if (not dimension-assoc)
        (error 'parse-sxml-unit "unknown dimension in unit definition" 
               unit-symbol dimension-name)
        (make-unit unit-symbol (cdr dimension-assoc) (expt 10 unit-power) '()))
    ))

(define (opt-string->number str #!key (default 0.0))
  (or (and str (string->number str)) default))

(define (parse-sxml-dimension sxml)
  (let (
        (name (string->symbol (sxml:attr sxml 'name)))
        (m    (opt-string->number (sxml:attr sxml 'm)))
        (l    (opt-string->number (sxml:attr sxml 'l)))
        (i    (opt-string->number (sxml:attr sxml 'i)))
        (n    (opt-string->number (sxml:attr sxml 'n)))
        (k    (opt-string->number (sxml:attr sxml 'k)))
        (j    (opt-string->number (sxml:attr sxml 'j)))
        (t    (opt-string->number (sxml:attr sxml 't)))
        )

    (let ((dims (s32vector l t k m i j n 0 0)))

      (make-quantity name (dimint dims))

    ))
  )
    

)
