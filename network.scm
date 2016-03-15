;;
;; NineML network level descriptions.
;;
;;
;; Copyright 2015 Ivan Raikov
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


(require-extension extras posix utils files data-structures srfi-1 srfi-13 irregex)
(require-extension datatype matchable make ssax sxml-transforms sxpath sxpath-lolevel 
                   object-graph ersatz-lib unitconv getopt-long )
(require-extension 9ML-types 9ML-parse 9ML-ivp-mlton 9ML-utils)

(require-library ersatz-lib salt)
(import (prefix ersatz-lib ersatz: )
        (prefix salt salt: ))



(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))


(define $ string->symbol)
(define (s+ . rest) (string-concatenate (map ->string rest)))

(define (alist->tenv xs)
  (map (lambda (x) (cons (car x) (ersatz:sexpr->tvalue (cdr x)))) xs))

(define (string->bool x)
  (cond ((string=? x "true") #t)
        ((string=? x "false") #f)
        (else (error 'string->bool "invalid boolean string" x))))

(define (warn port message . specialising-msgs)
  (print-error-message message (current-output-port) "Warning")
  (print (string-concatenate (map ->string specialising-msgs))))

	
(include "SXML.scm")
(include "SXML-to-XML.scm")
(include "stx-engine.scm")

(define ivp-simulation-platform (make-parameter 'mlton))
(define alsys-simulation-platform (make-parameter 'mlton))
(define ivp-simulation-method (make-parameter 'rkfe))

(define opt-defaults
  `(
    (platform . mlton)
    (method . rk3)
    ))

(define (defopt x)
  (lookup-def x opt-defaults))

(define opt-grammar
  `(

    (platform        "simulation platform (one of mlton, chicken, chicken/cvode)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((chicken chicken/cvode mlton octave/mlton) s)
				    (else (error '9ML-network "unrecognized platform" x))))))
			    (transformer ,string->symbol)
                            ))

    (method        "integration method (one of rkfe, rk3, rk4a, rk4b, rkhe, rkbs, rkf45, rkv65, rkf78, rkoz, rkdp, crk3, crkdp, crkbs)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((rkfe rk3 rk4a rk4b rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crk3 crkdp crkbs ) s)
				    (else (error '9ML-network "unrecognized method" x))))))
			    (transformer ,string->symbol)
                            )
                     (single-char #\m)
                     )

    (verbose          "print commands as they are executed"
		      (single-char #\v))

    (trace        "trace one or more procedures in the code generation backend"
                  (value (required NAMES)
                         (transformer ,(lambda (x) (map string->symbol (string-split x ","))))
                         )
                  )

    (help  "Print help"
	    (single-char #\h))
  
  ))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (network:usage)
  (print "Usage: " (car (argv)) " file1... [options...] ")
  (newline)
  (print "Where operands are NineML user layer files")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (width 45)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))


;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))

(define network-verbose (make-parameter 0))
(define data-dir (make-parameter #f))
(define simulation-platform (make-parameter #f))
(define simulation-method (make-parameter 'rk3))


(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (network-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))



(define (get-data-dir)
  (or (opt 'data-dir)
      (or (data-dir)
	  (let ([dir (create-temporary-directory)])
	    (data-dir dir)
	    dir ) ) ))


(define (run:execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd) (system (->string cmd)))
	    (map smooth explist)))


(define (run:execute* explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (car slst) (cdr slst)) " ")))
  (for-each (lambda (cmd) (system* "~a" cmd))
	    (map smooth explist)))


(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (begin
       (d "running ~A ...~%" (list `exp ...))
       (run:execute* (list `exp ...))))))


(define-syntax run-
  (syntax-rules ()
    ((_ exp ...)
     (begin
       (d "running ~A ...~%" (list `exp ...))
       (run:execute (list `exp ...))))))


(define (create-temporary-directory)
  (let ((dir (or (get-environment-variable "TMPDIR") 
		 (get-environment-variable "TEMP") 
		 (get-environment-variable "TMP") 
		 "/tmp")))
    (let loop ()
      (let* ((n (current-milliseconds))
	     (pn (make-pathname dir (string-append "9ML-network-" (number->string n 16)) "tmp")))
	(cond ((file-exists? pn) (loop))
	      (else (mkdir pn) pn))))))





(define (make-population-tenv name prototype size order)
  (alist->tenv
   `((name      . ,name)
     (prototype . ,prototype)
     (size      . ,size)
     (start     . ,order))
   ))


(define (population= x y) (equal? (car x) (car y)))


(define (make-population-set node populations)
  (let*
      (

       (concatenate-template 
	(sxml:match 'nml:Concatenate
		    (lambda (node bindings root env) 
                      (let ((kids (sxml:kids node)))
                        (fold (lambda (x ax)
                                (lset-union population= 
                                    (make-population-set x populations) ax))
                              '() kids)
                        ))
                    ))

       (union-template 
	(sxml:match 'nml:Union
		    (lambda (node bindings root env) 
		      (let ((left (sxml:kidn* 'nml:Left node))
                            (right (sxml:kidn* 'nml:Right node)))
                        (lset-union population=
                                    (make-population-set (sxml:kid left) populations)
                                    (make-population-set (sxml:kid right) populations))
                        ))
                    ))

       (intersection-template 
	(sxml:match 'nml:Intersection
		    (lambda (node bindings root env) 
		      (let ((left (sxml:kidn* 'nml:Left node))
                            (right (sxml:kidn* 'nml:Right node)))
                        (lset-intersection population=
                                           (make-population-set (sxml:kid left) populations)
                                           (make-population-set (sxml:kid right) populations))
                        ))
                    ))

       (difference-template 
	(sxml:match 'nml:Difference
		    (lambda (node bindings root env) 
		      (let ((left (sxml:kidn* 'nml:Left node))
                            (right (sxml:kidn* 'nml:Right node)))
                        (lset-difference population=
                                         (make-population-set (sxml:kid left) populations)
                                         (make-population-set (sxml:kid right) populations))
                        ))
                    ))

       (singleton-template 
	(sxml:match 'nml:Item
		    (lambda (node bindings root env) 
		      (let* ((ref (sxml:kidn* 'nml:Reference node))
                             (name ($ (sxml:text ref))))
                        (d "Item: node = ~A name = ~A~%" node name)
                        (let ((population (lookup-def name populations)))
                          (if population
                              `((,name . ,population))
                              (error 'make-population-set "unknown population" name))
                          ))
                      )))
       )

    (stx:apply-templates 
     node
     (sxml:make-null-ss union-template
                        intersection-template
                        difference-template
                        singleton-template
                        concatenate-template)
     node (list))
    ))

    
    
(define (make-population-set-tenv name populations)
  (alist->tenv
   `((name        . ,name)
     (populations . ,(map cdr populations))
     (size        . ,(fold + 0 (map (lambda (x) 
                                      (ersatz:tvalue->sexpr (alist-ref 'size (cdr x)))) 
                                    populations)))
     )))
  

(define (make-projection-tenv name type source destination connectivity response response-ports plasticity del properties)
    (d "make-projection-tenv: type = ~A~%" type)
    (d "make-projection-tenv: source = ~A~%" source)
    (d "make-projection-tenv: destination = ~A~%" destination)
    (d "make-projection-tenv: connectivity = ~A~%" connectivity)
    (d "make-projection-tenv: delay = ~A~%" del)
    (d "make-projection-tenv: properties = ~A~%" properties)
    (alist->tenv
     `((name          . ,name)
       (type          . ,type)
       (source        . ,source)
       (destination   . ,destination)
       (connectivity  . ,connectivity)
       (response      . ,response)
       (responsePorts . ,response-ports)
       (plasticity    . ,plasticity)
       (delay         . ,del)
       (properties    . ,properties)
       )
     ))
  
  




(define (make-connection-tenv prefix name node-env)

  (let ((sys-name ($ (->string name))))

  (cond

   ((lookup-def sys-name node-env) =>
    (match-lambda (($ connection-rule-node name connection-formals connection-rule)
                   (alist->tenv
                    (append
                    `((name  . ,sys-name)
                      (stdlib . ,(car connection-rule)))
                    (cdr connection-rule)
                    )))
                  (else (error 'make-connection-tenv "unknown stdlib connection"))))

   ((lookup-def sys-name node-env) =>
    (lambda (sdinfo)

      (let ((dvars   (lookup-def 'dvars sdinfo))
            (ic      (lookup-def 'initial-conditions sdinfo))
            (params  (lookup-def 'params sdinfo))
            (sysFn   (lookup-def 'sys-id sdinfo)))
        (let* (
               (states               dvars)
               (icstates             (filter (lambda (x) (member (car x) states)) ic))
               (initialExpr/ML       (mlton-initial (append ic params)))
               (initialStateExpr/ML  (and (not (null? icstates)) (mlton-initial icstates)))
               )
          (d "NineML make-connection-tenv: states = ~A ics = ~A~%" states (map car ic))
          (alist->tenv
           `((name               . ,name)
             (sysFn              . ,sysFn)
             (states             . ,states)
             ))
          ))
      ))
   (else
    (error 'make-connection-tenv "unknown connection equation system" name))
   ))
  )


(define (make-group-tenv name order populations sets projections 
                         psr-types plas-types connection-types projection-ports
                         properties)
  (let ((alst 
         `((group 
            . 
            ((name        . ,name)
             (order       . ,order)
             (sets        . ,(alist->tenv sets))
             (populations . ,populations)
             (projections . ,projections)
             (projectionPorts . ,projection-ports)
             (psrtypes    . ,(if (null? psr-types) #f psr-types))
             (plastypes   . ,(if (null? plas-types) #f plas-types))
             (conntypes   . ,(if (null? connection-types) #f connection-types))
             (properties  . ,(if (null? properties) (ersatz:sexpr->tvalue '()) properties))
             ))
           ))
        )
    (alist->tenv alst)))


(define (eval-ul-group operand ul-properties node ul-node-env)

  (define (update-population-prototype-env
           population-prototype-env
           population-names
           response)
    (let recur ((prototypes (population-prototype-env))
                (new-prototypes '()))
      (if (null? prototypes)
          (population-prototype-env new-prototypes)
          (let ((prototype (car prototypes)))
            (if (member (car prototype) population-names)
                (recur (cdr prototypes)
                       (cons (cons (car prototype)
                                   (cons (cadr prototype)
                                         (append (cddr prototype) (list response))))
                             new-prototypes))
                (recur (cdr prototypes)
                       (cons prototype new-prototypes)))
            ))
      ))
      

  (define (projections-range projections)
    (let ((destination-union
           (fold
            (lambda (x ax)
              (let* ((props (cdr x))
                     (destination (ersatz:tvalue->sexpr (alist-ref 'destination props))))
                (lset-union population= (alist-ref 'populations destination) ax)))
            '() projections)))
      (fold (lambda (x ax) (+ (alist-ref 'size x) ax)) 0 destination-union)
      ))

  ;;(pp `("UL node" . ,node) (current-error-port))

  (let* (
         (source-dir       (pathname-directory operand))
         (prefix           (pathname-file operand))
         (group-name       (or (sxml:attr node 'name) (string->symbol prefix)))
         (populations-sxml (sxml:kidsn 'nml:Population node))
         (selections-sxml  (sxml:kidsn 'nml:Selection  node))
         (projections-sxml ((sxpath `(// nml:Projection)) node))
         (properties-sxml  ((sxpath `(// nml:Property)) node))
         )

    (d "UL group: ~A properties: ~A populations: ~A selections: ~A projections: ~A~%" 
       group-name properties-sxml populations-sxml selections-sxml projections-sxml)

    (if (null? populations-sxml)
	(error 'eval-ul-group "group without populations" node))

    (let* ((properties (parse-ul-properties group-name properties-sxml))

           (populations+order
            (fold 
             (lambda (node ax)
               (let ((populations (car ax))
                     (order (cadr ax)))
                 (let* ((name (sxml:attr node 'name))
                        (prototype-name ($ (sxml:text (sxml:kidn* 'nml:Reference (sxml:kidn* 'nml:Cell node)))))
                        (size (eval-ul-property group-name (sxml:kidn* 'nml:Size node)))
                        (size-val (inexact->exact (cdr size))))
                   (list
                    (cons
                     `(,($ name) . ,(make-population-tenv ($ name) `((name . ,prototype-name)) size-val order))
                     populations)
                    (+ size-val order)
                    ))
                 ))
             (list '() 0)
             populations-sxml))
           
           (populations (reverse (car populations+order)))
           (order (cadr populations+order))

           (population-prototype-env
            (make-parameter
             (map (lambda (node)
                    (let* ((name ($ (sxml:attr node 'name)))
                           (prototype-name ($ (sxml:text (sxml:kidn* 'nml:Reference (sxml:kidn* 'nml:Cell node))))))
                      (list name prototype-name)
                      ))
                 populations-sxml)))

           (sets
            (append 
             (map
              (lambda (x) 
                (let ((name (car x)))
                  `(,name . ((name . ,name) 
                             (populations ,(string->symbol (->string (alist-ref 'name (cdr x))))))))
                )
              populations)
              (map
               (lambda (node)
                 (let* ((name (sxml:attr node 'name))
                        (set (make-population-set (sxml:kid node) populations)))
                   `(,($ name) . ((name . ,name)
                                  (populations . ,(map (lambda (x) (string->symbol (->string (alist-ref 'name (cdr x))))) set))))
                   ))
               selections-sxml)))
            
           (sets-tenv
            (append 
             (map
              (lambda (x) 
                (let ((name (car x)))
                  `(,name . ((name . ,name) 
                             (populations . ,(ersatz:sexpr->tvalue (list (cdr x))))
                             (size . ,(alist-ref 'size (cdr x))))
                          )
                  ))
              populations)
             (map
              (lambda (node)
                (let* ((name (sxml:attr node 'name))
                       (set (make-population-set (sxml:kid node) populations)))
                  `(,($ name) . ,(make-population-set-tenv ($ name) set))))
              selections-sxml)))

           (projection-port (make-parameter 0))

           (projections+types
            (map
             (lambda (node)
               (let* (
                      (name          (sxml:attr node 'name))
                      (type          ($ (or (sxml:attr node 'type) "event")))
                      (source-node   (sxml:kidn* 'nml:Source node))
                      (source-name   ($ (sxml:text (sxml:kidn* 'nml:Reference source-node))))
                      
                      (destination-node   (sxml:kidn* 'nml:Destination node))
                      (destination-name   ($ (sxml:text (sxml:kidn* 'nml:Reference destination-node))))
                      (destination-response-ports
                       (let ((from-response
                              (sxml:kidn* 'nml:FromResponse destination-node)))
                         (list ($ (sxml:attr from-response 'send_port))
                               ($ (sxml:attr from-response 'receive_port)))
                         ))
                      
                      (response-node (sxml:kidn* 'nml:Response node))
                      (response-name (and response-node (sxml:text (sxml:kidn* 'nml:Reference response-node ))))
                      
                      (response-plasticity-ports 
                       (if response-node
                           (let ((from-plasticity
                                  (sxml:kidn* 'nml:FromPlasticity response-node )))
                              (d "response-node: response-node = ~A from-plasticity = ~A~%" 
                                 response-node from-plasticity)
                              (list ($ (sxml:attr from-plasticity 'send_port))
                                    ($ (sxml:attr from-plasticity 'receive_port)))
                              )
                           (list 'send_port 'receive_port)))

                      (response-ports `(
                                        (projection-port . ,(projection-port))
                                        (plasticity-ports . ,response-plasticity-ports)
                                        (destination-ports . ,destination-response-ports)
                                        ))
                      
                      (plasticity-node (sxml:kidn* 'nml:Plasticity node))
                      (plasticity-name (sxml:text (sxml:kidn* 'nml:Reference plasticity-node)))

                      (connectivity   (sxml:kidn* 'nml:Connectivity node))
                      (connectivity-name (let ((ref (sxml:kidn* 'nml:Reference connectivity))) 
                                           (and ref (sxml:text ref))))
                      (connectivity-port (let ((st (sxml:kidn* 'nml:Port connectivity))) 
                                           (and st (sxml:text st))))
                      (del (salt:codegen-const-expr (cdr (eval-ul-property name (sxml:kidn* 'nml:Delay node)))))
                      (properties      (parse-ul-properties 
                                        name
                                        (append (sxml:kidsn 'nml:property connectivity)
                                                (sxml:kidsn 'nml:Property connectivity))))
                      )

                     (d "group-ul-eval: projection node = ~A~%" node)
                     (d "group-ul-eval: response = ~A response-name = ~A~%" 
                        response-node response-name)
                     (d "group-ul-eval: response-ports = ~A~%" response-ports)
                     (d "group-ul-eval: delay = ~A~%" del)
                     (d "group-ul-eval: type = ~A ~%" type)
                     (d "group-ul-eval: plasticity = ~A plasticity-name = ~A~%" 
                        plasticity-node plasticity-name)
                     (d "group-ul-eval: properties = ~A ~%" properties)
                     (d "group-ul-eval: connectivity-name = ~A ~%" connectivity-name)
                     (d "group-ul-eval: connectivity-port = ~A ~%" connectivity-port)

                     (projection-port (+ 1 (projection-port)))
                     (let* (
                            (source (lookup-def source-name sets-tenv))
                            (destination (lookup-def destination-name sets-tenv))
                            (connection (and connectivity-name (make-connection-tenv prefix connectivity-name ul-node-env)))
                           )

                       (if (not source)
                           (error 'eval-ul-group "invalid projection source" source))

                       (if (not destination)
                           (error 'eval-ul-group "invalid projection destination" destination))

                       (update-population-prototype-env
                        population-prototype-env
                        (alist-ref 'populations (alist-ref destination-name sets))
                        (cons source-name (cons response-name response-ports)))

                       (list
                        `(,($ name) . ,(make-projection-tenv 
                                        ($ name) type source destination 
                                        `(
                                          (name . ,connectivity-name) 
                                          (port . ,connectivity-port)
                                          (type . ,connection)
                                          )
                                        response-name response-ports
                                        plasticity-name
                                        `((exprML . ,(salt:value->ML del)))
                                        properties))
                        `(
                          ,(and response-name ($ response-name)) 
                          (type       . ,type)
                          (response   . ,response-name) 
                          (ports      . ,response-ports)
                          (projection . ,name)
                          )
                        `(,(and plasticity-name ($ plasticity-name)) 
                          (plasticity . ,plasticity-name) )
                        `(,($ connectivity-name)
                          (connection . ,connection) )
                        )
                       ))
               
               )
             projections-sxml))

           (projections (map car projections+types ))
           
           (psr-types
            (let* ((psrs0 (filter car (map cadr projections+types)))
                   (psr-projections
                    (fold (lambda (x ax)
                            (let* ((psr-name (car x))
                                   (projection (alist-ref 'projection (cdr x)))
                                   (type (alist-ref 'type (cdr x)))
                                   (psr-projections (alist-ref psr-name ax)))
                              (if psr-projections
                                  (alist-update psr-name (cons `(,projection ,type) psr-projections) ax)
                                  (alist-update psr-name (list `(,projection ,type)) ax))))
                          '() psrs0))
                   )
              (map 
               (lambda (x)
                 (let* (
                        (name (car x)) (response (alist-ref 'response (cdr x)))
                        (projection-names (map car (alist-ref name psr-projections)))
                        (projection-types (delete-duplicates (map cadr (alist-ref name psr-projections))))
                        (ports (alist-ref 'ports (cdr x)))
                        )

                   (if (> (length projection-types) 1)
                       (error 'eval-ul-group "different projection types for synapse model" name))

                   `(,name . ((response . ,response)
                              (projections . ,projection-names)
                              (type  . ,(car projection-types))
                              (range . ,(projections-range
                                         (map (lambda (x) (alist-ref ($ x) projections)) 
                                              projection-names)))
                              (ports . ,ports)
                              ))
                   ))
               (delete-duplicates psrs0
                                  (lambda (x y) (eq? (car x) (car y))) ))
              ))
                         
           (plas-types
            (let ((plas-types0 (filter car (map caddr projections+types))))
              (map 
               (lambda (x)
                 (let* ((name (car x)) (plasticity (alist-ref 'plasticity (cdr x))))
                   `(,name . ,plasticity)
                   ))
               (delete-duplicates plas-types0
                                  (lambda (x y) (eq? (car x) (car y)))
                                  plas-types0))
              ))

           (connection-types
            (let ((connection-types0 (filter car (map cadddr projections+types))))
              (map 
               (lambda (x)
                 (let* ((name (car x)) (connection (alist-ref 'connection (cdr x))))
                   `(,name . ,connection)
                   ))
               (delete-duplicates connection-types0
                                  (lambda (x y) (eq? (car x) (car y)))
                                  connection-types0))
              ))
                         
           )

      (d "group-ul-eval: order = ~A~%" order)

      (d "group-ul-eval: projections = ~A~%" projections)
      (d "group-ul-eval: psr-types = ~A~%" psr-types)
      (d "group-ul-eval: plas-types = ~A~%" plas-types)

      (let* (
             (shared-dir    (chicken-home))
             (template-dir  (make-pathname (make-pathname shared-dir "9ML") "templates"))
             (network-tmpl  (case (ivp-simulation-method)
                              ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crkdp crkbs) "Network.sml.adaptive.tmpl")
                              (else "Network.sml.tmpl")))
             (sim-tmpl      (case (ivp-simulation-method)
                              ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crkdp crkbs) "Sim.sml.adaptive.tmpl")
                              (else "Sim.sml.tmpl")))
             (mlb-tmpl      (case (ivp-simulation-method)
                              ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65) "Sim.mlb.adaptive.tmpl")
                              (else "Sim.mlb.tmpl")))
             (makefile-tmpl  "Makefile.tmpl")
             (group-path    (make-pathname source-dir (conc group-name ".sml")))
             (sim-path      (make-pathname source-dir (conc "Sim_" group-name ".sml")))
             (mlb-path      (make-pathname source-dir (conc "Sim_" group-name ".mlb")))
             (exec-path     (make-pathname source-dir (conc "Sim_" group-name)))
             (makefile-path (make-pathname source-dir (conc "Makefile." group-name)))
             
             (projection-ports
              (ersatz:sexpr->tvalue 
               (map (match-lambda
                     ((population node-name . responses)
                      (let ((ports (filter-map
                                    (lambda (x) (alist-ref 'projection-port (cddr x)))
                                    responses)))
                        `(,population . ,ports))))
                    (population-prototype-env))))

             (group-tenv
              (make-group-tenv group-name order populations sets-tenv projections 
                               psr-types plas-types connection-types projection-ports
                               (append properties ul-properties) ))

             )

        (d "projection-ports = ~A~%" (ersatz:tvalue->sexpr projection-ports))
        (d "group-path = ~A~%" group-path)
        (d "group-tenv = ~A~%" (map (lambda (x) (cons (car x) (ersatz:tvalue->sexpr (cdr x)))) group-tenv))
        (d "population-prototype-env = ~A~%" (population-prototype-env))

        (for-each
         (match-lambda
          ((population node-name . responses)
           (match-let
            (
             (($ dynamics-node model-name model-formals model-env model-eqset)
              (alist-ref node-name ul-node-env))
             )
            (d "node name = ~A model-formals = ~A model-eqset = ~A responses = ~A~%" 
               node-name model-formals model-eqset responses)
            (let* ((response-dynamics
                    (map
                     (match-lambda 
                      ((source-population response-node . ports)
                       (if response-node 
                           (match-let (
                                       (($ dynamics-node model-name model-formals model-env model-eqset)
                                        (alist-ref (string->symbol response-node) ul-node-env))
                                       )
                                      model-eqset)
                           (let* ((projection-port (alist-ref 'projection-port ports))
                                  (destination-port (cadr (alist-ref 'destination-ports ports)))
                                  (ext-port (cadr (alist-ref 'plasticity-ports ports)))
                                  (ext-event (gensym 'event))
                                  (dim (alist-ref destination-port model-formals))
                                  (decls `((define ,ext-port = external (dim ,dim) ,destination-port)
                                           (define ,ext-event = external-event +inf.0)
                                           ((reduce (+ ,destination-port)) = ,ext-port)))
                                  )
                             (salt:parse decls)
                             ))
                       ))
                     responses))
                   (prototype-decls
                    (salt:make-astdecls
                     (append (salt:astdecls-decls model-eqset) response-dynamics))))
              (d "response-dynamics = ~A~%" response-dynamics)
              (d "prototype-decls = ~A~%" prototype-decls)
              (let* ((sim (salt:simcreate (salt:elaborate prototype-decls))))
                (let ((sml-port (open-output-file (make-pathname source-dir (sprintf "~A.sml" node-name)))))
                  (salt:codegen-ODE/ML node-name sim out: sml-port solver: (ivp-simulation-method) libs: '(random))
                  (close-output-port sml-port)
                  (case (ivp-simulation-method) 
                    ((crk3 crkbs crkdp)
                     (let ((c-port (open-output-file (make-pathname source-dir (sprintf "~A.c" node-name)))))
                       (salt:codegen-ODE/C node-name sim out: c-port solver: (ivp-simulation-method) libs: '(random))
                       (close-output-port c-port)
                       ))
                    (else (begin)))
                  )
                ))
            ))
          )
         (population-prototype-env))

        (for-each
         (match-lambda
          ((node-name . plas-type)
           (match-let
            (
             (($ dynamics-node model-name model-formals model-env model-eqset)
              (alist-ref node-name ul-node-env))
             )
            (d "plasticity node name = ~A model-eqset = ~A~%" node-name model-eqset)
              (let* ((sim (salt:simcreate (salt:elaborate model-eqset))))
                (let ((port (open-output-file (make-pathname source-dir (sprintf "~A.sml" node-name)))))
                  (salt:codegen-ODE/ML node-name sim out: port solver: (ivp-simulation-method) libs: '(random))
                  (close-output-port port))
                ))
            ))
         plas-types)

        (let ((node-files 
               (map 
                (match-lambda
                 ((population node-name . responses)
                  (make-pathname source-dir (sprintf "~A.sml" node-name))))
                (population-prototype-env))))
          (make/proc
           `((,group-path 
              ,node-files
              ,(lambda ()
                 (with-output-to-file group-path 
                   (lambda ()
                     (print (ersatz:from-file 
                             network-tmpl
                             env: (template-std-env search-path: `(,template-dir))
                             models: group-tenv)))))
              ))
           (list group-path))
          )

        (make (

               (sim-path (group-path)
                         (with-output-to-file sim-path 
                           (lambda ()
                             (print (ersatz:from-file 
                                     sim-tmpl
                                     env: (template-std-env search-path: `(,template-dir))
                                     models: group-tenv))))
                         )
               
               (mlb-path (group-path)
                         (with-output-to-file mlb-path 
                           (lambda ()
                             (print (ersatz:from-file 
                                     mlb-tmpl
                                     env: (template-std-env search-path: `(,template-dir))
                                         models: (append 
                                                  group-tenv
                                                  `((UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                            ((crk3 crkbs crkdp) #t)
                                                                            (else #f))))
                                                    ))
                                         ))
                             ))
                         )

               (makefile-path ()
                              (with-output-to-file makefile-path 
                                (lambda ()
                                  (print (ersatz:from-file 
                                          makefile-tmpl
                                          env: (template-std-env search-path: `(,template-dir))
                                          models: (append 
                                                   group-tenv
                                                   `((sml_lib_home . ,(Tstr (make-pathname 
                                                                             (make-pathname shared-dir "salt")
                                                                             "sml-lib")))
                                                     (nineml_lib_home . ,(Tstr (make-pathname 
                                                                                (make-pathname shared-dir "9ML")
                                                                                "sml-lib")))
                                                     (UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                             ((crk3 crkbs crkdp) #t)
                                                                             (else #f))))
                                                     (CSolverFiles . ,(let ((csolver-path 
                                                                             (make-pathname
                                                                              (make-pathname 
                                                                               (make-pathname shared-dir "salt")
                                                                               "sml-lib")
                                                                              "rk")))
                                                                        (Tlist (case (ivp-simulation-method)
                                                                                 ((crk3) (list (Tstr (make-pathname csolver-path "crk3.c"))))
                                                                                 ((crkbs) (list (Tstr (make-pathname csolver-path "crkbs.c"))))
                                                                                 ((crkdp) (list (Tstr (make-pathname csolver-path "crkdp.c"))))
                                                                                 (else (list))))))
                                                                                
                                                     ))
                                          ))
                                  ))
                              )

               (exec-path (group-path sim-path mlb-path makefile-path)
                          (run (make -f ,makefile-path)))

               )

          (list exec-path) )
        ))
    ))

           
(define (find-duplicates lis)
  (let recur ((xs lis) (res '()))
    (if (null? xs) res
        (let ((x (car xs)) (xtail (cdr xs)))
          (let-values ([(xlis ylis) (partition (lambda (y) (equal? x y)) lis)])
            (recur xtail (if (> (length xlis) 1) (cons x res) res))
            ))
        ))
  )



(define (main options operands)

  (if (options 'help) (network:usage))

  (if (null? operands) (network:usage))

  (if (options 'verbose) 
      (begin
        (salt:verbose 1)
        (utils-verbose 1)
        (network-verbose 1)))

  (if (options 'trace) 
      (for-each (lambda (name) (salt:add-trace name)) (options 'trace)))
  
  (simulation-platform (or (options 'platform) (defopt 'platform) ))
  (simulation-method (or (options 'method) (defopt 'method) ))
  
  (ivp-simulation-platform (simulation-platform))
  (alsys-simulation-platform (simulation-platform))
  (ivp-simulation-method (simulation-method))

  (salt:model-quantities (cons (cons 'dimensionless Unity) (salt:model-quantities)))
  
  (for-each
   
   (lambda (operand)
     
     (let* (
            (nineml-sxml ((sxpath `(// nml:NineML)) (parse-xml (read-all operand))))
            (model-sxml (sxml:kids nineml-sxml))
            (ul-imports ((sxpath `(// (*or* nml:Import nml:import)))  model-sxml))
            (ul-import-sxmls (map (lambda (x) (parse-xml (fetch (sxml-string->uri (sxml:text x))))) ul-imports))
            (all-sxml (fold append model-sxml ul-import-sxmls))
            )

       ;;(pp `("ULXML" . ,all-sxml) (current-error-port))       

       (let-values (((ul-component-env ul-sxml) (resolve-ul-components all-sxml)))
         
         (let ((dimensions-sxml (sxml:kidsn 'nml:Dimension `(nml:NineML . ,all-sxml)))
               (units-sxml (sxml:kidsn 'nml:Unit `(nml:NineML . ,all-sxml))))

           (eval-sxml-units dimensions-sxml units-sxml))
         
         (d "ul-component-env = ~A~%" ul-component-env)
         (let* ((al-component-env (resolve-al-components all-sxml))

                (ul-properties
                 (parse-ul-properties
                  operand ((sxpath `(// (*or* nml:Property nml:property))) ul-sxml)))
                
                (ul-component-eval-env
                 (map (lambda (x) (eval-ul-component x al-component-env ul-component-env)) 
                      (map cdr ul-component-env)))

                )
           
           
           (let ((names (map car ul-component-eval-env)))
             (let ((dups (find-duplicates names)))
               (if (not (null? dups))
                   (error '9ML-network "Duplicate component names found" dups))
               ))
           
           (eval-ul-group operand ul-properties `(nml:Group . ,ul-sxml) ul-component-eval-env)
           
           ))
       ))
   
   operands))


(main opt (opt '@))


