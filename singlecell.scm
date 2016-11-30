;;
;; NineML single cell descriptions.
;;
;;
;; Copyright 2015-2016 Ivan Raikov
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


(require-extension extras posix utils files data-structures tcp srfi-1 srfi-13 irregex)
(require-extension datatype matchable make ssax sxml-transforms sxpath sxpath-lolevel 
                   object-graph ersatz-lib unitconv uri-generic getopt-long )
(require-extension 9ML-types 9ML-parse 9ML-utils 9ML-ivp-mlton)

(require-library ersatz-lib salt)
(import (prefix ersatz-lib ersatz: )
        (prefix salt salt: ))


(define (string-match rx str)
  (and-let* ((m (irregex-match rx str)))
    (let loop ((i (irregex-match-num-submatches m))
               (res '()))
      (if (fx<= i 0)
          (cons str res)
          (loop (fx- i 1) (cons (irregex-match-substring m i) res))))))


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
    (method . rkdp)
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
				    (else (error '9ML-singlecell "unrecognized platform" x))))))
			    (transformer ,string->symbol)
                            ))

    (method        "integration method (one of rkfe, rk3, rk4a, rk4b, rkhe, rkbs, rkf45, rkv65, rkf78, rkoz3, rkdp, crk3, crk4a, crk4b, crkdp, crkbs)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((rkfe rk3 rk4a rk4b rkhe rkbs rkf45 rkck rkoz3 rkdp rkf45 rkf78 rkv65 crk3 crk4a crk4b crkdp crkbs ) s)
				    (else (error '9ML-singlecell "unrecognized method" x))))))
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
(define (singlecell:usage)
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

(define singlecell-verbose (make-parameter 0))
(define data-dir (make-parameter #f))
(define simulation-platform (make-parameter #f))
(define simulation-method (make-parameter 'rk3))


(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (singlecell-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))


(define (sxml-string->uri s) 
  (let ((ss (string-trim-both s)))
    (uri-reference ss)))


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
	     (pn (make-pathname dir (string-append "9ML-singlecell-" (number->string n 16)) "tmp")))
	(cond ((file-exists? pn) (loop))
	      (else (mkdir pn) pn))))))




(define (codegen-ivp-ul-component/mlton operand ul-properties node)
  
  (let* (
         (shared-dir    (chicken-home))
         (template-dir  (make-pathname (make-pathname shared-dir "9ML") "templates"))
         (sim-tmpl      (case (ivp-simulation-method)
                          ((rkhe rkbs rkf45 rkck rkoz3 rkdp rkf45 rkf78 rkv65 crkdp crkbs) "Sim.sml.single.adaptive.tmpl")
                          (else "Sim.sml.single.tmpl")))
         (mlb-tmpl      (case (ivp-simulation-method)
                          ((rkhe rkbs rkf45 rkck rkoz3 rkdp rkf45 rkf78 rkv65 crkbs crkdp) "Sim.mlb.single.adaptive.tmpl")
                          (else "Sim.mlb.single.tmpl")))
         (makefile-tmpl "Makefile.single.tmpl")
         (source-dir    (pathname-directory operand))

         )

    (match-let
     (
      ((node-name . ($ dynamics-node model-name model-formals model-env model-eqset)) node)
      )
     (d "node name = ~A model-eqset = ~A~%" node-name model-eqset)
     (let* (
            (sim-path      (make-pathname source-dir (conc "Sim_" node-name "." 
                                                           (ivp-simulation-method) ".sml")))
            (mlb-path      (make-pathname source-dir (conc "Sim_" node-name ".mlb")))
            (exec-path     (make-pathname source-dir (conc "Sim_" node-name)))
            (makefile-path (make-pathname source-dir (conc "Makefile." node-name "." 
                                                           (ivp-simulation-method))))
            (node-path     (make-pathname source-dir (sprintf "~A.~A.sml" node-name
                                                              (ivp-simulation-method))))
            (node-c-path   (make-pathname source-dir (sprintf "~A.c" node-name)))

            (prototype-decls
             (salt:make-astdecls
              (salt:astdecls-decls model-eqset)))
            )
       (d "prototype-decls = ~A~%" prototype-decls)
       (let* ((sim (salt:simcreate (salt:elaborate prototype-decls))))
         (d "node name = ~A model-sim = ~A~%" node-name sim)
         (let ((sml-port (open-output-file node-path)))
           (salt:codegen-ODE/ML node-name sim out: sml-port solver: (ivp-simulation-method) libs: '(random))
           (close-output-port sml-port)
           (case (ivp-simulation-method) 
             ((crk3 crk4a crk4b crkbs crkdp)
              (let ((c-port (open-output-file node-c-path)))
                (salt:codegen-ODE/C node-name sim out: c-port solver: (ivp-simulation-method) libs: '(random))
                (close-output-port c-port)
                ))
             (else (begin)))
           ))
       
       (make (
            
            (sim-path (operand)
                      (with-output-to-file sim-path 
                        (lambda ()
                          (print (ersatz:from-file 
                                  sim-tmpl
                                  env: (template-std-env search-path: `(,template-dir))
                                  models: `(
                                            (modelName . ,(Tstr (->string node-name)))
                                            ))
                                 ))
                        ))
            
            (mlb-path (operand)
                      (with-output-to-file mlb-path 
                        (lambda ()
                          (print (ersatz:from-file 
                                  mlb-tmpl
                                  env: (template-std-env search-path: `(,template-dir))
                                  models: `(
                                            (modelName . ,(Tstr (->string node-name)))
                                            (solverMethod . ,(Tstr (->string (ivp-simulation-method))))
                                            (UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                    ((crk3 crk4a crk4b crkdp crkbs) #t)
                                                                    (else #f))))
                                            ))
                                 ))
                        ))
            
            (makefile-path ()
                           (with-output-to-file makefile-path 
                             (lambda ()
                               (print (ersatz:from-file 
                                       makefile-tmpl
                                       env: (template-std-env search-path: `(,template-dir))
                                       models: `((modelName . ,(Tstr (->string node-name)))
                                                 (solverMethod . ,(Tstr (->string (ivp-simulation-method))))
                                                 (sml_lib_home . ,(Tstr (make-pathname 
                                                                         (make-pathname shared-dir "salt")
                                                                         "sml-lib")))
                                                 (nineml_lib_home . ,(Tstr (make-pathname 
                                                                            (make-pathname shared-dir "9ML")
                                                                            "sml-lib")))
                                                 (UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                         ((crk3 crk4a crk4b crkbs crkdp) #t)
                                                                         (else #f))))
                                                 (CSolverFiles . ,(let ((csolver-path 
                                                                         (make-pathname
                                                                          (make-pathname 
                                                                           (make-pathname shared-dir "salt")
                                                                           "sml-lib")
                                                                          "rk")))
                                                                    (Tlist (case (ivp-simulation-method)
                                                                             ((crk3) (list (Tstr (make-pathname csolver-path "crk3.c"))))
                                                                             ((crk4a) (list (Tstr (make-pathname csolver-path "crk4a.c"))))
                                                                             ((crk4b) (list (Tstr (make-pathname csolver-path "crk4b.c"))))
                                                                             ((crkbs) (list (Tstr (make-pathname csolver-path "crkbs.c"))))
                                                                             ((crkdp) (list (Tstr (make-pathname csolver-path "crkdp.c"))))
                                                                             (else (list))))))
                                                 )
                                       ))
                               ))
                           )

            (exec-path (operand sim-path mlb-path makefile-path)
                       (run (make -f ,makefile-path)))
            
            )

          (list exec-path) )
        ))
    ))




(define (main options operands)

  (if (options 'help) (singlecell:usage))

  (if (null? operands) (singlecell:usage))

  (if (options 'verbose) 
      (begin
        (salt:verbose 1)
        (singlecell-verbose 1)))

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
            (content-sxml (parse-xml (read-all operand)))
            (nineml-sxml ((sxpath `(// nml:NineML)) content-sxml)) 
            (model-sxml (sxml:kids nineml-sxml))
            (ul-imports ((sxpath `(// (*or* nml:Import nml:import)))  model-sxml))
            (ul-import-sxmls (map (lambda (x) (parse-xml (fetch (sxml-string->uri (sxml:text x))))) ul-imports))
            (all-sxml (fold append model-sxml ul-import-sxmls))
            )
       
       (let-values (((ul-component-env ul-sxml) (resolve-ul-components all-sxml)))
         
         (let ((dimensions-sxml (sxml:kidsn 'nml:Dimension `(nml:NineML . ,all-sxml)))
               (units-sxml (sxml:kidsn 'nml:Unit `(nml:NineML . ,all-sxml))))
           (eval-sxml-units dimensions-sxml units-sxml))
         
         (let* (
                (ul-properties
                 (parse-ul-properties
                  operand ((sxpath `(// (*or* nml:Property nml:property)))  ul-sxml)))
                
                (dd (d "ul-properties = ~A~%" ul-properties))
                
                (al-component-env (resolve-al-components all-sxml))
                (dd (d "al-component-env = ~A~%" al-component-env))

                (ul-component-eval-env
                 (map (lambda (x) (eval-ul-component x al-component-env ul-component-env)) 
                      (map cdr ul-component-env)))


                
                )
           
           (for-each (lambda (x) (codegen-ivp-ul-component/mlton operand ul-properties x)) ul-component-eval-env)
           
           ))
       ))
   
   operands))


(main opt (opt '@))


