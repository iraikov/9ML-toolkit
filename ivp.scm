;;
;;  An IVP solver for NineML.
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

(require-extension datatype matchable static-modules miniML miniMLsyntax miniMLvalue miniMLeval)
(require-extension getopt-long ssax sxml-transforms sxpath sxpath-lolevel object-graph signal-diagram)
(require-extension 9ML-parse 9ML-eval 9ML-plot)
(require-extension 9ML-ivp-lib )
	


(include "SXML.scm")
(include "SXML-to-XML.scm")


(define-values (env-binding? env-empty env-add-signature env-add-module env-add-type env-add-spec env-add-value
	        env-find-value env-find-type env-find-module env-find)
  (make-mod-env core-syntax))

(define-values (scope-typedecl scope-modtype scope-signature scope-modterm scope-moddef)
  (make-mod-scoping core-syntax core-scoping))

(define-values (check-modtype check-signature type-modterm type-moddef type-definition)
  (make-mod-typing core-syntax core-typing))



(include "NineMLcore.scm")
(include "NineMLreal.scm")
(include "NineMLrandom.scm")
(include "NineMLsignal.scm")
(include "NineMLdiagram.scm")
(include "NineMLivp.scm")


(define init-scope      (make-parameter st-empty))
(define init-type-env   (make-parameter env-empty))
(define init-eval-env   (make-parameter env-empty))


(define (enter-typedecl id decl)
  (init-scope (st-enter-type id (init-scope)))
  (init-type-env   (env-add-type id decl (init-type-env))))

(define (enter-valtype name ty)
  (let ((id (ident-create name)))
    (init-scope (st-enter-value id (init-scope)))
    (init-type-env   (env-add-value id ty (init-type-env)))))

(define (enter-val name val)
  (let ((id (or (and (ident? name) name) (ident-create name))))
    (init-eval-env (ident-add id val (init-eval-env)))))

(core-initialize enter-typedecl enter-valtype)
(eval-cbv-initialize enter-val)


(define (enter-module id mty)
  (init-scope (st-enter-module id (init-scope)))
  (init-type-env (env-add-module id mty (init-type-env))))


(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))


(define opt-defaults
  `(
    (platform . chicken)
    (method . rk3)
    ))

(define (defopt x)
  (lookup-def x opt-defaults))

                 
(define opt-grammar
  `(

    (print-type-env  "prints the type environment of each operand"
		     (single-char #\t)
		     (value (optional COMPONENT-LIST)
			    (default all)
			    (transformer 
			     ,(lambda (x) 
				(if (string=? x "all") x
				    (list (string-split x ",")))))))

    (print-eval-env  "prints the evaluation environment of each operand"
		     (single-char #\e)
		     (value (optional COMPONENT-LIST)
			    (default all)
			    (transformer 
			     ,(lambda (x) 
				(if (string=? x "all") x
				    (list (string-split x ",")))))))

    (print-source-defs  "prints the source definitions of each operand"
			(single-char #\s))

    (html-report        "prints out an HTML report of the unified environments of each operand")

    (output-sxml        "sets output format to SXML")

    (output-xml         "sets output format to XML")

    (data            "save data from simulations in files ${OPERAND}_NAME.log"
		     (single-char #\d))

    (plot            "save PNG plots of simulation data in files ${OPERAND}_NAME.png"
		     (single-char #\p))

    (plot-eps        "save EPS plots of simulation data in files ${OPERAND}_NAME.eps")

    (plot-index      "specify index of variable to plot"
		     (value (required INDEX)
			    (predicate  ,(lambda (x) (integer? (string->number x))))
			    (transformer ,string->number)
			     ))

    (xml            "reads canonical NineML XML representation of each operand"
		    (single-char #\x))

    (platform        "simulation platform (one of chicken, mlton, octave, octave/mlton)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((chicken mlton octave octave/mlton) s)
				    (else (error 'ivp "unrecognized platform" x))))))
			    (transformer ,string->symbol)
			     ))

    (method        "integration method (one of rkfe, rk3, rk4a, rk4b, rkhe, rkoz, rkdp)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((rkfe rk3 rk4a rk4b rkoz rkdp) s)
				    (else (error '9ML-ivp "unrecognized method" x))))))
			    (transformer ,string->symbol)
                            ))

    (verbose          "print commands as they are executed"
		      (single-char #\v))


    (help         (single-char #\h))           

    ))


;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))

(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (ivp-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (ivp:usage)
  (print "Usage: " (car (argv)) " file1... [options...] ")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))


(define nl "\n")

(define (parse-xml fpath)
  (with-input-from-file fpath
    (lambda () (cons '*TOP* (ssax:xml->sxml 
                             (current-input-port) 
                             `((nml . ,nineml-xmlns-base) (nml . "CoModL")))))
    ))


(define (interpreter operand #!key (xml #f))

  (let ((defs (if xml (parse-al-sxml (parse-xml operand))
		  (call-with-input-file operand (lambda (in) (parse 'NineML in))))))


    (let* ((scoped-defs      (scope-moddef (init-scope) defs))
	   (mty              (type-moddef (init-type-env) '() scoped-defs))
	   (type-env         (map (lambda (x) (cases modspec x
						     (Value_sig (id vty) (cons id x))
						     (Type_sig (id decl) (cons id x))
						     (Module_sig (id mty) (cons id x))
						     )) mty))
	   (eval-env         (mod-eval-cbv (init-eval-env) scoped-defs))
	   (unified-env      (list scoped-defs 
				   (filter (lambda (x) (not (assoc (car x) (init-type-env)))) type-env) 
				   (filter (lambda (x) (not (assoc (car x) (init-eval-env)))) eval-env) ))

	   )
      unified-env
      )))


(define (main options operands)

  (if (options 'help) (ivp:usage))

  (let ((find-module (lambda (x) (env-find-module x (init-type-env)))))
    (for-each (lambda (init name) (init name enter-module find-module init-eval-env))
	      (list Real:module-initialize   
		    Random:module-initialize   
		    Signal:module-initialize   
		    Diagram:module-initialize  
		    IVP:module-initialize )
	      (list "Real" "Random" "Signal" "Diagram" "IVP" )) )

  (if (null? operands)
      (ivp:usage)
      (let ((output-type (cond ((options 'output-xml)  'xml)
			       ((options 'output-sxml) 'sxml)
			       (else #f)))
	    (unified-envs (map (lambda (x) (interpreter x xml: (options 'xml))) operands)))
	(if (options 'verbose) (begin (eval-verbose 1) (ivp-verbose 1)))

	(ivp-simulation-platform (or (options 'platform) (defopt 'platform) ))
	(ivp-simulation-method (or (options 'method) (defopt 'method) ))

	(for-each
	 (lambda (operand uenv)
	   (let ((source-defs (car uenv))
		 (mty         (cadr uenv))
		 (eval-env    (caddr uenv)))
	     
	     (let ((type-env-opt (options 'print-type-env)))
	       (if type-env-opt
		   (if (and (string? type-env-opt) (string=?  type-env-opt "all"))
		     (print-type-env mty output-type)
		     (let ((fc (lambda (x) (and (member (ident-name (car x)) type-env-opt) x))))
		       (print-type-env mty output-type fc)))
		   ))

	     (let ((eval-env-opt (options 'print-eval-env)))
	       (if eval-env-opt
		   (if (and (string? eval-env-opt) (string=? eval-env-opt "all"))
		     (print-eval-env eval-env)
		     (let ((fc (lambda (x) (and (member (ident-name (car x)) eval-env-opt) x))))
		       (print-eval-env eval-env output-type fc)))
		   ))

	     (if (options 'print-source-defs)
		 (print-source-defs source-defs output-type))

	     (if (options 'html-report)
		 (html-report operand uenv value-hook: (make-ivp-data-hook diagram: #t ivp: #t )))

	     (if (or (options 'data) (options 'plot) (options 'plot-eps))
		 (begin
		   (traverse-definitions operand uenv value-hook: (make-ivp-data-hook ivp: #t))
		   (process-wait) ))

	     (if (options 'plot)
		    (traverse-definitions operand uenv value-hook: (make-ivp-plot-hook ivp: #t plot-index: (options 'plot-index))))

	     (if (options 'plot-eps)
		 (traverse-definitions operand uenv value-hook: (make-ivp-plot-hook ivp: #t plot-format: 'eps plot-index: (options 'plot-index))))

	     ))
	 operands unified-envs))))

(width 40)
(main opt (opt '@))
