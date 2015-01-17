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


(module 9ML-ivp-lib

	(
         ivp-verbose  ivp-simulation-platform ivp-simulation-method
         make-ivp-cgen-hook make-ivp-data-hook make-ivp-plot-hook
	 )

(import scheme chicken)

(require-extension datatype matchable static-modules miniML miniMLsyntax miniMLvalue miniMLeval)
(require-extension object-graph signal-diagram)
(require-extension 9ML-parse 9ML-eval 9ML-plot)
(require-extension 9ML-ivp-octave 9ML-ivp-chicken 9ML-ivp-mlton 9ML-ivp-octave-mlton )

(import (only files pathname-file)
        (only utils system*)
        (only extras fprintf pp)
        (only posix process-fork)
        (only data-structures string-intersperse ->string alist-ref)
        (only srfi-1 lset-intersection lset-difference filter)
        )

(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))


(define ivp-simulation-platform (make-parameter #f))
(define ivp-simulation-method (make-parameter 'rk3))
(define ivp-verbose (make-parameter 0))

(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (ivp-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))


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


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.

(define nl "\n")


(define (construct-ivp prefix name sxml-tuple htype)

  (let ((sexpr 
	 (let ((sexpr (sxml-value->sexpr sxml-tuple)))
	   (case (car sexpr)
	     ((ivp)
	      (and (pair? (cdr sexpr))
		   (case (cadr sexpr)
		     ((initial)  (cddr sexpr))
		     (else #f))))
	     (else #f)))))

      (and sexpr
	   (match-let (((ivar hvar) (cdr sexpr)))

             (let* (
                    (diagram+initial (sexpr->diagram+initial `(,htype ,hvar) (car sexpr) ))
                    (sd (construct (car diagram+initial)))
                    (ic (cadr diagram+initial))
                    (fields (caddr diagram+initial))
                    (params (cadddr diagram+initial))
                    )


	       (let* ((dfe (dataflow sd '()))
		      (dvars (lset-difference eq?
					      (lset-intersection eq? (alist-ref 'in dfe) (alist-ref 'out dfe))
					      (list ivar)))
                      (pvars (map car params))
		      (events (reverse (events sd)))
		      (inputs (lset-difference eq? (map car ic) (cons hvar (cons ivar dvars))))
                      (outputs (alist-ref 'out dfe))
                      (ic (filter (lambda (x) (member (car x) (alist-ref 'in dfe))) ic))
		      )

                 (if (not (member ivar outputs))
                     (error 'construct-ivp "IVP independent variable is not present as an output in given system" name ivar))

                 `(
                   (signal-diagram . ,sd)
                   (initial-conditions . ,ic)
                   (params . ,params)
                   (dfe    . ,dfe)
                   (ivar   . ,ivar)
                   (hvar   . ,hvar)
                   (dvars  . ,dvars)
                   (pvars  . ,pvars)
                   (events . ,events)
                   (inputs . ,inputs)
                   (outputs . ,outputs)
                   (fields . ,fields)
                   )
                 ))
             ))
      ))
                   
                 

(define (generate-ivp-code prefix ivp-id sxml-tuple
                           #!key (platform 'chicken) (method 'rk3) (random #f))

  (let* ((htype (case method 
                  ((rkfe rk3 rk4a rk4b) 'fixed)
                  ((rkoz rkdp) 'variable)))
         (sdinfo (construct-ivp prefix ivp-id sxml-tuple htype)))

    (d "generate-ivp-code: sdinfo = ~A~%" sdinfo)


    (if (not sdinfo)

        (error 'generate-ivp-code "invalid network node")
        
        (let (
              (sd      (lookup-def 'signal-diagram sdinfo))
              (ic      (lookup-def 'initial-conditions sdinfo))
              (ivar    (lookup-def 'ivar sdinfo))
              (hvar    (lookup-def 'hvar sdinfo))
              (dvars   (lookup-def 'dvars sdinfo))
              (pvars   (lookup-def 'pvars sdinfo))
              (events  (lookup-def 'events sdinfo))
              )
          
          (d "generate-ivp-code: ic = ~A~%" ic)
          
          (case platform
            
            ((octave)
             (begin
               (ivp-octave-codegen prefix ivp-id hvar ivar dvars pvars events ic sd)
               `((ivp-id . ,ivp-id) . ,sdinfo)))
            
            ((octave/mlton octave-mlton)
             (begin
               (ivp-octave-mlton-codegen prefix ivp-id hvar ivar dvars pvars ic sd)
               `((ivp-id . ,ivp-id) . ,sdinfo)))
            
            ((mlton)
             (begin
               (ivp-mlton-codegen prefix ivp-id ivar dvars pvars ic sd method random: random)
               `((ivp-id . ,ivp-id) . ,sdinfo)))
            
            ((chicken)
             (begin
               (ivp-chicken-codegen prefix ivp-id ivar dvars pvars events ic sd method)
               `((ivp-id . ,ivp-id) . ,sdinfo)))
            
            (else (error 'generate-ivp-code "unknown platform"  platform))
            
            ))
        ))
  
  )                

(define (generate-ivp-table prefix ivp-id sxml-tuple #!key (platform 'chicken) (method 'rk3) (random #f))

               
  (let ((htype (case method 
                 ((rkfe rk3 rk4a rk4b) 'fixed)
                 ((rkoz rkdp) 'variable)))
        (sexpr 
	 (let ((sexpr (sxml-value->sexpr sxml-tuple)))
	   (case (car sexpr)
	     ((ivp)
	      (and (pair? (cdr sexpr))
		   (case (cadr sexpr)
		     ((run)  (cddr sexpr))
		     (else #f))))
	     (else #f)))))

    (d "generate-ivp-table: sexpr = ~A~%" (sxml-value->sexpr sxml-tuple))

      (and sexpr
	   (match-let (((ivar hvar start end) (cdr sexpr)))

             (let* ((diagram+initial (sexpr->diagram+initial `(,htype ,hvar) (car sexpr)))
                    (sd (construct (car diagram+initial)))
                    (ic (cadr diagram+initial)))

               (d "generate-ivp-table: ic = ~A~%" ic)

	       (if (not (alist-ref ivar ic))
		   (error 'generate-ivp-table "IVP independent variable is not present in given system" ivar))

	       (let* ((dfe (dataflow sd '()))
		      (dvars (lset-difference eq?
					      (lset-intersection eq? (alist-ref 'in dfe) (alist-ref 'out dfe))
					      (list ivar)))
		      (pvars (lset-difference eq? (alist-ref 'in dfe) (cons ivar dvars)))
		      (events (events sd))
                      (ic (filter (lambda (x) (member (car x) (alist-ref 'in dfe))) ic))
		      )


		 (case platform
		   
		   ((octave)
		    (process-fork
		     (lambda () (ivp-octave prefix ivp-id hvar ivar dvars pvars events start end ic sd)))
		    (list ivp-id ivar dvars) )

		   ((octave/mlton octave-mlton)
		    (process-fork
		     (lambda () (ivp-octave-mlton prefix ivp-id hvar ivar dvars pvars start end ic sd)))
		    (list ivp-id ivar dvars) )
		   
		   ((mlton)
		    (process-fork
		     (lambda () (ivp-mlton  prefix ivp-id ivar dvars pvars start end ic sd method random: random)))
		    (list ivp-id ivar dvars) )

		   ((chicken)
		    (process-fork
		     (lambda () (ivp-chicken prefix ivp-id ivar dvars pvars events start end ic sd)))
		    (list ivp-id ivar dvars) )

		   (else (error 'generate-ivp-table "unknown platform"  platform))
		   
		   )))
	     )))
  )



(define (make-ivp-cgen-hook ivp-env)
  (lambda (prefix name label value)

    (cond

     ((or (and (string? label) (string=? label "ivp"))
          (and (pair? label) (string=? (car label) "ivp"))) ;; value is an IVP

      (d "make-ivp-cgen-hook: value = ~A~%" value)

      (let ((ivp-info (generate-ivp-code prefix name value 
                                         method: (ivp-simulation-method) 
                                         platform: (ivp-simulation-platform)
                                         random: #t)))
        (ivp-env (cons `(,(string->symbol name) . ,ivp-info) (ivp-env)))
        ivp-info
	))
     
     (else #f)
     ))
  )
	    

(define (make-ivp-data-hook #!key (ivp #f) (diagram #f))
  (lambda (prefix name label value)

    (cond
     ((and diagram
	   (or (and (string? label) (string=? label "diagram")) 
	       (and (pair? label) (string=? (car label) "diagram")))) ;; value is a diagram
      (let* ((diagram-id (gensym 'diagram))
	     (diagram-link `(img (@ (src ,(sprintf "~A.png" diagram-id))) (alt "NineML diagram"))))
	(plot-diagram prefix diagram-id value)
	diagram-link
	))
     
     ((and ivp (or (and (string? label) (string=? label "ivp"))
		   (and (pair? label) (string=? (car label) "ivp")))) ;; value is an IVP
      (let ((ivp-id (gensym (string->symbol (string-append (->string name) "ivp")))))
	(let ((ivp-info (generate-ivp-table prefix ivp-id value 
                                            method: (ivp-simulation-method)
                                            platform: (ivp-simulation-platform))))
	  ivp-info
	))
      )
     
     (else #f))))
	    

(define (make-ivp-plot-hook #!key (ivp #f) (plot-format 'png) (plot-index #f))
  (lambda (prefix name label value)
    (cond

     ((and ivp (or (and (string? label) (string=? label "ivp"))
		   (and (pair? label) (string=? (car label) "ivp")))) ;; value is an IVP
      (let* ((ivp-id (gensym 'ivp))
	     (ivp-plot-link `(img (@ (src ,(sprintf "~A_~A.png" (pathname-file prefix) ivp-id)) (alt "NineML IVP plot")))))

	(let ((ivp-info (generate-ivp-table prefix ivp-id value 
                                            platform: (ivp-simulation-platform)
                                            method: (ivp-simulation-method))))
	  (plot-ivp prefix ivp-info format: plot-format index: plot-index)
	  ivp-plot-link)
	))
     
     (else #f))))
	    


)
