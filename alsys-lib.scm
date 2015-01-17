;;
;;  An algebraic system solver for NineML.
;;
;;
;; Copyright 2013-2014 Ivan Raikov
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


(module 9ML-alsys-lib

	(
         alsys-verbose  alsys-simulation-platform 
         make-alsys-cgen-hook
	 )

(import scheme chicken)

(require-extension datatype matchable static-modules miniML miniMLsyntax miniMLvalue miniMLeval)
(require-extension object-graph algebraic-system)
(require-extension 9ML-parse 9ML-eval 9ML-plot)
(require-extension 9ML-alsys-mlton )

(import (only files pathname-file)
        (only utils system*)
        (only extras fprintf)
        (only posix process-fork)
        (only data-structures string-intersperse ->string alist-ref)
        (only srfi-1 lset-intersection lset-difference)
        )

(define lookup-def 
  (lambda (k lst . rest)
    (let-optionals rest ((default #f))
      (alist-ref k lst eq? default))))


(define alsys-simulation-platform (make-parameter #f))
(define alsys-verbose (make-parameter 0))

(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (alsys-verbose)) 
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


(define (construct-alsys prefix name sxml-tuple)

  (let ((sexpr 
	 (let ((sexpr (sxml-value->sexpr sxml-tuple)))
	   (case (car sexpr)
	     ((alsys) sexpr)
	     (else #f)))))

      (and sexpr

           (let* (
                  (alsys+initial (sexpr->alsys+initial sexpr))
                  (ls     (construct (car alsys+initial)))
                  (ic     (cadr alsys+initial))
                  (fields (caddr alsys+initial))
                  (params (cadddr alsys+initial))
                  )

             
             (let* (
                    (dfe   (dataflow ls '()))
                    (dvars (alist-ref 'out dfe))
                    (pvars (map car params))
                    )

               (d "construct-alsys: dvars = ~A~%" dvars)
               
               `(
                 (algebraic-system   . ,ls)
                 (initial-conditions . ,ic)
                 (params . ,params)
                 (dfe    . ,dfe)
                 (dvars  . ,dvars)
                 (pvars  . ,pvars)
                 (fields . ,fields)
                 )
               ))
           ))
  )
                   
                 

(define (generate-alsys-code prefix alsys-id sxml-tuple #!key (platform 'mlton) (random #f))

  (let ((lsinfo (construct-alsys prefix alsys-id sxml-tuple)))

    (d "generate-alsys-code: lsinfo = ~A~%" lsinfo)

    (let (
          (ls      (lookup-def 'algebraic-system lsinfo))
          (ic      (lookup-def 'initial-conditions lsinfo))
          (dvars   (lookup-def 'dvars lsinfo))
          (pvars   (lookup-def 'pvars lsinfo))
          )

      (d "generate-alsys-code: ic = ~A~%" ic)
      
      (case platform
        
        ((mlton)
         (begin
           (alsys-mlton-codegen prefix alsys-id dvars pvars ic ls random: random)
           `((sys-id . ,alsys-id) . ,lsinfo)))
        
        (else (error 'generate-alsys-code "unknown platform"  platform))
        
        ))
    ))
    
                 


(define (make-alsys-cgen-hook alsys-env)

  (lambda (prefix name label value)

    (cond

     ((or (and (string? label) (string=? label "alsys"))
          (and (pair? label) (string=? (car label) "alsys"))) ;; value is an algebraic system

      (let ((alsys-info (generate-alsys-code prefix name value
                                             platform: (alsys-simulation-platform) random: #t)))
        (alsys-env (cons `(,(string->symbol name) . ,alsys-info) (alsys-env)))
        alsys-info
	))
     
     (else #f)
     ))
  )
	    


)
