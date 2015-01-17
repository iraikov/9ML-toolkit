;;
;; NineML IVP code generator for Chicken Scheme.
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


(module 9ML-ivp-chicken

	(ivp-chicken ivp-chicken/cvode ivp-chicken-codegen ivp-chicken-codegen/cvode)

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file)
	(only data-structures conc))
(require-extension make datatype signal-diagram 9ML-eval setup-api)


(define nl "\n")
	
(define (chicken-value v)
  (cond
   ((pair? v)
    (case (car v)
      ((realsig)   (chicken-value (caddr v)))
      ((realconst) (chicken-value (cadr v)))
      ((random)    (sprintf "~A ()" (cadr v)))
      ((neg)       (sprintf "(- (~A))" (chicken-value (cadr v))))
      ((+ - * / >= <= > <) 
       (sprintf "(~A ~A ~A)"
                (car v) 
                (chicken-value (cadr v)) 
                (chicken-value (caddr v))))
      ((log ln sin cos cosh tanh exp)
       (sprintf "(~A ~A)"
                (car v) (chicken-value (cadr v)) ))
      (else (error 'chicken-value "invalid value" v))))
   ((boolean? v)  (if v "#t" "#f"))
   (else (sprintf "~A" v))))

(define chicken-run
#<<EOF

(define-syntax run
  (syntax-rules ()
    ((_ f indep dep events end input parameters)
     (let ((nstate input))
       (printf "# ~A " indep)
       (for-each (lambda (x) (printf "~A " x)) dep)
       (printf "~%")
       (let recur ((nstate nstate))
	 (let ((ival (alist-ref indep nstate)))
	   (printf "~A " ival)
	   (for-each (lambda (x)
		       (let ((v (alist-ref x nstate)))
			 (printf "~A " (if (boolean? v) (or (and v 1) 0) v))))
		     dep)
	   (printf "~%")
	   (if (> ival end)
	       (print "# All done!")
	       (recur (append (f nstate) parameters))))))
     )))
EOF
)


(define chicken-run/cvode
#<<EOF


(define-syntax run
  (syntax-rules ()
    ((_ f h indep dep events end input parameters)
     (let* ((nstate (make-parameter input))

            (numstates (filter (lambda (y) (number? (alist-ref y input))) dep))
            
            (update-nstate (lambda (old new)
                             (old (fold (lambda (kv ax) (alist-update (car kv) (cdr kv) ax)) (old) new))))

            (vector->nstate (lambda (yvec) 
                              (fold (lambda (y yv nstate) (cons (cons y yv) nstate))
                                    '()
                                    numstates (f64vector->list yvec))))
            (nstate->vector (lambda (nstate) 
                              (list->f64vector (map (lambda (y) (alist-ref y nstate)) numstates))))

            (fwrap (lambda (t yvec nstate)
                     (let ((nstate1 (cons (cons indep t) (vector->nstate yvec))))
                       (nstate nstate1)
                       (let ((nstate2 (f (append nstate1 parameters))))
                         (nstate->vector nstate2)
                         ))
                     ))

            (fcall (lambda (t yvec nstate)
                     (let ((nstate1 (cons (cons indep t) (vector->nstate yvec))))
                       (nstate nstate1)
                       (let ((nstate2 (f (append nstate1 parameters))))
                         nstate2
                         ))
                     ))

            (event-detect (lambda (t yy nstate)
                            (let ((Vpeak (alist-ref 'Vpeak parameters))
                                  (V (f64vector-ref yy 2)))
                              (s32vector (floor (- V Vpeak))))))


            (solver (cvode-create-solver
                     (alist-ref indep input) 
                     (nstate->vector input)
                     fwrap
                     abstol: 1e-4
                     reltol: 1e-4
                     events: (make-s32vector (length events) 0)
                     event-fn: event-detect
                     user-data: nstate
                     ))

            (solvewrap (lambda (solver tnext)
                         (let ((status (cvode-solve solver tnext)))
                           (if (negative? status) (error "CVODE solver error" status))
                           (if (zero? status)
                               (cons (cons indep tnext) (vector->nstate (cvode-yy solver)))
                               (let ((yy (cvode-yy solver)) (tt (cvode-t solver)))
                                 (nstate (f (append (fcall tt yy nstate) parameters)))
                                 (cvode-reinit-solver solver tnext (nstate->vector (nstate)))
                                 (cons (cons indep tt) (vector->nstate (cvode-yy solver)))
                                 )))
                           ))
            )
       (printf "# ~A " indep)
       (for-each (lambda (x) (printf "~A " x)) dep)
       (printf "~%")
       (let recur ((hv (alist-ref h parameters)))
	 (let ((ival (alist-ref indep (nstate))))
	   (printf "~A " ival)
	   (for-each (lambda (x)
		       (let ((v (alist-ref x (nstate))))
			 (printf "~A " (if (boolean? v) (or (and v 1) 0) v))))
		     dep)
	   (printf "~%")
	   (if (> ival end)
	       (print "# All done!")
	       (let ((nstate1 (solvewrap solver (+ hv ival))))
                 (recur hv)))
           ))
       ))
    ))

EOF
)

(define (ivp-chicken prefix ivp-id ivar dvars pvars events start end ic sd
                     #!key (solver 'rk3))

  (let* ((dir (or (pathname-directory prefix) "."))
	 (solver-path (make-pathname dir (conc ivp-id "_solver.scm")))
	 (run-path    (make-pathname dir (sprintf "~A_run.scm" ivp-id)))
	 (exec-path   (make-pathname dir (sprintf "~A_run" ivp-id)))
	 (log-path    (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (csc-path    (make-pathname (program-path) "csc")))
    
    (make 
	(
	 (solver-path (prefix)
		      (with-output-to-file solver-path
			(lambda () (codegen/scheme ivp-id sd solver: solver))))
	 
	 (run-path (prefix)
		   (with-output-to-file run-path
		     (lambda () 
		       (print-fragments
			(list
			 (sprintf "(include \"~A_solver.scm\")~%~%" ivp-id)
			 chicken-run nl
			 (sprintf "(define initial (quasiquote ~A))~%~%" 
                                  (cons (cons ivar start) 
                                        (map (lambda (x)
                                               (let ((n (car x))
                                                     (v (chicken-value (cdr x))))
                                                 (cons n (sprintf "(unquote ~A)" v))))
                                             ic)))
			 (sprintf "(define parameters (quote ~A))~%~%" 
                                  (map (lambda (x) 
                                         (let ((nv (assoc x ic)))
                                           (let ((n (car nv))
                                                 (v (chicken-value (cdr nv))))
                                             (cons n v))))
                                       pvars))
			 (sprintf "(run ~A (quote ~A) (quote ~A) (quote ~A) ~A initial parameters)~%~%" 
                                  ivp-id ivar dvars events end)
			 ))
                       ))
                   )
	 
	 (exec-path (run-path solver-path)
		    (run (,csc-path -w -I ,dir -b -S -d0 -O3 -disable-interrupts ,run-path)))
	 
	 (log-path (exec-path) (run (,exec-path > ,log-path)))
	 )
      
      (list log-path) )
    ))


(define (ivp-chicken/cvode prefix ivp-id hvar ivar dvars pvars events start end ic sd)

  (let* ((dir (or (pathname-directory prefix) "."))
	 (solver-path (make-pathname dir (conc ivp-id "_solver.scm")))
	 (run-path    (make-pathname dir (sprintf "~A_run.scm" ivp-id)))
	 (exec-path   (make-pathname dir (sprintf "~A_run" ivp-id)))
	 (log-path    (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (csc-path    (make-pathname (program-path) "csc")))
    
    (make 
	(
	 (solver-path (prefix)
		      (with-output-to-file solver-path
			(lambda () (codegen/scheme ivp-id sd solver: 'cvode))))
	 
	 (run-path (prefix)
		   (with-output-to-file run-path
		     (lambda () 
		       (print-fragments
			(list
			 (sprintf "(include \"~A_solver.scm\")~%~%" ivp-id)
			 chicken-run/cvode nl
			 (sprintf "(define initial (quote ~A))~%~%" 
                                  (cons (cons ivar start) 
                                        (map (lambda (x)
                                               (let ((n (car x))
                                                     (v (chicken-value (cdr x))))
                                                 (cons n v)))
                                             ic)))
			 (sprintf "(define parameters (quote ~A))~%~%" 
                                  (map (lambda (x) (assoc x ic)) pvars))
			 (sprintf "(run ~A (quote ~A) (quote ~A) (quote ~A) (quote ~A) ~A initial parameters)~%~%" 
                                  ivp-id hvar ivar dvars events end)
			 )))))
	 
	 (exec-path (run-path solver-path)
		    (run (,csc-path -w -I ,dir -b -S -d0 -O3 -disable-interrupts ,run-path)))
	 
	 (log-path (exec-path) (run (,exec-path > ,log-path)))
	 )
      
      (list log-path) )
    ))


(define (ivp-chicken-codegen prefix ivp-id ivar dvars pvars events ic sd
                     #!key (solver 'rk3))

  (let* ((dir (or (pathname-directory prefix) "."))
	 (solver-path (make-pathname dir (conc ivp-id "_solver.scm")))
	 (run-path    (make-pathname dir (sprintf "~A_run.scm" ivp-id)))
         )
    
    (make 
	(
	 (solver-path (prefix)
		      (with-output-to-file solver-path
			(lambda () (codegen/scheme ivp-id sd solver: solver))))
	 
	 (run-path (prefix)
		   (with-output-to-file run-path
		     (lambda () 
		       (print-fragments
			(list
			 (sprintf "(include \"~A_solver.scm\")~%~%" ivp-id)
			 chicken-run nl
                         (sprintf "(define initial (quote ~A))~%~%" 
                                  (cons (cons ivar 0.0) 
                                        (map (lambda (x)
                                               (let ((n (car x))
                                                     (v (chicken-value (cdr x))))
                                                 (cons n v)))
                                             ic)))
			 (sprintf "(define parameters (quote ~A))~%~%" 
                                  (map (lambda (x) (assoc x ic)) pvars))
			 ))
                       ))
                   )
	 
	 )
      
      (list solver-path run-path) )
    ))


(define (ivp-chicken-codegen/cvode prefix ivp-id hvar ivar dvars pvars events ic sd)

  (let* ((dir (or (pathname-directory prefix) "."))
	 (solver-path (make-pathname dir (conc ivp-id "_solver.scm")))
	 (run-path    (make-pathname dir (sprintf "~A_run.scm" ivp-id)))
         )

    (make 
	(
	 (solver-path (prefix)
		      (with-output-to-file solver-path
			(lambda () (codegen/scheme ivp-id sd solver: 'cvode))))
	 
	 (run-path (prefix)
		   (with-output-to-file run-path
		     (lambda () 
		       (print-fragments
			(list
			 (sprintf "(include \"~A_solver.scm\")~%~%" ivp-id)
			 chicken-run/cvode nl
			 (sprintf "(define initial (quote ~A))~%~%" 
                                  (cons (cons ivar 0.0) 
                                        (map (lambda (x)
                                               (let ((n (car x))
                                                     (v (chicken-value (cdr x))))
                                                 (cons n v)))
                                             ic)))
			 (sprintf "(define parameters (quote ~A))~%~%" 
                                  (map (lambda (x) (assoc x ic)) pvars))
			 )))))
	 )
      
      (list solver-path run-path) )
    ))


)
