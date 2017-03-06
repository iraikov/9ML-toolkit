;;
;;  NineML IVP code generator for MLton.
;;
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


(module 9ML-ivp-mlton

	(ivp-mlton ivp-mlton-codegen
         mlton-initial mlton-state-update
         mlton-value
         )

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file)
	(only data-structures conc alist-ref intersperse)
	(only srfi-13 string-concatenate)
        (only srfi-1 delete-duplicates))
(require-extension make datatype setup-api salt)


(define nl "\n")

;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b #!key (out (current-output-port)))
  (let loop ((fragments b) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
       (display (car fragments) out)
       (loop (cdr fragments) #t)))))


(define (mlton-value v)
  (cond
   ((pair? v)
    (case (car v)
      ((real)      (mlton-value (caddr v)))
      ((realsig)   (mlton-value (caddr v)))
      ((realconst) (mlton-value (cadr v)))
      ((generator) (sprintf "~A ()" (cadr v)))
      ((random)    (sprintf "random_~A ()" (cadr v)))
      ((neg)       (sprintf "Real.~A (~A)" "~" (mlton-value (cadr v))))
      ((+ - * / >= > <= <) 
       (sprintf "Real.~A (~A, ~A)"
                (car v) (mlton-value (cadr v)) 
                (mlton-value (caddr v))))
      ((log ln sin cos cosh tanh exp)
       (sprintf "Math.~A (~A)"
                (car v) (mlton-value (cadr v)) ))
      (else (error 'mlton-value "invalid value" v))))
   ((and (number? v) (negative? v)) (string-append "~" (sprintf "~A" (abs v))))
   ((boolean? v)  (if v "true" "false"))
   (else (sprintf "~A" v))))
  
	
(define (mlton-initial ic #!key (update '()))
    (let ((ic (map (lambda (x) 
                     (let ((n (car x)) (v (cdr x))) 
                       (if (assoc n update)
                           (cons n (mlton-value (alist-ref n update)))
                           (cons n (mlton-value v))
                           )
                       ))
                   ic)))
      (string-append "{" (string-concatenate (intersperse (map (lambda (x) (sprintf "~A=(~A)" (car x) (cdr x))) ic) ",")) "}")
      ))


(define (mlton-state-update vars #!key (input "input") (field-input "fieldinput") (nstate "nstate") 
                            (update '()) (states '()) (fields '()))
  (let* (
	 (f (lambda (n) (if (assoc n update)
                            (sprintf "~A=~A" n (alist-ref n update))
                            (sprintf "~A=(#~A(~A))" n n 
                                     (cond ((member n states) nstate)
                                           ((member n fields) field-input)
                                           (else input)))
                            )))
         (nstate1 (string-append "{" (string-concatenate (intersperse (map f vars) ",")) "}"))
         )
    nstate1
    ))


 
(define mlton-run-prelude
#<<EOF

fun putStrLn str = 
    (TextIO.output (TextIO.stdOut, str);
     TextIO.output (TextIO.stdOut, "\n"))
    
fun putStr str = 
    (TextIO.output (TextIO.stdOut, str))
    
fun showBoolean b = (if b then "1" else "0")

fun showReal n = 
    let open StringCvt
	open Real
    in
	(if n < 0.0 then "-" else "") ^ (fmt (FIX (SOME 12)) (abs n))
    end

EOF
)


(define (mlton-printstate ivar dvars ic)
  (string-append
   (sprintf "fun printstate (input) = ~%")
   "("
   (sprintf "(showReal (#~A(input)))"  ivar)
   (string-concatenate
    (map (lambda (dvar)
	   (let ((v (alist-ref dvar ic)))
	     (let ((show (cond ((number? v) "showReal")
			       ((boolean? v) "showBoolean")
			       (else         "showReal"))))
	       (sprintf "^ \" \" ^ (~A (#~A(input)))" show dvar)))) dvars))
   ")" ))



(define (mlton-run-start ivar dvars ic)
  (let ((states (cons ivar dvars)))
    (sprintf
#<<EOF

fun start (tmax,f,initial) =
let
  fun run (input) =
    let val nstate = f input
        val nstate1 = ~A
    in putStrLn (printstate nstate1);
       if (#~A nstate)  > tmax
       then (putStrLn "# All done!"; nstate1)
       else (run nstate1)
    end
in
  run (initial)
end
 
EOF
   (mlton-state-update (map car ic) states: states) ivar)))


(define (ivp-mlton prefix ivp-id ivar dvars pvars start end ic sd solver #!key (random #f))
  (let* ((dir          (or (pathname-directory prefix) "."))
	 (shared-dir   (chicken-home))
	 (salt-dir     (make-pathname shared-dir "salt"))
	 (solver-path  (make-pathname (pathname-directory prefix) (conc ivp-id "_solver.sml")))
	 (run-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.sml")))
	 (mlb-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.mlb")))
	 (exec-path    (make-pathname dir (sprintf "~A_run" ivp-id)))
	 (log-path     (make-pathname dir (sprintf "~A_~A.log" (pathname-file prefix) ivp-id)))
	 (mlton-path  "mlton"))
    
    (make (
	   (solver-path (prefix)
			(with-output-to-file solver-path (lambda () (codegen-ODE/ML ivp-id sd solver: solver random: random))))
	   
	   (run-path (prefix)
		     (with-output-to-file run-path 
		       (lambda () 
			 (print-fragments
			  `(
			    (,mlton-run-prelude)
			    (,(mlton-printstate ivar dvars ic) ,nl)
			    (,(mlton-run-start ivar dvars ic) ,nl)
			    (,(sprintf "val initial = ~A~%" (mlton-initial ic)))
			    ,(sprintf "val _ = (printstate initial; start (~A, Model.~A, initial))~%~%" end ivp-id)
			    )))))
	   
	   (mlb-path ()
		     (with-output-to-file mlb-path
		       (lambda () 
			 (print-fragments
			  `(("$(SML_LIB)/basis/basis.mlb" ,nl )
                            ("$(SML_LIB)/basis/unsafe.mlb" ,nl )
			    ("$(RK_LIB)/rk.mlb" ,nl )
			    ,(case solver ((crkdp) `("$(RK_LIB)/crk.mlb" ,nl )) (else '()))
			    ("$(LASTN_LIB)/lastn-buffer.mlb" ,nl )
			    ("$(RANDMTZIG_LIB)/randmtzig.mlb" ,nl )
			    ("$(INTERP_LIB)/lininterp.mlb" ,nl )
			    ("local " ,nl)
			    (,(sprintf "    ~A_solver.sml" ivp-id) ,nl)
			    ("in" ,nl)
			    ("    structure Model" ,nl)
			    ("end" ,nl)
			    ,(sprintf "~A_run.sml" ivp-id) ,nl))
			 )))
	   
	   (exec-path (solver-path run-path mlb-path)
		      (run (,mlton-path 
                            -default-ann "'allowFFI true'"
                            -link-opt -s 
                            -mlb-path-var ,(string-append "'LASTN_LIB " salt-dir "/sml-lib/lastn-buffer'") 
                            -mlb-path-var ,(string-append "'RK_LIB " salt-dir "/sml-lib/rk'") 
                            -mlb-path-var ,(string-append "'RANDMTZIG_LIB " salt-dir "/sml-lib/randmtzig'") 
                            -mlb-path-var ,(string-append "'INTERP_LIB " salt-dir "/sml-lib/lininterp'") 
                            ,mlb-path
                            ,(string-append salt-dir "/sml-lib/randmtzig/randmtziglib.c")
                            )))
	   
	   (log-path (exec-path)
		     (run (,exec-path > ,log-path)))
	   )
      (list log-path) )
    ))


(define (ivp-mlton-codegen prefix ivp-id ivar dvars pvars ic sd solver #!key (random #f))
  (let* ((dir          (or (pathname-directory prefix) "."))
	 (shared-dir   (chicken-home))
	 (solver-path  (make-pathname (pathname-directory prefix) (conc ivp-id "_solver.sml")))
	 (run-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.sml")))
	 (mlb-path     (make-pathname (pathname-directory prefix) (conc ivp-id "_run.mlb")))
	 (mlton-path  "mlton")
         )
    
    (make (
	   (solver-path (prefix)
			(with-output-to-file solver-path (lambda () (codegen-ODE/ML ivp-id sd solver: solver random: random))))
	   
	   (run-path (prefix)
		     (with-output-to-file run-path 
		       (lambda () 
			 (print-fragments
			  `(
			    (,mlton-run-prelude)
			    (,(mlton-printstate ivar dvars ic) ,nl)
			    (,(mlton-run-start ivar dvars ic) ,nl)
			    (,(sprintf "val initial = ~A~%" (mlton-initial ic)))
			    )))))
	   
	   (mlb-path ()
		     (with-output-to-file mlb-path
		       (lambda () 
			 (print-fragments
			  `(("$(SML_LIB)/basis/basis.mlb" ,nl )
			    ("$(RK_LIB)/rk.mlb" ,nl )
			    ,(case solver ((crkdp) `("$(RK_LIB)/crk.mlb" ,nl )) (else '()))
			    ("$(LASTN_LIB)/lastn-buffer.mlb" ,nl )
			    ("$(RANDMTZIG_LIB)/randmtzig.mlb" ,nl )
			    ("$(INTERP_LIB)/lininterp.mlb" ,nl )
			    ("local " ,nl)
			    (,(sprintf "    ~A_solver.sml" ivp-id) ,nl)
			    ("in" ,nl)
			    ("    structure Model" ,nl)
			    ("end" ,nl)
			    ,(sprintf "~A_run.sml" ivp-id) ,nl))
			 )))
	   
	   )
      (list solver-path run-path mlb-path) )
    ))

)
