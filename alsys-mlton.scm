;;
;; NineML algebraic system code generator for MLton.
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


(module 9ML-alsys-mlton

	(alsys-mlton-codegen
         mlton-initial mlton-state-update
         mlton-value
         )

	(import scheme chicken )

(import (only files make-pathname pathname-directory pathname-file)
	(only extras sprintf)
	(only data-structures conc alist-ref intersperse)
	(only srfi-13 string-concatenate)
        (only srfi-1 delete-duplicates))
(require-extension make datatype algebraic-system 9ML-eval setup-api)


(define nl "\n")


(define (mlton-value v)
  (cond
   ((pair? v)
    (case (car v)
      ((realsig)   (mlton-value (caddr v)))
      ((realconst) (mlton-value (cadr v)))
      ((random)    (sprintf "~A ()" (cadr v)))
      ((neg)       (sprintf "Real.~~ (~A)" (mlton-value (cadr v))))
      ((+ - * / >= <= > <) 
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


(define (mlton-printstate dvars ic)
  (string-append
   (sprintf "fun printstate (input) = ~%")
   "("
   (string-concatenate
    (map (lambda (dvar)
	   (let ((v (alist-ref dvar ic)))
	     (let ((show (cond ((number? v) "showReal")
			       ((boolean? v) "showBoolean")
			       (else         "showReal"))))
	       (sprintf "^ \" \" ^ (~A (#~A(input)))" show dvar))))
         dvars))
   ")" ))





(define (alsys-mlton-codegen prefix alsys-id dvars pvars ic sd #!key (random #f))
  (let* ((dir          (or (pathname-directory prefix) "."))
	 (shared-dir   (chicken-home))
	 (solver-path  (make-pathname (pathname-directory prefix) (conc alsys-id "_solver.sml")))
	 (run-path     (make-pathname (pathname-directory prefix) (conc alsys-id "_run.sml")))
	 (mlb-path     (make-pathname (pathname-directory prefix) (conc alsys-id "_run.mlb")))
	 (mlton-path  "mlton")
         )

    (make (
	   (solver-path (prefix)
			(with-output-to-file solver-path (lambda () (codegen/ML alsys-id sd random: random))))
	   
	   (run-path (prefix)
		     (with-output-to-file run-path 
		       (lambda () 
			 (print-fragments
			  `(
			    (,mlton-run-prelude)
			    (,(mlton-printstate dvars ic) ,nl)
			    (,(sprintf "val initial = ~A~%" (mlton-initial ic)))
			    )))))
	   
	   (mlb-path ()
		     (with-output-to-file mlb-path
		       (lambda () 
			 (print-fragments
			  `(("$(SML_LIB)/basis/basis.mlb" ,nl )
			    ("$(RK_LIB)/rk.mlb" ,nl )
			    ("$(RANDMTZIG_LIB)/randmtzig.mlb" ,nl )
			    ("local " ,nl)
			    (,(sprintf "    ~A_solver.sml" alsys-id) ,nl)
			    ("in" ,nl)
			    ("    structure Model" ,nl)
			    ("end" ,nl)
			    ,(sprintf "~A_run.sml" alsys-id) ,nl))
			 )))
	   
	   )
      (list solver-path run-path mlb-path) )
    ))

)
