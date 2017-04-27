;;
;;  NineML code generator for MLton.
;;
;;
;; Copyright 2010-2017 Ivan Raikov
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


(module 9ML-codegen-mlton

	(
         mlton-initial 
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


)
