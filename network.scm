;;
;; NineML network level descriptions.
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


#|

TODO: use term rewriting rules for eval-ul-component, e.g.:

     ( (M component (definition $url) $properties) =>
       (M component (eval-env (M eval-definition $url)) $properties) )

     ( (M component (eval-env $eval-env) $properties) =>
       (M component (model-module (eval-env-last-entry $eval-env)) $properties) )

     ( (M component (eval-env $eval-env) $properties) =>
       (M component (model-module (eval-env-last-entry $eval-env)) $properties) )

     ( (M component (model-module $model-module) $properties) =>
       (eval-term (M apply-terms (Longid (Pdot (entry-name $model-module) "construct")) $properties)) )

     ( (M eval-definition $url ) =>
       (eval-source (fetch (uri-reference $url)) current-scope current-type-env current-eval-env ) )

     ( (M apply-terms $operator (seq $term $rest)) =>
       (M apply-terms (Apply $operator $term) $rest) )
       
     ( (M apply-terms $operator (seq-empty)) => $operator )
|#


(require-extension extras posix utils files data-structures tcp srfi-1 srfi-13 irregex)
(require-extension datatype matchable static-modules miniML miniMLsyntax miniMLvalue miniMLeval)
(require-extension make signal-diagram ssax sxml-transforms sxpath sxpath-lolevel object-graph ersatz-lib uri-generic getopt-long )
(require-extension 9ML-parse 9ML-eval 9ML-ivp-lib 9ML-alsys-lib 9ML-ivp-mlton)

(require-library ersatz-lib)
(import (prefix ersatz-lib ersatz: ))


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


(define (safe-car x) (and (pair? x) (car x)))

(define $ string->symbol)
(define (s+ . rest) (string-concatenate (map ->string rest)))

(define (alist->tenv xs)
  (map (lambda (x) (cons (car x) (ersatz:sexpr->tvalue (cdr x)))) xs))

(define (warn port message . specialising-msgs)
  (print-error-message message (current-output-port) "Warning")
  (print (string-concatenate (map ->string specialising-msgs))))

	
(include "SXML.scm")
(include "SXML-to-XML.scm")
(include "stx-engine.scm")

(define-values (env-binding? env-empty env-add-signature env-add-module env-add-type env-add-spec env-add-value
	        env-find-value env-find-type env-find-module env-find)
  (make-mod-env core-syntax))

(define-values (scope-typedecl scope-modtype scope-signature scope-modterm scope-moddef)
  (make-mod-scoping core-syntax core-scoping))

(define-values (check-modtype check-signature type-modterm type-moddef type-definition)
  (make-mod-typing core-syntax core-typing))


(define-values (type-variables reset-type-variables
                               find-type-variable instance typerepr
                               begin-def end-def newvar generalize
                               make-deftype make-valtype make-kind
                               binop ternop path-star path-list path-arrow
                               star-type list-type arrow-type label-type string-type bot-type
                               )
    (core-utils))

(include "NineMLcore.scm")
(include "NineMLreal.scm")
(include "NineMLrandom.scm")
(include "NineMLsignal.scm")
(include "NineMLdiagram.scm")
(include "NineMLalsys.scm")
(include "NineMLivp.scm")


(define current-scope      (make-parameter st-empty))
(define current-type-env   (make-parameter env-empty))
(define current-eval-env   (make-parameter env-empty))


(define (enter-typedecl id decl)
  (current-scope      (st-enter-type id (current-scope)))
  (current-type-env   (env-add-type id decl (current-type-env))))

(define (enter-valtype name ty)
  (let ((id (ident-create name)))
    (current-scope (st-enter-value id (current-scope)))
    (current-type-env   (env-add-value id ty (current-type-env)))))

(define (enter-val name val)
  (let ((id (or (and (ident? name) name) (ident-create name))))
    (current-eval-env (ident-add id val (current-eval-env)))))

(core-initialize enter-typedecl enter-valtype)
(eval-cbv-initialize enter-val)


(define (enter-module id mty)
  (current-scope (st-enter-module id (current-scope)))
  (current-type-env (env-add-module id mty (current-type-env))))


(define (apply-terms operator terms)
  (if (null? terms) 
      operator
      (apply-terms 
       (Apply operator (car terms))
       (cdr terms))))



(define (closure-formals v)

  (define (term-formals f ax)
    (cases term f
           (Function (i t) 
                     (term-formals t (cons ($ (ident-name i)) ax)))
           (else (reverse ax))))

  (cases MLvalue v
         (Closure_v (body env) (term-formals body '()))
         (else (error 'closure-formals "invalid closure" v))
         ))
 


(define opt-defaults
  `(
    (platform . mlton)
    (method . rkdp)
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

    (output-sxml        "sets output format to SXML")

    (output-xml         "sets output format to XML")

    (single-ivp          "evaluate all single-node IVP problems and save data in files ${OPERAND}_NAME.log"
                         (single-char #\d))

    (platform        "simulation platform (one of mlton, chicken, chicken/cvode, octave, octave/mlton)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((chicken chicken/cvode mlton octave octave/mlton) s)
				    (else (error '9ML-network "unrecognized platform" x))))))
			    (transformer ,string->symbol)
			     ))

    (method        "integration method (one of rkfe, rk3, rk4a, rk4b, rkoz, rkdp)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((rkfe rk3 rk4a rk4b rkoz rkdp) s)
				    (else (error '9ML-network "unrecognized method" x))))))
			    (transformer ,string->symbol)
                            ))

    (verbose          "print commands as they are executed"
		      (single-char #\v))

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
	     (pn (make-pathname dir (string-append "9ML-network-" (number->string n 16)) "tmp")))
	(cond ((file-exists? pn) (loop))
	      (else (mkdir pn) pn))))))


(define (network-failure msg . args)
  (signal
   (make-composite-condition
    (make-property-condition
       'exn
       'message "invalid response from server"
       'arguments args)
    (make-property-condition 'http-fetch))) )



(define (make-HTTP-GET/1.1 location user-agent host
			   #!key
			   (port 80)
			   (connection "close")
			   (accept "*")
			   (content-length 0))
  (conc
   "GET " location " HTTP/1.1" "\r\n"
   "Connection: " connection "\r\n"
   "User-Agent: " user-agent "\r\n"
   "Accept: " accept "\r\n"
   "Host: " host #\: port "\r\n"
   "Content-length: " content-length "\r\n"
   "\r\n") )

(define (match-http-response rsp)
  (and (string? rsp)
       (string-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" rsp)) )

(define (response-match-code? mrsp code)
  (and mrsp (string=? (number->string code) (cadr mrsp))) )

(define (match-chunked-transfer-encoding ln)
  (string-match "[Tt]ransfer-[Ee]ncoding:\\s*chunked.*" ln) )


(define (http-fetch uri dest)
  (d "fetching ~s ...~%" (uri->string uri))
  (match-let (((_ ((_ host port) ('/ . path) query) _) (uri->list uri)))
    (let* ((port      (or port 80))
	   (locn      (uri->string (update-uri (update-uri uri scheme: #f) host: #f)))
	   (query     (and query (not (string-null? query)) query))
	   (filedir   (uri-decode-string (string-concatenate (intersperse (if query path (drop-right path 1)) "/"))))
	   (filename  (uri-decode-string (or (and query (cadr (string-split query "="))) (last path))))
	   (dest      (make-pathname dest filedir))
	   (filepath  (make-pathname dest filename)))
      (if (file-exists? filepath) filepath
	  (begin
	  (d "connecting to host ~s, port ~a ...~%" host port)
	  (let-values ([(in out) (tcp-connect host port)])
		      (d "requesting ~s ...~%" locn)
		      (display
		       (make-HTTP-GET/1.1 locn "NineML" host port: port accept: "*/*")
		       out)
		      (flush-output out)
		      (d "reading response ...~%")
		      (let ([chunked #f] [ok-response #f])
			(let* ([h1 (read-line in)]
			       [response-match (match-http-response h1)])
			  (d "~a~%" h1)
			  ;;*** handle redirects here
			  (cond ((response-match-code? response-match 200)
				 (set! ok-response #t))
				((response-match-code? response-match 404)
				 (d "file not found on server: ~s~%" locn))
				(else (network-failure "invalid response from server" h1) ))
			(and ok-response
			    (begin
			      (let loop ()
				(let ([ln (read-line in)])
				  (unless (string-null? ln)
				    (when (match-chunked-transfer-encoding ln) (set! chunked #t))
				    (d "~a~%" ln)
				    (loop) ) ) )
			      (if chunked
				  (begin
				    (d "reading chunks ...~%")
				    (let ([data (read-chunks in)])
				      (close-input-port in)
				      (close-output-port out)
				      (if (not (file-exists? dest)) (mkdir dest))
				      (d "writing to ~s~%" filepath)
				      (with-output-to-file filepath (cut display data) )
				      filepath))
				  
				  (begin
				    (d "reading data ...~%")
				    (let ([data (read-string #f in)])
				      (close-input-port in)
				      (close-output-port out)
				      (if (not (file-exists? dest)) (mkdir dest))
				      (d "writing to ~s~%" filepath)
				      (with-output-to-file filepath (cut display data) binary:)
				      filepath)))))
			)
		      )))))))

  (define (read-chunks in)
    (let get-chunks ([data '()])
      (let ([size (string->number (read-line in) 16)])
	(if (zero? size)
	    (string-concatenate-reverse data)
	    (let ([chunk (read-string size in)])
	      (read-line in)
	      (get-chunks (cons chunk data)) ) ) ) ) )


(define (fetch uri)
  (case (uri-scheme uri)
    ((http)
     (let-values (((fd temp-path) (file-mkstemp "/tmp/9ML.XXXXXX")))
       (let ((data (and (http-fetch uri temp-path) (read-all temp-path))))
	 (file-close fd)
	 data)))

    ((file #f)
     (let ((data (read-all 
                  (string-concatenate
                   (intersperse (map ->string (uri-path uri)) "/")))))
       data))
    
    (else (error 'fetch "unknown scheme" (uri-scheme uri)))
    ))



(define (parse-xml str)
  (call-with-input-string str
      (lambda (in)
	(ssax:xml->sxml in `((nml . ,(string-append nineml-xmlns-base "1.0"))
                             (nml . ,(string-append nineml-xmlns-base "0.3"))
                             (nml . ,(string-append nineml-xmlns-base "0.2"))
                             )))
      ))

(define (eval-source defs current-scope current-type-env current-eval-env)
  (d "eval-source: defs = ~A~%" defs)
  (let* (
         (scoped-defs      (scope-moddef (current-scope) defs))
	 (mty              (type-moddef (current-type-env) '() scoped-defs))
	 (typed-defs       (map (lambda (x) 
                                  (cases modspec x
                                         (Value_sig (id vty) (cons id x))
                                         (Type_sig (id decl) (cons id x))
                                         (Module_sig (id mty) (cons id x))
                                         ))
                                mty))
         (type-env1        (fold (lambda (x ax) 
                                   (cases modspec x 
                                          (Value_sig  (id vty)   (env-add-value  id vty ax))
                                          (Type_sig   (id decl)  (env-add-type   id decl ax))
                                          (Module_sig (id mty)   (env-add-module id mty ax))
                                          ))
                                 (current-type-env) mty))
         (scope1          (fold (lambda (x ax) 
                                  (cases modspec x
                                         (Value_sig  (id vty)   (st-enter-value id ax))
                                         (Type_sig   (id decl)  (st-enter-type id ax))
                                         (Module_sig (id mty)   (st-enter-module id ax))
                                         ))
                                (current-scope) mty))
	 (eval-env         (mod-eval-cbv (current-eval-env) scoped-defs))
	 (unified-env      (list scoped-defs
				 (filter (lambda (x) (not (assoc (car x) (current-type-env)))) typed-defs) 
				 (filter (lambda (x) (not (assoc (car x) (current-eval-env)))) eval-env) ))
	 )
    (list unified-env (list scope1 type-env1))
    ))


(define (string->bool x)
  (cond ((string-contains-ci x "false") 0)
        ((string-contains-ci x "true") 1)
        (else #f)))


(define (make-real-const-signal value)
  (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realconst"))
         (Const `(real ,value))))

(define (make-bool-const-signal value)
  (Apply (Longid (Pdot (Pident (ident-create "Signal")) "boolconst"))
         (Const `(bool ,(if (zero? value) #f #t)))))


(define (make-real-signal name value)
  (Apply
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realsig"))
          (Const `(label ,($ name))))
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realconst"))
          (Const `(real ,value)))
   ))

(define (make-bool-signal name value)
  (Apply
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "boolsig"))
          (Const `(label ,($ name))))
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "boolconst"))
          (Const `(bool ,(if (zero? value) #f #t))))
   ))

(define (make-real-parameter name value)
  (Apply
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realparam"))
          (Const `(label ,($ name))))
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realconst"))
          (Const `(real ,value)))
   ))

(define (make-bool-parameter name value)
  (Apply
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "boolsig"))
          (Const `(label ,($ name))))
   (Apply (Longid (Pdot (Pident (ident-create "Signal")) "boolconst"))
          (Const `(bool ,(if (zero? value) #f #t))))
   ))





(define (eval-ul-component x) 

  (define path-diagram (Pdot (Pident (ident-create "Diagram")) "diagram"))
  (define path-alsys (Pdot (Pident (ident-create "AlgebraicSystem")) "alsys"))
  (define (type-diagram? t)
    (cases simple-type (typerepr t)
           (Tcon (con ts)
                 (cases tycon con
                        (Tpath (p) (path-equal? path-diagram p))
                        (else #f)))
           (Tvar (v) #f)))
  (define (type-alsys? t)
    (cases simple-type (typerepr t)
           (Tcon (con ts)
                 (cases tycon con
                        (Tpath (p) (path-equal? path-alsys p))
                        (else #f)))
           (Tvar (v) #f)))

  (let (
        (node-name  (sxml:attr x 'name))
        (definition ((sxpath `(// (*or* nml:Definition nml:definition)))  x))
	(propns     ((sxpath `(// (*or* nml:Property nml:property) @ name))  x))
	(propvs     ((sxpath `(// (*or* nml:Property nml:property) nml:SingleValue))  x))
	(fieldns    ((sxpath `(// (*or* nml:Field nml:field) @ name))  x))
	(fieldvs    ((sxpath `(// (*or* nml:Field nml:field) nml:SingleValue))  x))
	(initialns  ((sxpath `(// (*or* nml:Initial nml:initial) @ name))  x))
	(initialvs  ((sxpath `(// (*or* nml:Initial nml:initial) nml:SingleValue))  x))
        (ivp        (safe-car ((sxpath `(// nml:ivp))  x)))
        )

    (if (null? definition)
	(error 'eval-ul-component "component without definition" x))

    (let ((al-entry-name (ident-create (sxml:text (safe-car definition))))
          (uri (sxml-string->uri (sxml:attr (safe-car definition) 'url))))

      (d "NineML abstraction layer URI: ~A~%" uri)
      (d "NineML component propns: ~A~%" propns)
      (d "NineML component propvs: ~A~%" propvs)
      (d "NineML component fieldns: ~A~%" fieldns)
      (d "NineML component fieldvs: ~A~%" fieldvs)
      
      (let* (
             (uenv.tbls
              (let ((src (fetch uri)))
                (d "NineML abstraction layer source: ~A~%" src)
                (if (not src)
                    (error 'eval-ul-component "resource not found" (uri->string uri))
                    (eval-source (parse-al-sxml (parse-xml src))
                                 current-scope current-type-env current-eval-env)))
              )
             
             (dd    (d "NineML abstraction layer uenv: ~A~%" uenv.tbls))
             (uenv  (car uenv.tbls))
             (tbls  (cadr uenv.tbls))
             )
        
        (let ((eval-env (caddr uenv))
              (scope    (car tbls))
              (type-env (cadr tbls))
              )
          
          (current-scope scope)
          (current-type-env type-env)
          (current-eval-env (append eval-env (current-eval-env)))
          
          (if (null? eval-env)
              (error 'eval-ul-component "empty definition" (safe-car definition)))
          
          (d "NineML abstraction layer value: ~A~%" (last eval-env))
          
          (let* (

                 (al-entry-type (cases binding (cdr (first type-env))
                                       (Value (t) (instance t))))

                 (_ (d "NineML abstraction layer entry type: ~A~%" al-entry-type))

                 (al-return-type (let recur ((ty al-entry-type))
                                   (cases simple-type ty
                                        (Tcon (con ts)
                                              (cases tycon con
                                                     (Tpath (p) 
                                                            (if (path-equal? p path-arrow)
                                                                (recur (second ts))
                                                                ty))
                                                     (else ty)))
                                        (else ty))))
                                              

                 (_ (d "NineML abstraction layer return type: ~A~%" al-return-type))

                 (al-entry-formals (closure-formals (cdr (last eval-env))))
                 
                 (property-values
                  (map (lambda (n v) 
                         (let ((vtext (sxml:text v))
                               (name (sxml:text n)))
                           (let ((nv (string->number vtext))
                                 (bv (string->bool vtext)))
                             (cons ($ name)
                                   (or (and nv (make-real-parameter name nv))
                                       (and bv (make-bool-parameter name bv))
                                       (Apply
                                        (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realparam"))
                                               (Const `(label ,($ name))))
                                        (make-signal-expr
                                         (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline v )))
                                         '())
                                        )))
                             )))
                       propns propvs))
                 
                 (field-values
                  (map (lambda (n v) 
                         (let ((vtext (sxml:text v))
                               (name (sxml:text n)))
                           (let ((nv (string->number vtext))
                                 (bv (string->bool vtext)))
                             (cons ($ name)
                                   (or (and nv (make-real-signal name nv))
                                       (and bv (make-bool-signal name bv))
                                       (Apply
                                        (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realfield"))
                                               (Const `(label ,($ name))))
                                        (make-signal-expr
                                         (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline v )))
                                         '())
                                        )))
                             )))
                       fieldns fieldvs))
                 
                 (initial-values
                  (map (lambda (n v) 
                         (let ((name (sxml:text n))
                               (vtext (sxml:text v)))
                           (let ((nv (string->number vtext))
                                 (bv (string->bool vtext)))
                             (cons ($ name)
                                   (or (and nv (make-real-signal name nv))
                                       (and bv (make-bool-signal name bv))
                                       (Apply
                                        (Apply (Longid (Pdot (Pident (ident-create "Signal")) "realsig"))
                                               (Const `(label ,($ name))))
                                        (make-signal-expr
                                         (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline v )))
                                         '())
                                        ))
                                   ))
                             ))
                       initialns initialvs))
                 
                 
                 (ivp-duration (and ivp
                                    (car ((sxpath `(// nml:duration))  ivp))
                                    ))
                 
                 (ivp-timestep (and ivp
                                    (string->number
                                     (sxml:text
                                      (car ((sxpath `(// nml:timestep))  ivp))))
                                    ))

                 (dd (d "NineML abstraction layer formals: ~A property-values: ~A field-values: ~A initial-values: ~A return-type: ~A~%" 
                        al-entry-formals property-values field-values initial-values al-return-type))
                 
                 (node (Value_def (ident-create node-name) 
                                  (apply-terms (Longid (Pident al-entry-name)) 
                                               (let ((pfi-alst
                                                      (append property-values 
                                                              field-values
                                                              initial-values
                                                              (if (type-diagram? al-return-type)
                                                                  `((t . ,(make-real-signal "t" 0.0))
                                                                    (h . ,(make-real-signal "h" (or ivp-timestep 0.1))))
                                                                  '()))
                                                      ))
                                                 (map
                                                  (lambda (x) (let ((v (alist-ref x pfi-alst)))
                                                                (if (not v) 
                                                                    (case x
                                                                      ((_) (Const '(bool #f)))
                                                                      (else
                                                                       (error 'eval-ul-component 
                                                                              "value for quantity not found" x
                                                                              al-entry-name)))
                                                                    v)))
                                                  al-entry-formals))
                                               )
                                  ))
                 
                 (ivp-values  (and ivp
                                   (cons* (Longid (Pident (ident-create node-name)))
                                          (Const `(label t))
                                          (Const `(label h))
                                          (Const `(real 0.0))
                                          (map (compose (lambda (x) (Const `(real ,x))) string->number sxml:text) 
                                               (list ivp-duration)))))

                 
                 (ivp-name    (and ivp (sxml:attr ivp 'name)))
                 (ivp-node    (and (type-diagram? al-return-type)
                                   (or 
                                    (and ivp-values
                                         (Value_def (ident-create ivp-name) 
                                                    (apply-terms (Longid
                                                                  (Pdot (Pident (ident-create "IVP")) "run"))
                                                                 ivp-values)))
                                    (Value_def (ident-create (string-append "ivp_" node-name) )
                                               (apply-terms (Longid
                                                             (Pdot (Pident (ident-create "IVP")) "initial"))
                                                            (list (Longid (Pident (ident-create node-name)))
                                                                  (Const `(label t))
                                                                  (Const `(label h)))))
                                    )))
                 )
            
            (d "NineML abstraction layer current scope: ~A~%" (current-scope))
            (d "NineML abstraction layer entry: ~A~%" al-entry-name)
            (d "NineML ivp: ~A~%" ivp-node)
            
            (let ((v (car 
                      (eval-source (filter identity (list node ivp-node))
                                   current-scope current-type-env current-eval-env) )))

              v)
            ))
        ))
    ))


(define (parse-ul-properties prefix sxml-properties) 

  (define (parse-property-hook prefix name label value)

    (d "parse-property-hook: label = ~A name = ~A value = ~A~%" 
       label name (sxml-value->sexpr value))

      (cond
       
       ((or (and (string? label) (string=? label "sigfun"))
            (and (pair? label) (string=? (car label) "sigfun")))

        (sigfun-transform (sxml-value->sexpr value)))
       
       ((not label)
        (sigfun-transform (sxml-value->sexpr value)))
       
       (else #f)
       ))
	    

  (let ((prop-env
         (reverse
         (fold
          (lambda (node lst)

            (d "parse-ul-properties: node = ~A~%" node)
            
            (let ((name (sxml:attr node 'name))
                  (sxml-value (car ((sxpath `(// nml:SingleValue)) (list node)))))

              (d "parse-ul-properties: name = ~A sxml-value = ~A~%" 
                 name sxml-value)

              (let* (
                     (uenv.tbls
                      (let ((def
                             (Value_def (ident-create name) 
                                        (let ((vtext (sxml:text sxml-value)))
                                          (let ((n (string->number vtext))
                                                (b (string->bool vtext)))
                                            (or (and n (make-real-signal name n))
                                                (and b (make-bool-signal name b))
                                                (and
                                                 (sxml:kidn 'nml:MathInline sxml-value)
                                                 (make-signal-expr
                                                  (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline sxml-value )))
                                                  '()))
                                                (Const `(string ,vtext)))
                                            ))
                                        ))
                             )
                        (eval-source (list def)
                                     current-scope current-type-env current-eval-env) ))
                     
                     (uenv  (car uenv.tbls))
                     (tbls  (cadr uenv.tbls))
                     )
                
                (let ((eval-env (caddr uenv))
                      (scope    (car tbls))
                      (type-env (cadr tbls))
                      )
                  
                  (current-scope scope)
                  (current-type-env type-env)
                  (current-eval-env (append eval-env (current-eval-env)))
                  
                  (let* ((last-entry (last eval-env)))
                    
                    (cons last-entry lst)
                    ))
                ))
            )
          '() sxml-properties)))
        )

    (map (lambda (entry) 
           (d "parse-ul-properties: entry = ~A~%" entry)
           (let* ((name ($ (ident-name (car entry))))
                  (val  (definition-apply prefix (car entry)
                          (list (current-scope) (current-type-env) (list entry))
                          value-hook: parse-property-hook)))
             (d "parse-ul-properties: name = ~A val = ~A~%" name val)
             `(,name (expr . ,(->string val))
                     (exprML . ,(mlton-value val))
                     )))
         prop-env)

    ))


(define (eval-ul-property prefix node) 

  (define (eval-property-hook prefix name label value)

    (d "eval-property-hook: label = ~A name = ~A value = ~A~%" 
       label name (sxml-value->sexpr value))

      (cond
       
       ((or (and (string? label) (string=? label "sigfun"))
            (and (pair? label) (string=? (car label) "sigfun")))

        (sigfun-eval (sigfun-transform (sxml-value->sexpr value))))
       
       (else #f)
       ))

  (d "eval-ul-property: node = ~A~%" node)

  (let* (
         (sxml-value (car ((sxpath `(// nml:SingleValue)) (list node))))
         (name (gensym 'prop))
         (uenv.tbls
          (let ((def
                 (Value_def (ident-create name) 
                            (let ((vtext (sxml:text sxml-value)))
                              (let ((n (string->number vtext))
                                    (b (string->bool vtext)))
                                (or (and n (make-real-signal name n))
                                    (and b (make-bool-signal name b))
                                    (make-signal-expr
                                     (or (string->number (sxml:text sxml-value))
                                         (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline sxml-value ))))
                                     '())
                                     )
                                ))
                            ))
                )
            (eval-source (list def)
                         current-scope current-type-env current-eval-env) ))
         
         (uenv  (car uenv.tbls))
         (tbls  (cadr uenv.tbls))
         )
    
    (let ((eval-env (caddr uenv))
          (scope    (car tbls))
          (type-env (cadr tbls))
          )
                  
      (let* ((entry (last eval-env)))
        
        (let* ((name (ident-name (car entry)))
               (val  (definition-apply prefix (car entry)
                       (list (current-scope) (current-type-env) (list entry))
                       value-hook: eval-property-hook)))
          `(,name . ,val))
        ))
    ))



(define (make-prototype-tenv prefix name env)
  (let ((ivp-name ($ (string-append "ivp_" (->string name)))))
    (d "NineML make-prototype-tenv: ivp-name = ~A~%" ivp-name)
    (let ((sdinfo (lookup-def ivp-name env)))
      (if (not sdinfo) (error 'make-prototype "unable to find prototype" name))
      (let (
            (ivar    (lookup-def 'ivar sdinfo))
            (dvars   (lookup-def 'dvars sdinfo))
            (hvar    (lookup-def 'hvar sdinfo))
            (params  (lookup-def 'params sdinfo))
            (events  (lookup-def 'events sdinfo))
            (ic      (lookup-def 'initial-conditions sdinfo))
            (fields  (lookup-def 'fields sdinfo))
            (inputs  (lookup-def 'inputs sdinfo))
            (outputs (lookup-def 'outputs sdinfo))
            (ivpFn   (lookup-def 'ivp-id sdinfo))
            )

        (d "NineML make-prototype-tenv: ic = ~A params = ~A inputs = ~A~%" 
           ic params inputs)

        (let* (
               (states               (cons ivar dvars))
               (icstates             (filter (lambda (x) (member (car x) states)) ic))
               (outstates            (filter (lambda (x) (member x outputs)) states))
               (initialExpr/ML       (mlton-initial (append params ic) update: '((h . h))))
               (fieldExpr/ML         (and (not (null? fields)) (mlton-initial fields)))
               (initialStateExpr/ML  (mlton-initial icstates))
               (updateState/ML       (mlton-state-update 
                                      (append (map car ic) (map car params))
                                      nstate: "input" 
                                      input: "initial" 
                                      field-input: "fieldV"
                                      states: states 
                                      fields: (map car fields)
                                      update: (case (simulation-method)
                                                ((rkfe rk3 rk4a rk4b)
                                                 (map (lambda (x) `(,x . ,($ (s+ x "_i")))) inputs))
                                                ((rkoz rkdp)
                                                 (cons '(h . h_i) 
                                                       (map (lambda (x) `(,x . ,($ (s+ x "_i")))) inputs)))
                                                (else
                                                 (map (lambda (x) `(,x . ,($ (s+ x "_i")))) inputs)))))
               (copyState/ML          (mlton-state-update states 
                                                          input: "input" 
                                                          states: outstates))
               )
          (alist->tenv
           `((name               . ,name)
             (ivpFn              . ,ivpFn)
             (ivar               . ,ivar)
             (hvar               . ,hvar)
             (states             . ,states)
             (events             . ,(if (null? events) '(tnull) events))
             (inputs             . ,inputs)
             (initialExprML      . ,initialExpr/ML)
             (fieldExprML        . ,fieldExpr/ML)
             (initialStateExprML . ,initialStateExpr/ML)
             (updateStateML      . ,updateState/ML)
             (copyStateML        . ,copyState/ML)
             ))
          ))
      ))
  )
  

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
		      (let ((name ($ (sxml:text node))))
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
    (d "make-projection-tenv: connectivity = ~A~%" connectivity)
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
       )
     ))
  
  

(define (make-response-tenv prefix name ports env)
  (let ((ivp-name ($ (string-append "ivp_" (->string name)))))
    (let ((sdinfo (lookup-def ivp-name env)))
      (if (not sdinfo) (error 'make-response "unable to find prototype" name))
      (let (
            (ivar    (lookup-def 'ivar sdinfo))
            (dvars   (lookup-def 'dvars sdinfo))
            (hvar    (lookup-def 'hvar sdinfo))
            (params  (lookup-def 'params sdinfo))
            (events  (lookup-def 'events sdinfo))
            (outputs (lookup-def 'outputs sdinfo))
            (ic      (lookup-def 'initial-conditions sdinfo))
            (event-ports (lookup-def 'event-ports ports))
            (plasticity-ports (lookup-def 'plasticity-ports ports))
            (destination-ports (lookup-def 'destination-ports ports))
            )
        (d "NineML make-response-tenv: plasticity-ports = ~A destination-ports = ~A~%" 
           plasticity-ports destination-ports)
        (let* ((ivpFn                (lookup-def 'ivp-id sdinfo))
               (states               (cons ivar dvars))
               (outstates            (filter (lambda (x) (member x outputs)) states))
               (icstates             (filter (lambda (x) (member (car x) states)) ic))
               (initialExpr/ML       (mlton-initial (append params ic) update: '((h . h))))
               (initialStateExpr/ML  (mlton-initial icstates))
               (updateState/ML       (mlton-state-update
                                      (append (map car ic) (map car params))
                                      nstate: "input" 
                                      input:  "initial" 
                                      states: states 
                                      update: `(
                                                (,(cadr event-ports) . spike_i) 
                                                (,(cadr plasticity-ports) . weight_i)
                                                )
                                      ))
               (copyState/ML         (mlton-state-update
                                      states 
                                      input: "input" 
                                      states: outstates ))
               )
          (d "NineML make-response-tenv: ic = ~A states = ~A~%" ic states)
          (alist->tenv
           `((name               . ,name)
             (ivpFn              . ,ivpFn)
             (states             . ,states)
             (ics                . ,(map car ic))
             (initialExprML      . ,initialExpr/ML)
             (initialStateExprML . ,initialStateExpr/ML)
             (updateStateML      . ,updateState/ML)
             (copyStateML        . ,copyState/ML)
             (outputState        . ,(cadr destination-ports))
             ))
          ))
      ))
  )


(define (make-plasticity-tenv prefix name env)
  (let ((ivp-name ($ (string-append "ivp_" (->string name)))))
    (let ((sdinfo (lookup-def ivp-name env)))
      (if (not sdinfo) (error 'make-plasticity "unable to find prototype" name))
      (let ((ivar    (lookup-def 'ivar sdinfo))
            (dvars   (lookup-def 'dvars sdinfo))
            (hvar    (lookup-def 'hvar sdinfo))
            (events  (lookup-def 'events sdinfo))
            (ic      (lookup-def 'initial-conditions sdinfo)))
        (let* ((ivpFn                (lookup-def 'ivp-id sdinfo))
               (states               (cons ivar dvars))
               (icstates             (filter (lambda (x) (member (car x) states)) ic))
               (initialExpr/ML       (mlton-initial ic update: '((h . h))))
               (initialStateExpr/ML  (mlton-initial icstates))
               )
          (d "NineML make-plasticity-tenv: states = ~A ics = ~A~%" states (map car ic))
          (alist->tenv
           `((name               . ,name)
             (ivpFn              . ,ivpFn)
             (states             . ,states)
             (ics                . ,(map car ic))
             (initialExprML      . ,initialExpr/ML)
             (initialStateExprML . ,initialStateExpr/ML)
             ))
          ))
      ))
  )


(define (make-connection-tenv prefix name stdlib-env alsys-env)

  (let ((sys-name ($ (->string name))))

  (cond

   ((lookup-def sys-name stdlib-env) =>
    (lambda (stdlib)
      (match stdlib
             (('Tuple ('left _) ('right ('Tuple ('left ('Const ('string stdlib-name))) _)))
              (alist->tenv
               `((name  . ,sys-name)
                 (stdlib . ,stdlib-name))))
             (else (error 'make-connection-tenv "unknown stdlib connection")))))

   ((lookup-def sys-name alsys-env) =>
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
             (initialExprML      . ,initialExpr/ML)
             (initialStateExprML . ,initialStateExpr/ML)
             ))
          ))
      ))
   (else
    (error 'make-connection-tenv "unknown connection equation system" name))
   ))
  )


(define (make-group-tenv name order populations sets projections 
                         psr-types plas-types connection-types
                         spikepoplst properties)
  (let ((alst 
         `((group 
            . 
            ((name        . ,name)
             (order       . ,order)
             (sets        . ,sets)
             (populations . ,populations)
             (projections . ,projections)
             (psrtypes    . ,(if (null? psr-types) #f psr-types))
             (plastypes   . ,(if (null? plas-types) #f plas-types))
             (conntypes   . ,(if (null? connection-types) #f connection-types))
             (properties  . ,(if (null? properties) '(tnull) properties))
             (spikepoplst . ,spikepoplst)
             ))
           ))
        )
    (alist->tenv alst)))


(define (eval-ul-group prefix ul-properties node ivp-env alsys-env stdlib-env)

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
         
  (let (
        (group-name       (sxml:attr node 'name))
	(populations-sxml ((sxpath `(// nml:Population))  node))
	(selections-sxml  ((sxpath `(// nml:Selection))  node))
	(projections-sxml ((sxpath `(// nml:Projection)) node))
        (properties-sxml  ((sxpath `(// nml:Property)) node))
        (spikerecord-sxml ((sxpath `(nml:SpikeRecording)) node))
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
                        (size (eval-ul-property group-name (sxml:kidn* 'nml:Number node)))
                        (size-val (inexact->exact (cdr size))))
                   (list
                    (cons
                     `(,($ name) . ,(make-population-tenv ($ name) (make-prototype-tenv prefix prototype-name ivp-env) 
                                                          size-val order))
                     populations)
                    (+ size-val order)
                    ))
                 ))
             (list '() 0)
             populations-sxml))

           (populations (reverse (car populations+order)))
           (order (cadr populations+order))

           (sets
            (append 
             (map (lambda (x) 
                    (let ((name (car x)))
                      `(,name . ((name . ,name) 
                                 (populations . ,(ersatz:sexpr->tvalue (list (cdr x))))
                                 (size . ,(alist-ref 'size (cdr x))))
                              )
                      ))
                  populations)
             (map (lambda (node)
                    (let* ((name (sxml:attr node 'name))
                           (set (make-population-set (sxml:kid node) populations)))
                      `(,($ name) . ,(make-population-set-tenv ($ name) set))))
                  selections-sxml)))

           (projections+types
            (map (lambda (node)
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
                           
                          (response-event-ports 
                           (and response-node
                                (let ((from-source
                                       (sxml:kidn* 'nml:FromSource response-node )))
                                  (list ($ (sxml:attr from-source 'event_send_port))
                                        ($ (sxml:attr from-source 'event_receive_port)))
                                  )))
                          (response-plasticity-ports 
                           (and response-node
                                 (let ((from-plasticity
                                        (sxml:kidn* 'nml:FromPlasticity response-node )))
                                   (list ($ (sxml:attr from-plasticity 'send_port))
                                         ($ (sxml:attr from-plasticity 'receive_port)))
                                 )))
                          (response-ports `(
                                            (event-ports . ,response-event-ports)
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
                          (del  (cdr (eval-ul-property name (sxml:kidn* 'nml:Delay node))))
                          (properties      (parse-ul-properties 
                                            name
                                            (append (sxml:kidsn 'nml:property connectivity)
                                                    (sxml:kidsn 'nml:Property connectivity))))
                          )

                     (d "group-ul-eval: projection node = ~A~%" node)
                     (d "group-ul-eval: response = ~A response-name = ~A~%" response-node response-name)
                     (d "group-ul-eval: delay = ~A~%" del)
                     (d "group-ul-eval: type = ~A ~%" type)
                     (d "group-ul-eval: plasticity = ~A plasticity-name = ~A~%" plasticity-node plasticity-name)
                     (d "group-ul-eval: properties = ~A ~%" properties)
                     (d "group-ul-eval: connectivity-name = ~A ~%" connectivity-name)
                     (d "group-ul-eval: connectivity-port = ~A ~%" connectivity-port)

                     (let* (
                            (source (lookup-def source-name sets))
                            (destination (lookup-def destination-name sets))
                            (response (and response-name (make-response-tenv prefix response-name response-ports ivp-env)))
                            (plasticity (and plasticity-name (make-plasticity-tenv prefix plasticity-name ivp-env)))
                            (connection (and connectivity-name (make-connection-tenv prefix connectivity-name stdlib-env alsys-env)))
                           )

                       (d "group-ul-eval: plasticity tenv = ~A~%" plasticity)
                       (d "group-ul-eval: plasticity tenv = ~A~%" plasticity)
                       
                       (if (not source)
                           (error 'eval-ul-group "invalid projection source" source))

                       (if (not destination)
                           (error 'eval-ul-group "invalid projection destination" destination))

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
                                        `((expr . ,del) (exprML . ,(mlton-value del)))
                                        properties))
                        `(
                          ,(and response-name ($ response-name)) 
                          (type       . ,type)
                          (response   . ,response) 
                          (ports      . ,response-ports)
                          (projection . ,name)
                          )
                        `(,(and plasticity-name ($ plasticity-name)) 
                          (plasticity . ,plasticity) )
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
                       
                   `(,name . ,(append response 
                                      `((projections . ,projection-names)
                                        (type  . ,(car projection-types))
                                        (range . ,(projections-range
                                                   (map (lambda (x) (alist-ref ($ x) projections)) 
                                                        projection-names)))
                                        (ports . ,ports)
                                        )))
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

      (let* (
             (shared-dir    (chicken-home))
             (template-dir  (make-pathname (make-pathname shared-dir "9ML") "templates"))
             (network-tmpl  (case (simulation-method)
                              ((rkfe rk3 rk4a rk4b)
                               "Network.sml.tmpl")
                              ((rkoz rkdp)
                               "Network.sml.adaptive.tmpl")
                              (else
                               "Network.sml.tmpl")))
             (sim-tmpl      (case (simulation-method)
                              ((rkfe rk3 rk4a rk4b)
                               "Sim.sml.tmpl")
                              ((rkoz rkdp)
                               "Sim.sml.adaptive.tmpl")
                              (else
                               "Sim.sml.tmpl")))
             (mlb-tmpl      "Sim.mlb.tmpl")
             (makefile-tmpl "Makefile.tmpl")
             (group-path    (make-pathname (pathname-directory prefix)
                                           (string-append group-name ".sml")))
             (sim-path      (make-pathname (pathname-directory prefix)
                                           (string-append "Sim" group-name ".sml")))
             (mlb-path      (make-pathname (pathname-directory prefix)
                                           (string-append "Sim" group-name ".mlb")))
             (exec-path     (make-pathname (pathname-directory prefix)
                                           (string-append "Sim" group-name)))
             (makefile-path (make-pathname (pathname-directory prefix) 
                                           (string-append "Makefile." group-name)))
             (spikelst      (fold (lambda (node ax)
                                    (let ((set (alist-ref ($ (sxml:attr node 'set)) sets)))
                                      (let ((populations
                                             (let ((poplst (alist-ref 'populations set)))
                                               (ersatz:tvalue->sexpr poplst))))
                                        (append
                                         (map (lambda (x) (->string (alist-ref 'name x))) populations)
                                         ax))))
                                  '() spikerecord-sxml))
             (group-tenv    (make-group-tenv group-name order populations sets projections 
                                             psr-types plas-types connection-types spikelst 
                                             (append properties ul-properties) ))

             )

        (d "group-tenv = ~A~%" (map (lambda (x) (cons (car x) (ersatz:tvalue->sexpr (cdr x)))) group-tenv))

        (make (

               (group-path (prefix)
                           (with-output-to-file group-path 
                             (lambda ()
                               (print (ersatz:from-file 
                                       network-tmpl
                                       env: (template-std-env search-path: `(,template-dir))
                                       models: group-tenv))))
                           )
        
               (sim-path (prefix)
                         (with-output-to-file sim-path 
                           (lambda ()
                             (print (ersatz:from-file 
                                     sim-tmpl
                                     env: (template-std-env search-path: `(,template-dir))
                                     models: group-tenv))))
                         )
               
               (mlb-path ()
                         (with-output-to-file mlb-path 
                           (lambda ()
                             (print (ersatz:from-file 
                                     mlb-tmpl
                                     env: (template-std-env search-path: `(,template-dir))
                                         models: group-tenv))))
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
                                                                             (make-pathname shared-dir "signal-diagram")
                                                                             "sml-lib")))
                                                     (nineml_lib_home . ,(Tstr (make-pathname 
                                                                                (make-pathname shared-dir "9ML")
                                                                                "sml-lib")))
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
  


(define (make-stdlib-hook stdlib-env)

  (lambda (prefix name label value)

    (print "name = " name " label = " label " value = " value)

    (cond

     ((or (and (string? label) (string=? label "stdlib"))
          (and (pair? label) (string=? (car label) "stdlib"))) ;; value is string reference to stdlib connectivity

        (stdlib-env (cons `(,(string->symbol name) . ,value) (stdlib-env)))
	)
     
     (else #f)
     ))
  )


(define (main options operands)

  (if (options 'help) (network:usage))

  (let ((find-module (lambda (x) (env-find-module x (current-type-env)))))
    (for-each (lambda (init name) (init name enter-module find-module current-eval-env))
	      (list Real:module-initialize   
		    Random:module-initialize   
		    Signal:module-initialize   
		    Diagram:module-initialize  
		    IVP:module-initialize 
		    AlgebraicSystem:module-initialize  )
	      (list "Real" "Random" "Signal" "Diagram" "IVP" "AlgebraicSystem" )) )

  (if (null? operands)

      (network:usage)

      (let ((output-type (cond ((options 'output-xml)  'xml)
			       ((options 'output-sxml) 'sxml)
			       (else #f))))

	(if (options 'verbose) (begin (eval-verbose 1) (ivp-verbose 1) 
                                      (alsys-verbose 1) (network-verbose 1)))

	(simulation-platform (or (options 'platform) (defopt 'platform) ))
	(simulation-method (or (options 'method) (defopt 'method) ))


        (ivp-simulation-platform (simulation-platform))
        (alsys-simulation-platform (simulation-platform))
        (ivp-simulation-method (simulation-method))

	(for-each

	 (lambda (operand)

	   (let* (
                  (ul-sxml (parse-xml (read-all operand)))
		  (ul-imports ((sxpath `(// nml:NineML nml:import))  ul-sxml))
		  (ul-import-sxmls (map (lambda (x) (parse-xml (fetch (sxml-string->uri (sxml:text x))))) ul-imports))
                  )

	     (let* (
                    (ul-sxml (fold append ul-sxml ul-import-sxmls))
		    (ul-properties  (parse-ul-properties
                                     operand ((sxpath `(// nml:NineML (*or* nml:Property nml:property)))  ul-sxml)))
		    (ul-groups ((sxpath `(// nml:NineML (*or* nml:Group nml:group)))  ul-sxml))

		    (ul-components ((sxpath `(// nml:NineML (*or* nml:Component nml:component)))  ul-sxml))
		    (ul-component-uenvs (map eval-ul-component ul-components))
                    
                    (ivp-node-env (make-parameter '()))
                    (alsys-node-env (make-parameter '()))
                    (stdlib-env (make-parameter '()))
		    )

               (d "ul-sxml = ~A~%" ul-sxml)

               (d "ul-properties = ~A~%" ul-properties)
               (d "ul-groups = ~A~%" ul-groups)
               (d "ul-components = ~A~%" ul-components)
               (d "ul-component-uenvs = ~A~%" ul-component-uenvs)

	       (for-each
		(lambda (uenv) 
	       
		  (let (
                        (source-defs (car uenv))
                        (mty         (cadr uenv))
                        (eval-env    (caddr uenv))
                        )
		    
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
			      (print-eval-env eval-env output-type)
			      (let ((fc (lambda (x) (and (member (ident-name (car x)) eval-env-opt) x))))
				(print-eval-env eval-env output-type fc)))
			  ))
		    
		    (if (options 'print-source-defs)
			(print-source-defs source-defs output-type))
                    
                    (if (options 'single-ivp)
                        (begin
                          (traverse-definitions operand uenv value-hook: (make-ivp-data-hook ivp: #t))
                          (process-wait) )
                        (if (not (null? ul-groups))
                            (begin
                              (traverse-definitions operand uenv value-hook: (make-ivp-cgen-hook ivp-node-env))
                              (traverse-definitions operand uenv value-hook: (make-alsys-cgen-hook alsys-node-env))
                              (traverse-definitions operand uenv value-hook: (make-stdlib-hook stdlib-env))
                              )))

		    ))
		 ul-component-uenvs)

               (d "ivp-node-env = ~A~%" (ivp-node-env))
               (d "alsys-node-env = ~A~%" (alsys-node-env))
               (d "stdlib-env = ~A~%" (stdlib-env))

               (for-each (lambda (x) (eval-ul-group operand ul-properties x 
                                                    (ivp-node-env) (alsys-node-env) (stdlib-env)))
                         ul-groups)

               ))
           )

	 operands))))

(main opt (opt '@))


