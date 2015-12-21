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


(require-extension extras posix utils files data-structures tcp srfi-1 srfi-13 irregex)
(require-extension datatype matchable make ssax sxml-transforms sxpath sxpath-lolevel 
                   object-graph ersatz-lib unitconv uri-generic getopt-long )
(require-extension 9ML-types 9ML-parse 9ML-ivp-mlton)

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


(define (safe-car x) (and (pair? x) (car x)))

(define (sxml-singleton x)
  (and (pair? x) (car x)))

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

    (method        "integration method (one of rkfe, rk3, rk4a, rk4b, rkhe, rkbs, rkf45, rkv65, rkf78, rkoz, rkdp, crk3, crkdp, crkbs)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((rkfe rk3 rk4a rk4b rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crk3 crkdp crkbs ) s)
				    (else (error '9ML-singlecell "unrecognized method" x))))))
			    (transformer ,string->symbol)
                            )
                     (single-char #\m)
                     )

    (verbose          "print commands as they are executed"
		      (single-char #\v))

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
                             )))
      ))


(define default-units
  `(
    (current . nA)
    ))


(define (eval-sxml-units dimensions-sxml units-sxml)
    (let* ((dimensions (map parse-sxml-dimension dimensions-sxml))
           (dimensions-env (map (lambda (x) (cons (quantity-name x) x)) dimensions))
           (units (map (lambda (x) (parse-sxml-unit x dimensions-env)) units-sxml))
           (units-env (map (lambda (x) (cons (unit-name x) x)) units)))
      (salt:model-quantities (append dimensions-env (salt:model-quantities)))
      (salt:model-units (append units-env (salt:model-units)))
      ))


(define (eval-ul-component x) 

  (let (
        (node-name  (sxml:attr x 'name))
        (definition ((sxpath `(// (*or* nml:Definition nml:definition)))  x))
	(propns     ((sxpath `(// (*or* nml:Property nml:property) @ name))  x))
	(propvs     ((sxpath `(// (*or* nml:Property nml:property) nml:SingleValue))  x))
	(propunits  ((sxpath `(// (*or* nml:Property nml:property) @ units))  x))
	(fieldns    ((sxpath `(// (*or* nml:Field nml:field) @ name))  x))
	(fieldvs    ((sxpath `(// (*or* nml:Field nml:field) nml:SingleValue))  x))
	(initialns  ((sxpath `(// (*or* nml:Initial nml:initial) @ name))  x))
	(initialvs  ((sxpath `(// (*or* nml:Initial nml:initial) nml:SingleValue))  x))
	(initialunits  ((sxpath `(// (*or* nml:Initial nml:initial) @ units))  x))
        (ivp        (safe-car ((sxpath `(// nml:IVP))  x)))
        )

    (if (null? definition)
	(error 'eval-ul-component "component without definition" x))

    (let ((al-definition-name (string->symbol (sxml:text (safe-car definition))))
          (uri (sxml-string->uri (sxml:attr (safe-car definition) 'url))))

      (d "NineML abstraction layer URI: ~A~%" (uri->string uri))
      (d "NineML abstraction layer definition name: ~A~%" al-definition-name)
      (d "NineML component propns: ~A~%" propns)
      (d "NineML component propvs: ~A~%" propvs)
      (d "NineML component propunits: ~A~%" propunits)
      (d "NineML component initialns: ~A~%" initialns)
      (d "NineML component initialvs: ~A~%" initialvs)
      (d "NineML component initialunits: ~A~%" initialunits)
      (d "NineML component fieldns: ~A~%" fieldns)
      (d "NineML component fieldvs: ~A~%" fieldvs)
      
      (let* (
             (model-src (fetch uri))
             (model-sxml
              (if (not model-src)
                  (error 'eval-ul-component "resource not found" (uri->string uri))
                  (parse-xml model-src)))
             (model-decls (parse-al-sxml model-sxml))
             (dd     (d "NineML abstraction layer model declarations: ~A~%" model-decls))

             (model-dimensions-sxml ((sxpath `(// nml:NineML nml:Dimension)) model-sxml))
             (model-units-sxml ((sxpath `(// nml:NineML nml:Unit)) model-sxml))

             (units-env (eval-sxml-units model-dimensions-sxml model-units-sxml))

             (model-env (map (match-lambda
                              (($ dynamics-node model-name model-formals model-decls) 
                               (cons model-name (make-dynamics-node model-name model-formals 
                                                                    (salt:parse model-decls))))
                              ((and ($ alsys-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              ((and ($ connection-rule-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              (node (error 'eval-ul-component "unknown node type" node)))
                             model-decls))

             (dd     (d "NineML abstraction layer models: ~A~%" model-env))
             (model  (alist-ref al-definition-name model-env))
             (dd     (d "NineML abstraction layer model intermediate form: ~A~%" model))


             (model-parameters
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:Parameter)) model-sxml)))
             (model-variables
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:StateVariable)) model-sxml)))
             (model-event-receive-ports
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:EventReceivePort)) model-sxml)))
             (model-analog-receive-ports
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:AnalogReceivePort)) model-sxml)))
             (model-reduce-ports
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:AnalogReducePort)) model-sxml)))
             )

        (if (not model)
            (error 'eval-ul-component "cannot find definition named" al-definition-name))
        
        (let* (
               (parameter-decls
                  (map (lambda (n v u) 
                         (let* ((vtext (sxml:text v))
                                (name (string->symbol (sxml:text n)))
                                (unit (string->symbol (sxml:text u)))
                                (dim  (alist-ref name model-parameters)))
                           (if unit
                               `(define ,name = parameter (dim ,dim) (,(parse-string-expr vtext) * ,unit))
                               `(define ,name = parameter ,(parse-string-expr vtext)))
                           ))
                       propns propvs propunits))
                 
               (field-decls
                (map (lambda (n v) 
                       (let ((vtext (sxml:text v))
                             (name (sxml:text n)))
                         (cons ($ name)
                               `(signal.realfield
                                 ,(make-signal-expr
                                   (parse-string-expr vtext)
                                   '()))
                               )))
                     fieldns fieldvs))
                 
               (state-decls
                (map (lambda (n v u) 
                       (let* ((name (string->symbol (sxml:text n)))
                              (vtext (sxml:text v))
                              (unit (string->symbol (sxml:text u)))
                              (dim  (alist-ref name model-variables)))
                         (if unit
                             `(define ,name = unknown (dim ,dim) (,(parse-string-expr vtext) * ,unit))
                             `(define ,name = unknown ,(parse-string-expr vtext)))
                         ))
                     initialns initialvs initialunits))
                 
               (ext-decls
                (map (match-lambda 
                      ((name . dim) 
                       (case dim
                         ((Unity)
                          `(define ,name = external 0.0))
                         (else
                          (let ((unit (alist-ref dim default-units )))
                            (if (not unit) 
                                (error 'eval-ul-component
                                       "cannot find default unit for dimension in receive port definition"
                                       dim name))
                            `(define ,name = external (dim ,dim) (0.0 * ,unit)))))))
                     model-analog-receive-ports))
                 
               (extev-decls
                (map (match-lambda 
                      ((name . dim) 
                       (case dim
                         ((Unity)
                          `(define ,name = external-event +inf.0))
                         (else
                          (let ((unit (alist-ref dim default-units )))
                            (if (not unit) 
                                (error 'eval-ul-component
                                       "cannot find default unit for dimension in receive port definition"
                                       dim name))
                            `(define ,name = external-event (dim ,dim) (0.0 * ,unit)))))))
                     model-event-receive-ports))
                 
               (reduce-decls
                (map (match-lambda 
                      ((name . dim) 
                       (let ((unit (alist-ref dim default-units )))
                         (if (not unit) 
                             (error 'eval-ul-component
                                    "cannot find default unit for dimension in reduce port definition"
                                    dim name))
                         `(define ,name = unknown (dim ,dim) (0.0 * ,unit)))))
                     model-reduce-ports))
                 
                 
               )


          (match model
                 (($ dynamics-node model-name model-formals model-decls) 
                  (let ((decls (append parameter-decls
                                       state-decls
                                       ext-decls
                                       extev-decls
                                       reduce-decls
                                       (list model-decls))))
                    (pp `(model-decls = ,decls) (current-error-port))
                    (cons (string->symbol node-name)
                          (make-dynamics-node 
                           model-name model-formals 
                           (salt:parse decls)
                           ))
                    ))
                 
                 ((and ($ alsys-node model-name model-formals model-decls) node)
                  (cons (string->symbol node-name) node))
                 ((and ($ connection-rule-node model-name model-formals model-stdlib) node)
                  (let ((parameters
                         (map (lambda (n v) 
                                (let* ((vtext (sxml:text v))
                                       (name (string->symbol (sxml:text n))))
                                  `(,name . ,vtext)))
                              propns propvs)))
                  (cons (string->symbol node-name) 
                        (make-connection-rule-node model-name model-formals 
                                                   (cons model-stdlib parameters)))
                  ))
                 )
          ))
      ))
  )


(define (parse-ul-properties prefix sxml-properties) 

  (let (
        (prop-env
         (reverse
          (map
           (lambda (node lst)

             (d "parse-ul-properties: node = ~A~%" node)
            
             (let ((name (sxml:attr node 'name))
                   (sxml-value (car ((sxpath `(// nml:SingleValue)) (list node)))))

               (d "parse-ul-properties: name = ~A sxml-value = ~A~%" 
                  name sxml-value)

               (let ((n (string->number vtext)))
                 (or (and n (make-real-signal name n))
                     (and
                      (sxml:kidn 'nml:MathInline sxml-value)
                      (make-signal-expr
                       (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline sxml-value )))
                       '()))
                     ))
               ))
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

  (d "eval-ul-property: node = ~A~%" node)

  (let* (
         (sxml-single-value (sxml:kidn 'nml:SingleValue node))
         (sxml-math-expr (sxml:kidn 'nml:MathInline node))
         (name (gensym 'prop))
         )
    `(,name .
      ,(let* ((vtext (sxml:text node))
              (n (string->number vtext)))
         (cond
          (n n)
          (sxml-single-value
           (make-signal-expr
            (string->number (sxml:text sxml-single-value))))
          (sxml-math-expr
           (parse-string-expr (sxml:text sxml-math-expr)))
          (else (error 'eval-ul-property "unknown property format" node))
          ))
      ))
  )


(define (codegen-ul-component name ul-properties node)
  
  (let* (
         (shared-dir    (chicken-home))
         (template-dir  (make-pathname (make-pathname shared-dir "9ML") "templates"))
         (sim-tmpl      (case (ivp-simulation-method)
                          ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crkdp crkbs) "Sim.sml.single.adaptive.tmpl")
                          (else "Sim.single.sml.tmpl")))
         (mlb-tmpl      (case (ivp-simulation-method)
                          ((rkhe rkbs rkf45 rkck rkoz rkdp rkf45 rkf78 rkv65 crkbs crkdp) "Sim.mlb.single.adaptive.tmpl")
                          (else "Sim.mlb.single.tmpl")))
         (makefile-tmpl "Makefile.single.tmpl")
         (sim-path      (make-pathname source-directory (conc "Sim_" name ".sml")))
         (mlb-path      (make-pathname source-directory (conc "Sim_" name ".mlb")))
         (exec-path     (make-pathname source-directory (conc "Sim_" name)))
         (makefile-path (make-pathname source-directory (conc "Makefile." name)))

         )


    (match-let
     (
      (($ dynamics-node model-name model-formals model-eqset) node)
      )
     (d "node name = ~A model-eqset = ~A responses = ~A~%" name model-eqset responses)
     (let* (
            (prototype-decls
             (salt:make-astdecls
              (salt:astdecls-decls model-eqset)))
            )
       (d "prototype-decls = ~A~%" prototype-decls)
       (let* ((sim (salt:simcreate (salt:elaborate prototype-decls))))
         (let ((sml-port (open-output-file (make-pathname source-directory (sprintf "~A.sml" node-name)))))
           (salt:codegen-ODE/ML node-name sim out: sml-port solver: (ivp-simulation-method) libs: '(random))
           (close-output-port sml-port)
           (case (ivp-simulation-method) 
             ((crk3 crkbs crkdp)
              (let ((c-port (open-output-file (make-pathname source-directory (sprintf "~A.c" node-name)))))
                (salt:codegen-ODE/C node-name sim out: c-port solver: (ivp-simulation-method) libs: '(random))
                (close-output-port c-port)
                ))
             (else (begin)))
           )
         ))
     
     (make (
            
            (sim-path (group-path)
                      (with-output-to-file sim-path 
                        (lambda ()
                          (print (ersatz:from-file 
                                  sim-tmpl
                                  env: (template-std-env search-path: `(,template-dir))
                                  models: `(
                                            (modelName . ,(Tstr model-name))
                                            ))
                                 ))
                        ))
            
            (mlb-path (group-path)
                      (with-output-to-file mlb-path 
                        (lambda ()
                          (print (ersatz:from-file 
                                  mlb-tmpl
                                  env: (template-std-env search-path: `(,template-dir))
                                  models: `(
                                            (modelName . ,(Tstr model-name))
                                            (UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                    ((crk3 crkdp crkbs) #t)
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
                                       models: `((modelName . ,(Tstr model-name))
                                                 (sml_lib_home . ,(Tstr (make-pathname 
                                                                         (make-pathname shared-dir "salt")
                                                                         "sml-lib")))
                                                 (nineml_lib_home . ,(Tstr (make-pathname 
                                                                            (make-pathname shared-dir "9ML")
                                                                            "sml-lib")))
                                                 (UseCSolver . ,(Tbool (case (ivp-simulation-method)
                                                                         ((crk3 crkbs crkdp) #t)
                                                                         (else #f))))
                                                 (CSolverFiles . ,(Tlist (case (ivp-simulation-method)
                                                                           ((crk3) (list "crk3.c"))
                                                                           ((crkbs) (list "crkbs.c"))
                                                                           ((crkdp) (list "crkdp.c"))
                                                                           (else (list)))))
                                                 )
                                       ))
                               ))
                           )

            (exec-path (group-path sim-path mlb-path makefile-path)
                       (run (make -f ,makefile-path)))
            
            )

          (list exec-path) )
        ))
    )




(define (main options operands)

  (if (options 'help) (singlecell:usage))

  (if (null? operands) (singlecell:usage))

  (if (options 'verbose) 
      (begin
        (salt:verbose 1)
        (singlecell-verbose 1)))
  
  (simulation-platform (or (options 'platform) (defopt 'platform) ))
  (simulation-method (or (options 'method) (defopt 'method) ))
  
  (ivp-simulation-platform (simulation-platform))
  (alsys-simulation-platform (simulation-platform))
  (ivp-simulation-method (simulation-method))
  
  (for-each
   
   (lambda (operand)
     
     (let* (
            (nineml-sxml ((sxpath `(// nml:NineML)) (parse-xml (read-all operand))))
            (model-sxml (sxml:kids nineml-sxml))
            (ul-imports ((sxpath `(// (*or* nml:Import nml:import)))  model-sxml))
            (ul-import-sxmls (map (lambda (x) (parse-xml (fetch (sxml-string->uri (sxml:text x))))) ul-imports))
            (all-sxml (fold append model-sxml ul-import-sxmls))
            )
       
       (let-values (((component-env ul-sxml) (resolve-ul-components all-sxml)))
         
         (pp `(all-sxml . ,all-sxml) (current-error-port))
         (pp `(component-env . ,component-env) (current-error-port))
         
         (let ((dimensions-sxml (sxml:kidsn 'nml:Dimension `(nml:NineML . ,all-sxml)))
               (units-sxml (sxml:kidsn 'nml:Unit `(nml:NineML . ,all-sxml))))
           (eval-sxml-units dimensions-sxml units-sxml))
         
         (let* (
                (ul-properties
                 (parse-ul-properties
                  operand ((sxpath `(// (*or* nml:Property nml:property)))  ul-sxml)))
                
                (dd (d "ul-properties = ~A~%" ul-properties))
                
                (ul-component-eval-env
                 (map eval-ul-component component-env))
                
                )
           
           (for-each (lambda (x) (codegen-ul-component operand ul-properties x)) ul-component-eval-env)
           
           ))
       ))
   
   operands))


(main opt (opt '@))


