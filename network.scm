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


(require-extension extras posix utils files data-structures tcp srfi-1 srfi-13 irregex)
(require-extension datatype matchable make ssax sxml-transforms sxpath sxpath-lolevel 
                   object-graph ersatz-lib uri-generic getopt-long )
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

    (output-sxml        "sets output format to SXML")

    (output-xml         "sets output format to XML")

    (single-ivp          "evaluate all single-node IVP problems and save data in files ${OPERAND}_NAME.dat"
                         (single-char #\d))

    (platform        "simulation platform (one of mlton, chicken, chicken/cvode, octave/mlton)"
		     (value (required PLATFORM)
			    (predicate 
			     ,(lambda (x) 
				(let ((s (string->symbol (string-downcase x))))
				  (case s
				    ((chicken chicken/cvode mlton octave/mlton) s)
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
                             )))
      ))


(define (eval-ul-component x) 

  (let (
        (node-name  (sxml:attr x 'name))
        (definition ((sxpath `(// (*or* nml:Definition nml:definition)))  x))
	(propns     ((sxpath `(// (*or* nml:Property nml:property) @ name))  x))
	(propvs     ((sxpath `(// (*or* nml:Property nml:property) nml:SingleValue))  x))
	(fieldns    ((sxpath `(// (*or* nml:Field nml:field) @ name))  x))
	(fieldvs    ((sxpath `(// (*or* nml:Field nml:field) nml:SingleValue))  x))
	(initialns  ((sxpath `(// (*or* nml:Initial nml:initial) @ name))  x))
	(initialvs  ((sxpath `(// (*or* nml:Initial nml:initial) nml:SingleValue))  x))
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
      (d "NineML component fieldns: ~A~%" fieldns)
      (d "NineML component fieldvs: ~A~%" fieldvs)
      
      (let* (
             (model-srcs
              (let ((src (fetch uri)))
                (if (not src)
                    (error 'eval-ul-component "resource not found" (uri->string uri))
                    (parse-al-sxml (parse-xml src))
                    )))

             (model-env (map (match-lambda
                              (($ dynamics-node model-name model-formals model-decls) 
                               (cons model-name (make-dynamics-node model-name model-formals 
                                                                    (salt:parse model-decls))))
                              ((and ($ alsys-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              ((and ($ connection-rule-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              (node (error 'eval-ul-component "unknown node type" node)))
                             model-srcs))
             (dd     (d "NineML abstraction layer models: ~A~%" model-env))
             (model  (alist-ref al-definition-name model-env))
             (dd     (d "NineML abstraction layer model intermediate form: ~A~%" model))
             )

        (if (not model)
            (error 'eval-ul-component "cannot find definition named" al-definition-name))
        
        (let* (
               (parameter-decls
                  (map (lambda (n v) 
                         (let ((vtext (sxml:text v))
                               (name (sxml:text n)))
                           `(define ,($ name) = parameter ,(parse-string-expr vtext))
                           ))
                       propns propvs))
                 
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
                (map (lambda (n v) 
                       (let ((name (sxml:text n))
                             (vtext (sxml:text v)))
                         `(define ,($ name) = unknown ,(parse-string-expr vtext))
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

               )

          (match model
                 (($ dynamics-node model-name model-formals model-decls) 
                  (cons (string->symbol node-name)
                        (make-dynamics-node 
                         model-name  
                         model-formals
                         (salt:elaborate
                          (salt:parse
                           (append parameter-decls
                                   state-decls
                                   model-decls))))
                        ))
                 
                 ((and ($ alsys-node model-name model-formals model-decls) node)
                  (cons node-name node))
                 ((and ($ connection-rule-node model-name model-formals model-decls) node)
                  (cons node-name node))
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
            (string->number (sxml:text sxml-value))))
          (sxml-math-expr
           (parse-string-expr (sxml:text sxml-math-expr)))
          (else (error 'eval-ul-property "unknown property format" node))
          ))
      ))
  )



(define (make-prototype-tenv prefix name env)
    (let ((node (lookup-def name env)))

      (if (not node) (error 'make-prototype "unable to find prototype" name))

      (match-let* ((($ dynamics-node name formals eqset) node))

         (let* (
                (sim     (salt:simcreate eqset))
                (hvar    'h)
                (ivar    salt:model-time)
                (dvars   (salt:equation-set-definitions eqset))
                (params  (salt:equation-set-parameters eqset))
                (events  (salt:equation-set-conditions eqset))
                (ic      (salt:equation-set-initial eqset))
                (fields  (salt:equation-set-fields eqset))
                (inputs  (append dvars params fields))
                (outputs dvars)
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
             (ivpFn              . ,name)
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
    (let ((sdinfo (lookup-def name env)))
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



(define (make-plasticity-tenv prefix name env)
    (let ((sdinfo (lookup-def name env)))
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


(define (make-connection-tenv prefix name node-env)

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

   ((lookup-def sys-name node-env) =>
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


(define (eval-ul-group prefix ul-properties node ul-node-env)

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

  (pp `("UL node" . ,node) (current-error-port))

  (let (
        (group-name       (or (sxml:attr node 'name) (gensym 'group)))
	(populations-sxml (sxml:kidsn 'nml:Population node))
	(selections-sxml  (sxml:kidsn 'nml:Selection  node))
	(projections-sxml ((sxpath `(// nml:Projection)) node))
        (properties-sxml  ((sxpath `(// nml:Property)) node))
        (spikerecord-sxml ((sxpath `(// nml:SpikeRecording)) node))
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
                     `(,($ name) . ,(make-population-tenv ($ name) (make-prototype-tenv prefix prototype-name ul-node-env) 
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
             (map
              (lambda (x) 
                (let ((name (car x)))
                  `(,name . ((name . ,name) 
                             (populations . ,(ersatz:sexpr->tvalue (list (cdr x))))
                             (size . ,(alist-ref 'size (cdr x))))
                          )
                  ))
              populations)
             (map
              (lambda (node)
                (let* ((name (sxml:attr node 'name))
                       (set (make-population-set (sxml:kid node) populations)))
                  `(,($ name) . ,(make-population-set-tenv ($ name) set))))
              selections-sxml)))

           (projections+types
            (map
             (lambda (node)
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
                      (del (cdr (eval-ul-property name (sxml:kidn* 'nml:Delay node))))
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
                            (response (and response-name (make-response-tenv prefix response-name response-ports ul-node-env)))
                            (plasticity (and plasticity-name (make-plasticity-tenv prefix plasticity-name ul-node-env)))
                            (connection (and connectivity-name (make-connection-tenv prefix connectivity-name ul-node-env)))
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
  

(define (resolve-ul-components node)

  (let ((components-list (make-parameter '())))

    (let
        (
         (population-template 
          (sxml:match 'nml:Population
                      (lambda (node bindings root env) 
                        (let ((name (sxml:attr node 'name))
                              (number (sxml:kidn 'nml:Number node))
                              (cell-component ((sxpath `(// nml:Cell nml:Component)) node)))
                          (if (null? cell-component) node
                              (let ((component-name (sxml:attr (sxml-singleton cell-component) 'name)))
                                (components-list (cons (sxml-singleton cell-component) (components-list)))
                                `(nml:Population (@ (name ,name))
                                                 ,number (nml:Cell (nml:Reference ,component-name)))
                                ))
                          ))
                      ))
         (projection-template 
          (sxml:match 'nml:Projection
                      (lambda (node bindings root env) 
                        (let* (
                               (name (sxml:attr node 'name))
                               (src  ((sxpath `(// nml:Source)) node))
                               (dest ((sxpath `(// nml:Destination)) node))
                               (del  ((sxpath `(// nml:Delay)) node))
                               (connectivity ((sxpath `(// nml:Connectivity)) node))
                               (connectivity-component ((sxpath `(// nml:Connectivity nml:Component)) node))
                               (response ((sxpath `(// nml:Response)) node))
                               (response-component ((sxpath `(// nml:Response nml:Component)) node))
                               (plasticity ((sxpath `(// nml:Plasticity)) node))
                               (plasticity-component ((sxpath `(// nml:Plasticity nml:Component)) node))
                              )
                          (if (not (null? connectivity-component))
                              (components-list (cons (sxml-singleton connectivity-component) (components-list))))
                          (if (not (null? response-component))
                              (components-list (cons (sxml-singleton response-component) (components-list))))
                          (if (not (null? plasticity-component))
                              (components-list (cons (sxml-singleton plasticity-component) (components-list))))
                          
                          `(nml:Projection (@ (name ,name))
                                           ,(sxml-singleton src)
                                           ,(sxml-singleton dest)
                                           ,(sxml-singleton del)
                                           ,(if (null? connectivity-component)
                                                (sxml-singleton connectivity)
                                                (let ((component-name (sxml:attr (sxml-singleton connectivity-component) 'name)))
                                                  `(nml:Connectivity (nml:Reference ,component-name))))
                                           ,(if (null? response-component)
                                                (sxml-singleton response)
                                                (let ((component-name (sxml:attr (sxml-singleton response-component) 'name)))
                                                  `(nml:Response (nml:Reference ,component-name))))
                                           ,(if (null? plasticity-component)
                                                (sxml-singleton plasticity)
                                                (let ((component-name (sxml:attr (sxml-singleton plasticity-component) 'name)))
                                                  `(nml:Plasticity (nml:Reference ,component-name))))
                                           )
                                ))
                          ))
         )

          (let ((result 
                 (stx:apply-templates 
                  node
                  (sxml:make-identity-ss 
                   population-template
                   projection-template
                   )
                  node (list))))
            
            (values (components-list) result)

            ))
    ))
         

(define (main options operands)

  (if (options 'help) (network:usage))

  (if (null? operands)

      (network:usage)

      (let ((output-type (cond ((options 'output-xml)  'xml)
			       ((options 'output-sxml) 'sxml)
			       (else #f))))

	(if (options 'verbose) 
            (begin
              (salt:verbose 1)
              (network-verbose 1)))

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
                  )

             (let-values (((component-env ul-sxml) (resolve-ul-components (fold append model-sxml ul-import-sxmls))))

               (pp `(ul-sxml . ,ul-sxml) (current-error-port))
               (pp `(component-env . ,component-env) (current-error-port))

               (let* (
                      (ul-properties
                       (parse-ul-properties
                        operand ((sxpath `(// nml:NineML (*or* nml:Property nml:property)))  ul-sxml)))
                      
                      (ul-components
                       (append ((sxpath `(// (*or* nml:Component nml:component)))  model-sxml)
                               component-env))

                      (ul-component-eval-env
                       (map eval-ul-component ul-components))
                      
                      )


                 (d "ul-properties = ~A~%" ul-properties)
                 (d "ul-components = ~A~%" ul-components)
                 
                 (eval-ul-group operand ul-properties `(nml:Group . ,ul-sxml) ul-component-eval-env)
                 
                 ))
             ))

	 operands))
      ))

(main opt (opt '@))


