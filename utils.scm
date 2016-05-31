;;
;;  NineML utilities library.
;;
;;
;; Copyright 2010-2016 Ivan Raikov
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


(module 9ML-utils

	(
         utils-verbose
         fetch parse-xml
         parse-ul-properties
         eval-ul-property
         eval-sxml-units  
         eval-ul-component
         resolve-al-components
         resolve-ul-components
         default-units
         )

	(import scheme chicken )

(require-library files data-structures srfi-1 srfi-13 extras utils data-structures tcp posix ports
                 salt)
(import (only files make-pathname pathname-directory pathname-file)
	(only data-structures conc alist-ref intersperse)
	(only srfi-13 string-concatenate string-trim-both string-null? string-concatenate-reverse )
        (only srfi-1 delete-duplicates drop-right last filter filter-map)
        (only extras pp fprintf read-line read-string)
        (only utils read-all)
        (only data-structures string-split ->string)
        (only tcp tcp-connect)
        (only posix file-mkstemp file-close create-directory)
        (only ports call-with-input-string)
        )
(require-extension matchable datatype ssax sxpath sxpath-lolevel sxml-transforms 
                   unitconv uri-generic irregex setup-api 9ML-types 9ML-parse)

(require-library salt)
(import (prefix salt salt: ))

(define (warn port message . specialising-msgs)
  (print-error-message message (current-output-port) "Warning")
  (print (string-concatenate (map ->string specialising-msgs))))

(include "SXML.scm")
(include "stx-engine.scm")

(define utils-verbose (make-parameter 0))

(define (d fstr . args)
  (let ([port (current-error-port)])
    (if (positive? (utils-verbose)) 
	(begin (apply fprintf port fstr args)
	       (flush-output port) ) )))

(define (safe-car x) (and (pair? x) (car x)))

(define nl "\n")

(define $ string->symbol)
(define (s+ . rest) (string-concatenate (map ->string rest)))

(define (sxml-string->uri s) 
  (let ((ss (string-trim-both s)))
    (uri-reference ss)))

(define (sxml-singleton x)
  (and (pair? x) (car x)))

(define (string-match rx str)
  (and-let* ((m (irregex-match rx str)))
    (let loop ((i (irregex-match-num-submatches m))
               (res '()))
      (if (fx<= i 0)
          (cons str res)
          (loop (fx- i 1) (cons (irregex-match-substring m i) res))))))


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
				      (if (not (file-exists? dest)) (create-directory dest))
				      (d "writing to ~s~%" filepath)
				      (with-output-to-file filepath (cut display data) )
				      filepath))
				  
				  (begin
				    (d "reading data ...~%")
				    (let ([data (read-string #f in)])
				      (close-input-port in)
				      (close-output-port out)
				      (if (not (file-exists? dest)) (create-directory dest))
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



(define (eval-sxml-units dimensions-sxml units-sxml)
    (let* ((dimensions (map parse-sxml-dimension dimensions-sxml))
           (dimensions-env (map (lambda (x) (cons (quantity-name x) x)) dimensions))
           (units (map (lambda (x) (parse-sxml-unit x dimensions-env)) units-sxml))
           (units-env (map (lambda (x) (cons (unit-name x) x)) units)))
      (salt:model-quantities (append dimensions-env (salt:model-quantities)))
      (salt:model-units (append units-env (salt:model-units)))
      ))


(define default-units
  `(
    (current . nA)
    ))


(define (eval-ul-component x al-component-env ul-component-env) 

  (define (eval-initial x)
    (let* ((val (sxml:kidn 'nml:SingleValue x))
           (ref (sxml:kidn 'nml:Reference x))
           (refname (and ref (string->symbol (sxml:text ref)))))
      (cond (refname
             (let ((eval-ref
                    (eval-ul-component (alist-ref refname ul-component-env)
                                       al-component-env '())))
               (match eval-ref
                      ((_ . ($ random-dist-node model-name model-formals model-stdlib))
                       (let ((random-op
                              (case (car model-stdlib)
                                ((http://www.uncertml.org/distributions/uniform)
                                 'random.unifrange)
                                ((http://www.uncertml.org/distributions/exponential)
                                 'random.exponential)
                                ((http://www.uncertml.org/distributions/normal)
                                 'random.normal)
                                ((http://www.uncertml.org/distributions/poisson)
                                 'random.poisson)
                                (else
                                 (error 'eval-ul-component "unknown random distribution" (car model-stdlib))))
                              ))
                         (let ((expr `(,random-op ,(intersperse (map cdr (cdr model-stdlib)) '~))))
                           expr)
                         ))
                      (else (error 'eval-ul-component "unknown component type in initial element" x)))
               ))
            (val 
             (let ((vtext (sxml:text val)))
               (parse-string-expr vtext)))
            (else 
             (error 'eval-ul-component "initial element without value element" x)))
      ))


  (let (
        (node-name  (sxml:attr x 'name))
        (definition ((sxpath `(// (*or* nml:Definition nml:definition)))  x))
	(props      ((sxpath `(// (*or* nml:Property nml:property)))  x))
	(fieldns    ((sxpath `(// (*or* nml:Field nml:field) @ name))  x))
	(fieldvs    ((sxpath `(// (*or* nml:Field nml:field) nml:SingleValue))  x))
	(initials   ((sxpath `(// (*or* nml:Initial nml:initial)))  x))
        (ivp        (safe-car ((sxpath `(// nml:IVP))  x)))
        )

    (d "NineML user layer XML: ~A~%" x)
    (d "NineML user layer definition: ~A~%" definition)
    (if (null? definition)
	(error 'eval-ul-component "component without definition" x))

    (let (
          (al-definition-name (string->symbol (sxml:text (safe-car definition))))
          (uri (let ((str (sxml:attr (safe-car definition) 'url)))
                 (and str (sxml-string->uri str))))
          )
      
      (let* (
             (model-src (and uri (fetch uri)))
             (model-sxml (if (not model-src)
                             (or (alist-ref al-definition-name al-component-env)
                                 (error 'eval-ul-component "resource not found" al-definition-name))
                             (parse-xml model-src)))
             (dd     (d "NineML abstraction layer model XML: ~A~%" model-sxml))

             (model-decls (parse-al-sxml model-sxml))
             (dd     (d "NineML abstraction layer model declarations: ~A~%" model-decls))

             (model-dimensions-sxml ((sxpath `(// nml:NineML nml:Dimension)) model-sxml))
             (model-units-sxml ((sxpath `(// nml:NineML nml:Unit)) model-sxml))

             (units-env (eval-sxml-units model-dimensions-sxml model-units-sxml))

             (model-env (map (match-lambda
                              (($ dynamics-node model-name model-formals model-env model-decls) 
                               (cons model-name (make-dynamics-node model-name model-formals model-env
                                                                    (salt:parse model-decls))))
                              ((and ($ alsys-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              ((and ($ connection-rule-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              ((and ($ random-dist-node model-name model-formals model-decls) node)
                               (cons model-name node))
                              (node (error 'eval-ul-component "unknown node type" node)))
                             model-decls))

             (dd     (d "NineML abstraction layer models: ~A~%" model-env))
             (model  (alist-ref al-definition-name model-env))
             (dd     (d "NineML abstraction layer model intermediate form: ~A~%" model))

             (initialns  (map (lambda (x) 
                                (let ((name (sxml:attr x 'name)))
                                  (if (not name)
                                      (error 'eval-ul-component "initial element without name attribute" x))
                                  name))
                              initials))
             (initialvs  (map eval-initial initials))
             (initialunits  (map (lambda (x) (sxml:attr x 'units)) initials))
             
             (propns  (map (lambda (x) 
                             (let ((name (sxml:attr x 'name)))
                               (if (not name)
                                   (error 'eval-ul-component "property element without name attribute" x))
                               name))
                           props))
             (propvs  (map (lambda (x) 
                                (let ((val (sxml:kidn 'nml:SingleValue x)))
                                  (if (not val)
                                      (error 'eval-ul-component "property element without value element" x))
                                  val))
                              props))
             (propunits  (map (lambda (x) (sxml:attr x 'units)) props))
             (prop-env   (map cons propns propvs))

             (dd (begin
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
                   ))

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
             (model-analog-send-ports
              (map (lambda (x) 
                     (let ((name (string->symbol (sxml:attr x 'name)))
                           (dim (sxml:attr x 'dimension)))
                       (if dim
                           `(,name . ,(string->symbol dim))
                           `(,name . Unity))))
                   ((sxpath `(// nml:AnalogSendPort)) model-sxml)))
             )

        (if (not model)
            (error 'eval-ul-component "cannot find definition named" al-definition-name))
        
        (let* (
               (parameter-decls
                  (filter-map
                   (lambda (n v u) 
                     (let* ((vtext (sxml:text v))
                            (name (string->symbol n))
                            (unit (and u (string->symbol u)))
                            (dim  (alist-ref name model-parameters)))
                       (and dim
                            (if unit
                                `(define ,name = parameter (dim ,dim) (,(parse-string-expr vtext) * ,unit))
                                `(define ,name = parameter ,(parse-string-expr vtext))))
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
                       (let* ((name (string->symbol n))
                              (unit (and u (string->symbol u)))
                              (dim  (alist-ref name model-variables))
                              (ty (if (member name (alist-ref 'ode-states (dynamics-node-env model)))
                                      'unknown 'discrete))
                              )
                         (if (and unit dim)
                             `(define ,name = ,ty (dim ,dim) (,v * ,unit))
                             `(define ,name = ,ty ,v))
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
                     model-analog-receive-ports
                     ))
                 
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
                       (let* ((unit (alist-ref dim default-units ))
                              (val (or (alist-ref name prop-env) 
                                       `(0.0 * ,unit))))
                         (if (not unit) 
                             (error 'eval-ul-component
                                    "cannot find default unit for dimension in reduce port definition"
                                    dim name))
                         `(define ,name = unknown (dim ,dim) ,val))))
                     model-reduce-ports
                     ))

               )

          (match model
                 (($ dynamics-node model-name model-formals model-env model-decls) 
                  (let ((decls (append parameter-decls
                                       state-decls
                                       ext-decls
                                       extev-decls
                                       ;reduce-decls
                                       (list model-decls))))
                    ;;(pp `(model-decls = ,decls) (current-error-port))
                    (cons (string->symbol node-name)
                          (make-dynamics-node 
                           model-name model-formals model-env
                           (salt:parse decls)
                           ))
                    ))
                 
                 ((and ($ alsys-node model-name model-formals model-decls) node)
                  (cons (string->symbol node-name) node))
                 ((and ($ connection-rule-node model-name model-formals model-stdlib) node)
                  (let ((parameters
                          (map (lambda (n v) 
                                 (let* ((vtext (sxml:text v))
                                        (name (string->symbol (if (string? n) n (sxml:text n)))))
                                   `(,name . ,(string->number vtext))))
                               propns propvs)))
                  (cons (string->symbol node-name) 
                        (make-connection-rule-node model-name model-formals 
                                                   (cons model-stdlib parameters)))
                  ))
                 ((and ($ random-dist-node model-name model-formals model-stdlib) node)
                  (let ((parameters
                          (map (lambda (n v) 
                                 (let* ((vtext (sxml:text v))
                                        (name (string->symbol (if (string? n) n (sxml:text n)))))
                                   `(,name . ,(string->number vtext))))
                               propns propvs)))
                  (cons (string->symbol node-name) 
                        (make-random-dist-node model-name model-formals 
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

               (let ((n (string->number sxml-value)))
                 (or (and n (make-signal-expr n))
                     (and
                      (sxml:kidn 'nml:MathInline sxml-value)
                      (make-signal-expr
                       (parse-string-expr (->string (sxml:kidn-cadr 'nml:MathInline sxml-value )))
                       '()))
                     ))
               ))
          '() sxml-properties)))
        )

    prop-env
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


(define (rename-component component name)
  (let ((kids (sxml:kids component)))
    `(nml:Component (@ (name ,name)) . ,kids)))


(define (resolve-ul-components node)
  
  (let ((components-list (make-parameter '())))

    (let*
        (
         (initial-template 
          (lambda (scope-name)
            (sxml:match 'nml:Initial
                        (lambda (node bindings root env) 
                          (let ((name (sxml:attr node 'name))
                                (units (sxml:attr node 'units))
                                (init-component ((sxpath `(// nml:Component)) node)))
                            (if (null? init-component) node
                                (let ((component-name (sprintf "~A_~A_~A" scope-name name (sxml:attr (sxml-singleton init-component) 'name))))
                                  (components-list
                                   (cons `(,(string->symbol component-name) .
                                           ,(rename-component (sxml-singleton init-component) component-name))
                                         (components-list)))
                                  `(nml:Initial (@ (name ,name) (units ,units)) (nml:Reference ,component-name))
                                  ))
                            ))
                        ))
          )
         (component-template 
          (sxml:match 'nml:Component
                      (lambda (node bindings root env) 
                        (let ((name (sxml:attr node 'name)))
                          (components-list
                           (cons `(,(string->symbol name) . ,node)
                                 (components-list)))
                          node))
                      ))
                          
         (population-template 
          (sxml:match 'nml:Population
                      (lambda (node bindings root env) 
                        (let ((name (sxml:attr node 'name))
                              (size (sxml:kidn 'nml:Size node))
                              (cell-component ((sxpath `(// nml:Cell nml:Component)) node)))
                          (let ((cell-component1 
                                 (if (null? cell-component) '()
                                     `(nml:Component (@ (name ,(sxml:attr (sxml-singleton cell-component) 'name)))
                                       . ,(stx:apply-templates 
                                           (sxml:kids (sxml-singleton cell-component))
                                           (sxml:make-identity-ss 
                                            (initial-template name))
                                           (sxml-singleton cell-component) 
                                           (list))))))
                            (if (null? cell-component) node
                                (let ((component-name (sprintf "~A_~A" name (sxml:attr cell-component1 'name))))
                                  (components-list (cons  `(,(string->symbol component-name) .
                                                            ,(rename-component cell-component1 component-name))
                                                         (components-list)))
                                  `(nml:Population (@ (name ,name))
                                                   ,size (nml:Cell (nml:Reference ,component-name)))
                                  ))
                            ))
                        ))
          )
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
                          `(nml:Projection (@ (name ,name))
                                           ,(sxml-singleton src)
                                           ,(sxml-singleton dest)
                                           ,(sxml-singleton del)
                                           ,(if (null? connectivity-component)
                                                (sxml-singleton connectivity)
                                                (let ((component-name (sprintf "~A_~A" name (sxml:attr (sxml-singleton connectivity-component) 'name))))
                                                  (components-list 
                                                   (cons `(,(string->symbol component-name)
                                                           . ,(rename-component (sxml-singleton connectivity-component) component-name))
                                                         (components-list)))
                                                  `(nml:Connectivity (nml:Reference ,component-name)
                                                                     . ,((select-kids (lambda (x) (not (eq? (car x) 'nml:Component)))) connectivity))))
                                           ,(if (null? response-component)
                                                (sxml-singleton response)
                                                (let ((component-name (sprintf "~A_~A" name (sxml:attr (sxml-singleton response-component) 'name))))
                                                  (components-list (cons `(,(string->symbol component-name)
                                                                           . ,(rename-component (sxml-singleton response-component) component-name))
                                                                         (components-list)))
                                                  `(nml:Response (nml:Reference ,component-name)
                                                                   . ,((select-kids (lambda (x) (not (eq? (car x) 'nml:Component)))) response))
                                                  ))
                                           ,(if (null? plasticity-component)
                                                (sxml-singleton plasticity)
                                                (let ((component-name (sprintf "~A_~A" name (sxml:attr (sxml-singleton plasticity-component) 'name))))
                                                  (components-list
                                                   (cons `(,(string->symbol component-name)
                                                           . ,(rename-component (sxml-singleton plasticity-component) component-name))
                                                         (components-list)))
                                                  `(nml:Plasticity (nml:Reference ,component-name)
                                                                   . ,((select-kids (lambda (x) (not (eq? (car x) 'nml:Component)))) plasticity))))
                                           ))
                        ))
          ))

          (let ((result 
                 (stx:apply-templates 
                  node
                  (sxml:make-identity-ss 
                   component-template
                   population-template
                   projection-template
                   )
                  node (list))))
            
            (values (reverse (components-list)) result)

            ))
    ))


(define (resolve-al-components node)

  (let ((components-env (make-parameter '())))

    (let (
          (component-template 
           (sxml:match 'nml:ComponentClass
                       (lambda (node bindings root env) 
                         (let ((component-name (string->symbol (sxml:attr node 'name))))
                           (components-env (cons (cons component-name `(nml:NineML ,node)) (components-env)))
                           node
                           ))
                       ))
          )
      
      (let ((result 
             (stx:apply-templates 
              node
              (sxml:make-identity-ss 
               component-template
               )
              node (list))))
        
        (components-env) 
        
        ))
    ))


)
