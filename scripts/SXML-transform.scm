
(import files setup-api srfi-1 srfi-4 srfi-13 srfi-69)
(require-extension datatype matchable ssax sxml-transforms sxpath sxpath-lolevel )



(define (warn port message . specialising-msgs)
  (print-error-message message (current-output-port) "Warning")
  (print (string-concatenate (map ->string specialising-msgs))))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (current-error-port)) (display x (current-error-port))))
            args))

(define (stx:error . messages)
  (cerr nl "STX: ")
  (apply cerr messages)
  (cerr nl)
  (exit -1))

  
;; obtain all non-attribute children of a node
(define (sxml:kids node)
  ((select-kids (lambda (x) (not (eq? (car x) '@)))) node))


;; obtain all children of a node named n
(define (sxml:kidsn name node)
  ((select-kids (lambda (x) (eq? (car x) name))) node))

;; obtain  child named n of a node
(define (sxml:kidn name node)
  ((select-first-kid (lambda (x)  (eq? (car x) name))) node))

;; obtain  non-empty child named n of a node
(define (sxml:kidn* name node)
  ((select-first-kid (lambda (x) (and (eq? (car x) name) (not (null? (cdr x)))))) node))

;; obtain  the cdr of child named n
(define (sxml:kidn-cdr name node)
  (let ((v ((select-first-kid (lambda (x)  (eq? (car x) name))) node)))
    (if (not v)  (error 'sxml:kidn-cdr "node does not have children" node)  (cdr v))))
  
  
;; obtain  the cadr of child named n
(define (sxml:kidn-cadr name node)
  (let ((v ((select-first-kid (lambda (x) (eq? (car x) name))) node)))
    (if (not v)  (error 'sxml:kidn-cadr "node does not have children" node)  (cadr v))))


(define null-template 
  `(*default* ,(lambda (node bindings root env) 
		 (begin 
		   (warn "Unrecognized input element:" node)
		   '()))))


(define-syntax  sxml:make-null-ss
   (syntax-rules  ()
      ((stx rule ...)
       (list 
	; default handler
	null-template
	; handler for textual nodes
	(list '*text*  (lambda (text) text)) 
	rule ...))))

(define identity-template 
  `(*default* ,(lambda (node bindings root env) 
                 (begin 
                   node))))

(define-syntax sxml:make-ss
  (syntax-rules  ()
    ((stx rule ...)
     (list 
      identity-template
      (list '*text*  (lambda (text) text)) 
      rule ...))
    ))


;------------------------------------------------------------------------------
; These macros provide support for abbreviated stylesheets:
;
;  <Stylesheet> ::= (stx:stylesheet <Template>+)
;  <Template>   ::= (match <SXPath> <Handler>)
;  <SXPath>     ::= SXPath expression
;  <Handler>    ::= (lambda (current-node stx:templates current-root $) ...)
; 
; For example:
;  (stx:stylesheet
;    (match "//element[state/@condition='standard']"
;	   (lambda (current-node stx:templates current-root $)
;	     (sxml:text current-node)))
;    (match (table (tr 4))
;	   (lambda (current-node stx:templates current-root $)
;	     `(ol
;		,@(map 
;		    (lambda(x) `(li ,x))
;		,((sxpath '(td *text*)) current-node))))))

(define-syntax  sxml:stylesheet
   (syntax-rules  ()
 		 ((stx rule ...)
		  (list 
		    ; default handler
		    (list '*default* 
			  (lambda (node bindings root environment)
			     (stx:apply-templates (sxml:content node) 
						  bindings 
						  root environment)
			     ))
		    ; handler for textual nodes
		    (list '*text* 
			  (lambda(text) text)) 
		    rule ...))))

(define-syntax  sxml:match
   (syntax-rules  ()
 		 ((match pattern handler)
		   (list (if (symbol? pattern) pattern (sxpath pattern))
			   handler))
		 ))


;=============================================================================
; Tree transformation

; stx:apply-templates:: <tree> x <templates> x <root> x <environment> -> <new-tree>
; where
; <templates> ::= <default-template> <text-template> <template>*
; <default-template> ::= (*default* . <handler>)
; <text-template> ::= (*text* . <handler>)
; <template>  ::= (<matcher> <handler>) | ( XMLname <handler>)
; <root>     ::= <document-root>
; <environment> ::= <lambda-tuple>
; <matcher>  ::= <node> <root> -> <nodeset>
; <handler> :: <node> <templates> <root> <environment> -> <new-node>
;
; The stx:apply-templates function visits top-level nodes of a given tree and 
; process them in accordance with a list of templates given. 
; If a node is a textual one then it is processed usind 'text-template',
; which has to be second element in given list of templates. 
; If a node is a pair then stx:apply-templates looks up a corresponding template
; among  given <templates> using stx:find-template function. 
; If failed, stx:apply-templates tries to locate a *default* template, 
; which has to be first element in given list of templates. It's an
; error if this latter attempt fails as well.  
; Having found a template, its handler is applied to the current node. 
; The result of the handler application, which should
; also be a <tree>, replaces the current node in output tree.
;
; This function is slightly similar to Oleg Kiselyov's "pre-post-order" function
; with *preorder* bindings. 
(define (stx:apply-templates tree templates root environment)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (stx:apply-templates a-tree templates root environment)) 
	  tree))
    ((pair? tree) 
     (cond
       (;(tee-4 "Template: " 
	(stx:find-template tree 
		      (cddr templates) ; *default* and *text* skipped
		      root);) 
	=> (lambda (template) 
	     ((cadr template) tree templates root environment)))
       (else 
	 (if (eq? '*default* (caar templates))
	   ((cadar templates) tree templates root environment)
	   (stx:error "stx:apply-templates: There is no template in: " templates
		      nl "for: " tree
		      )) 
	 )))
    ((string? tree) ; for *text* , simple speed-up - just return 'tree' 
	 (if (eq? '*text* (caadr templates))
	   ((cadadr templates) tree)
	   (stx:error "stx:apply-templates: There is no *text* templates for: " 
		      templates))) 
    (else (stx:error "Unexpected type of node: " tree))))

;  stx:find-template: <node> x <templates> x <root> -> <template>
;  This function returns first template in <templates> whouse <matcher>
;  matches given <node>
;  <matcher> matches node if:
;    - if it is a symbol and its the same as the name of the node matched
;    - if it is a procedure (sxpath/txpath generated one) then it is 
;     applyed (with respect to given <root>) sequentially to the matched node 
;     and its parents until the matched node is a member of a resulting nodeset 
;     or root node is reached. In the first case the node matches successfuly, 
;     in the second case it does not. 
(define (stx:find-template node templates root)
  (let ((pattern-matches? 
	  (lambda (node pattern-test) 
	    (let rpt ((context-node node))
	      (cond 
		((null? context-node) #f)
 ;		((memq node (pattern-test context-node root '()))
		((memq node (pattern-test context-node `((*root* . ,root))))
		 #t)
		(else ; try PARENT
		  (rpt ((sxml:node-parent root) context-node))))))))  
    (let rpt ((bnd templates)) 
      (cond ((null? bnd) #f)
	    ((and (symbol? (caar bnd)) (eq? (caar bnd) (car node)))
	     (car bnd))
	    ((and (procedure? (caar bnd)) ; redundant?
		  (pattern-matches? node (caar bnd)))
	     (car bnd))
	    (else (rpt (cdr bnd)))))))


(define nl #\newline)

(define (sxml-transform content rulesets)
   (fold (lambda (ruleset content)
		     (sxml:pre-post-order* content ruleset))
	 content rulesets))


(define (generate-XML content #!key (rulesets '()) (protect #f))
  (let*
      (
       (content (fold (lambda (ruleset content)
			(sxml:pre-post-order* content ruleset))
		      content rulesets))
       )

   (post-order content
   `(
     ,@(if protect universal-protected-rules universal-conversion-rules)

     (begin
      . ,(lambda (tag . body)
	   (list "<?xml version=\"1.0\" ?>" nl
		  body
		  nl)))
 
     ; Description of the Resources
     ; A resource is declared by a SXML element
     ;	(Resource name title date version)
     ; where name is a filename for a document resource,
     ; name of an element or an attribute
     ; The Resource element by itself does not generate any
     ; XML. It is used as a container of information about the
     ; resource. This information is when evaluating Resource-ref
     ; and Resource-descr elements.

     (Resource
      . ,(lambda (tag name title date version)
	   '()))		; null expansion


     
     (Described-by	; (Described-by res-name) 
      . ,(lambda (tag name)	; A higher-order tag
	   `(DescribedBy
	     (Resource-ref ,name))))

     (DescribeDoc	; Describe a document resource
      . ,(lambda (tag name)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeDocument
	       (InformationResourceLocation ,name)
	       )))
	   ))

     (DescribeSample	; Describe an XML sample doc
      . ,(lambda (tag name . relations)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLSample
	       (InformationResourceLocation ,name)
	       (Relationships
		,@relations))))
	   ))

     (DescribeDTD	; Describe a DTD
      . ,(lambda (tag name . relations)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLschema
	       (@ (schemaType "DTD"))
	       (InformationResourceLocation ,name)
	       (Relationships
		,@relations))))
	   ))

     (XMLElement	; Describe an XML element
      . ,(lambda (tag name content descr-by . attlist)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLElement
	       ,content
	       (Relationships
		,@attlist
		(DescribedBy
		 (Resource-ref ,descr-by))
		))))))

     (XMLSpecFor
      . ,(lambda (tag name lit-version)
	   (list 'IsXMLSpecFor
	    '(Namespace "MET")
	    (list 'InformationResourceName name)
	    (list 'InformationResourceVersion lit-version))))

     ; Describe datatypes of elements and attributes
     (DTString		; A string datatype
      . ,(lambda (tag length)
	   `(DataTypeString (StringLength ,length))))

     (DTInt		; An int datatype
      . ,(lambda (tag length unit)
	   `(DataTypeInteger (IntegerLength ,length)
			     (IntegerUnitMeasure ,unit))))

     (DTFloat		; A floating-point datatype
      . ,(lambda (tag length precision unit)
	   `(DataTypeFloat (FloatLength ,length)
			   (FloatPrecision ,precision)
			   (FloatUnitMeasure ,unit))))


     (DTContainer	; A container of other elements
      . ,(lambda (tag . elem-names)
	   (cons 'DataTypeContainer
		 (map 
		  (lambda (elem-name) 
		    (list 'Contains
			  (list 'Resource-ref elem-name)))
		  elem-names))))

     (Attlist
      . ,(lambda (tag . attr-names)
	   (map (lambda (attr-name) 
		  (list 'IsQualifiedByAttribute
			(list 'Resource-ref attr-name)))
	   attr-names)))


     (XMLAttr	; default-value may be #f if omitted
      . ,(lambda (tag name content default-value descr-by)
	   (generate-XML
	    `(AddTransaction
	      (Resource-descr ,name)
	      (InformationResourceTypeXMLAttribute
	       ,content
	       ,(and default-value (list 'DefaultValue default-value))
	       (Relationships
		(DescribedBy
		 (Resource-ref ,descr-by))
		))))))

  ))))


;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b)
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
       (display (car fragments))
       (loop (cdr fragments) #t)))))



(define (nineml-apply-templates nineml:model)
  (letrec (
           (property-template 
            (sxml:match 'nml:Property
                        (lambda (node bindings root env) 
                          (let (
                                (label  (sxml:kidn* 'nml:label node))
                                (value  (sxml:kidn* 'nml:value node))
                                (unit   (sxml:kidn* 'nml:unit node))
                                )
                            (if unit
                                `(Property (@ (name ,(cdr label)) (unit ,(cdr unit))) (Quantity (SingleValue ,(cdr value))))
                                `(Property (@ (name ,(cdr label))) (Quantity (SingleValue ,(cdr value)))))
                            ))
                        ))

           (initial-template 
            (sxml:match 'nml:Initial
                        (lambda (node bindings root env) 
                          (let (
                                (label  (sxml:kidn* 'nml:label node))
                                (value  (sxml:kidn* 'nml:value node))
                                (unit   (sxml:kidn* 'nml:unit node))
                                )
                            (if unit
                                `(Initial (@ (name ,(cdr label)) (unit ,(cdr unit))) (Quantity (SingleValue ,(cdr value))))
                                `(Initial (@ (name ,(cdr label))) (Quantity (SingleValue ,(cdr value)))))
                            ))
                        ))

           (field-template 
            (sxml:match 'nml:Field
                        (lambda (node bindings root env) 
                          (let (
                                (label  (sxml:kidn* 'nml:label node))
                                (value  (sxml:kidn* 'nml:value node))
                                (unit   (sxml:kidn* 'nml:unit node))
                                )
                            (if unit
                                `(Field (@ (name ,(cdr label)) (unit ,(cdr unit))) (Quantity (SingleValue ,(cdr value))))
                                `(Field (@ (name ,(cdr label))) (Quantity (SingleValue ,(cdr value)))))
                            ))
                        ))

           (component-template
            (sxml:match 'nml:Component
                        (lambda (node bindings root env) 
                          (let (
                                (definition (sxml:kidn* 'nml:Definition node))
                                (properties (sxml:kidsn 'nml:Property node))
                                (initials   (sxml:kidsn 'nml:Initial node))
                                (fields     (sxml:kidsn 'nml:Field node))
                                )
                            `(Component
                              ,(let ((link (sxml:kidn* 'nml:link definition))
                                     (name (sxml:attr definition 'name)))
                                 `(Definition (@ (url ,link) (name ,name))))
                              ,@(stx:apply-templates 
                                 properties
                                 (sxml:make-ss property-template)
                                 root env)
                              ,@(stx:apply-templates 
                                 initials
                                 (sxml:make-ss initial-template)
                                 root env)
                              ,@(stx:apply-templates 
                                 fields
                                 (sxml:make-ss field-template)
                                 root env)
                              )
                            ))
                        ))
           
           )

    (stx:apply-templates 
     nineml:model 
     (sxml:make-ss 
      property-template
      component-template
      )
      nineml:model (list))

    ))


(define nineml-xmlns-base "http://nineml.incf.org/9ML/")

(define (read-xml name) 
  (call-with-input-file name
    (lambda (in) 
      (ssax:xml->sxml in
                      `((nml . ,(string-append nineml-xmlns-base "0.2"))
                        (nml . ,(string-append nineml-xmlns-base "0.3"))
                        (nml . ,(string-append nineml-xmlns-base "1.0"))
                        )))
                      ))

(define (main operands)
  (for-each
   (lambda (operand)
     (let* ((sxml (read-xml operand))
            (nineml:model (sxml:kidn* 'nml:NineML sxml))
            (nineml:model1 
             `(begin
                (NineML
                 (@ (xmlns ,(string-append nineml-xmlns-base "1.0"))
                    (xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance")
                    (xsi:schemaLocation "http://nineml.incf.org/9ML/1.0 ../NineML_v1.0.xsd")
                    (name ,(sxml:attr 'name nineml:model)))
                 ,(nineml-apply-templates (sxml:kids nineml:model))))))
       (print-fragments (generate-XML nineml:model1))))
   operands))

                   

(main (command-line-arguments))

