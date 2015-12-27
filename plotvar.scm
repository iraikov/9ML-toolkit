(use srfi-13 ploticus getopt-long)


(define (plot-var title field-names x-field x-range y-field y-range data-filename output-filename plot-format
                  #!key (maxrows 1000000) (maxfields 4500000) (maxvector 4500000) (cpulimit 90) (textsize 12))

  (init plot-format output-filename)

  (arg "-maxrows"     ,maxrows)
  (arg "-maxfields"   ,maxfields)
  (arg "-maxvector"   ,maxvector)
  (arg "-cpulimit"    ,cpulimit)
  (arg "-textsize"    ,textsize)
  
  (proc "getdata"
        `(("file"            . ,data-filename)
          ("fieldnames"      . ,(string-concatenate (intersperse (map ->string field-names) " ")))
          ))

  (proc "areadef"
        `(("title"           . ,title)
          ("titledetails"    . "size=14  align=C")
          ("rectangle"       . "1 1 8 4")
          ("xrange"          . ,(sprintf "~A ~A" (car x-range) (cdr x-range)))
          ("yrange"          . ,(sprintf "~A ~A" (car y-range) (cdr y-range)))
          ))
  
  
  (proc "yaxis"
        `(("stubs"     . "inc 10")
          ("gridskip"  . "min")
          ))
  
  (proc "curvefit"
        `(("xfield"      . ,(->string x-field))
          ("yfield"      . ,(->string y-field))
          ("linedetails" . "color=red width=.5")
          ("legendlabel" . ,(->string y-field))
          ("maxinpoints" . 300000)
          ))
  
  (proc "curvefit"
        `(("xfield"      . ,(->string x-field))
          ("yfield"      . ,(->string y-field))
          ("linedetails" . "color=blue width=.5")
          ("legendlabel" . ,(->string y-field))
          ("maxinpoints" . 300000)
          ))
  
  (end)
)



(define opt-grammar
  `(

    (title        "plot title"
                  (value (required STRING)
                         (single-char #\t)
                         ))

    (data-filename "name of file containing the data"
                   (value (required FILENAME)
                          (single-char #\d)
                          ))

    (output-filename "name of file containing the output (default is {INPUT-FILENAME}.{PLOT-FORMAT})"
                   (value (required FILENAME)
                          (single-char #\o)
                          ))

    (plot-format "output format (one of eps, png, jpg, svg; default is eps)"
                   (value (required FORMAT)
                          (predicate 
                           ,(lambda (x) 
                              (let ((s (string->symbol (string-downcase x))))
                                (case s
                                  ((png gif x11 svg jpeg eps) s)
                                  (else (error 'plotvar "unrecognized format" x))))))
                          (transformer ,(compose string->symbol string-downcase))
                          (single-char #\p)
                          ))

    (fields       "comma-separated field names"
                  (value (required FIELDN)
                         (transformer ,(lambda (x) (string-split x ",")))
                         )
                     (single-char #\f)
                     )

    (x-field     "name of x-axis data field"
                 (value (required FIELD-NAME))
                 (single-char #\x)
                 )

    (x-range     "colon-separated minimum and maximum of x-axis data field"
                 (value (required FIELD-RANGE)
                        (predicate 
                         ,(lambda (x) 
                            (let ((sl (map (string->number (string-split x ":")))))
                              (cond ((and (pair? sl) (number? (car sl)) (number? (cadr sl))) sl)
                                    (else (error 'plotvar "invalid range" x))))))
                        (transformer ,(lambda (x) 
                                        (let ((sl (map (string->number (string-split x ":")))))
                                          (cons (car sl) (cadr sl)))))
                        ))

    (y-field     "name of x-axis data field"
                 (value (required FIELD-NAME))
                 (single-char #\y)
                 )

    (y-range     "colon-separated minimum and maximum of x-axis data field"
                 (value (required FIELD-RANGE)
                        (predicate 
                         ,(lambda (x) 
                            (let ((sl (map (string->number (string-split x ":")))))
                              (cond ((and (pair? sl) (number? (car sl)) (number? (cadr sl))) sl)
                                    (else (error 'plotvar "invalid range" x))))))
                        (transformer ,(lambda (x) 
                                        (let ((sl (map (string->number (string-split x ":")))))
                                          (cons (car sl) (cadr sl)))))
                        ))


    (help  "Print help"
	    (single-char #\h))
  
  ))

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define opt-defaults
  `(
    (plot-format . eps)
    ))

(define (defopt x)
  (lookup-def x opt-defaults))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (plotvar:usage)
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (width 45)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))


(define (main options)

  (if (options 'help) (plotvar:usage))

  (if (not (and (opt 'data-filename) (opt 'fields) (opt 'x-field) (opt 'y-field)))
      (plotvar:usage))

  (let* (
         (plot-format (or (opt 'plot-format) 
                          (defopt 'plot-format)))
         (data-filename (or (opt 'data-filename) 
                            (defopt 'data-filename)))
         (output-filename (or (opt 'output-filename)
                              (pathname-replace-extension 
                               data-filename
                               (sprintf ".~A" output-format))))
         (field-names (opt 'fields))
         (x-field (opt 'x-field))
         (y-field (opt 'y-field))
         (title (or (opt 'title) (sprintf "Plot of variable ~A" y-field)))
         (x-range (or (opt 'x-range) "xautorange"))
         (y-range (or (opt 'y-range) "yautorange"))
         )
    
    (plot-var title field-names x-field x-range y-field y-range data-filename output-filename plot-format)

    ))

(main opt)

