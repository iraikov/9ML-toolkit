(require-extension matchable getopt-long)
(require-library srfi-1 irregex data-structures files posix extras ploticus)
(import
 (only srfi-1 filter list-tabulate)
 (only files make-pathname)
 (only data-structures ->string alist-ref compose)
 (only extras fprintf random)
 (prefix ploticus plot:)
 )


(define comment-pat (string->irregex "^#.*"))

(define (sample n v)
  (let ((ub (vector-length v)))
    (list-tabulate n (lambda (i) 
                       (let loop ((idx (random ub)))
                         (let ((v (vector-ref v idx)))
                           (if (null? v)
                               (loop (random ub))
                               v))))))
  )


(define (plot-raster plot-format data-file output-file plot-title xrange yrange)
  (match-let 

   (
    ((data tmax nmax)
     (fold
      (lambda (data-file ax)
        (match-let (((data tmax nmax)  ax))
          (let ((data1 (map (lambda (line) (map string->number (string-split  line " ")))
                          (filter (lambda (line) (not (irregex-match comment-pat line)))
                                  (read-lines data-file)))))
            (let ((t1 (fold (lambda (row ax) (max (car row) ax)) tmax data1))
                  (nmax1 (fold (lambda (row ax) (fold max ax (cdr row))) nmax data1)))
              (list (append data1 data) (max tmax t1) nmax1)
              ))
          ))
        '(() 0.0 0)
        (list data-file)))
    )

   (print "tmax = " tmax)
   (print "nmax = " nmax)

   (let* (

          (nsample 50)
          (bin-size 0.1)

          ;; event times per node
          (event-times
           (let ((v (make-vector nmax '())))
             (for-each (match-lambda
                        ((t . ns)
                         (for-each (lambda (n) (vector-set! v (- n 1) (cons t (vector-ref v (- n 1)))))
                                   ns)))
                       (reverse data))
             v))

          (sampled-event-times (sample nsample event-times))

          ;; # events in 0.1 ms intervals
          
          (event-bins 
           (let recur ((data data) (tbin 0.0) (nbin 0) (bins '()))
             (if (null? data) 
                 (cons (list tbin nbin) bins)
                 (match (car data)
                        ((t . ns)
                         (if (> t tbin)
                             (recur data (+ tbin bin-size) 0 (cons (list tbin nbin) bins))
                             (recur (cdr data) tbin (+ (length ns) nbin) bins)))
                        ))
             ))

          (nevents (map (lambda (x) (if (null? x) 0 (length (cdr x)))) (vector->list event-times)))
          (average-rates (map (lambda (x) (* 1000 (/ x tmax))) nevents))
          
          (average-event-frequency (round (/ (fold + 0.0 average-rates) (vector-length event-times))))

          )

     (print "average event frequency = " average-event-frequency)
    
  (let-values (
               ((fd1 temp-path1) (file-mkstemp "/tmp/plot-raster.s1.XXXXXX"))
               ((fd2 temp-path2) (file-mkstemp "/tmp/plot-raster.s2.XXXXXX"))
	       )
	 (file-close fd1)
	 (file-close fd2)

	 (let ((dataport (open-output-file temp-path1)))
           (fold (lambda (ts i) (for-each (lambda (t) (fprintf dataport "~A,~A~%" t i)) ts) (+ 1 i)) 1 sampled-event-times)
	   (close-output-port dataport))
	 
	 (let ((dataport (open-output-file temp-path2)))
	   (for-each (match-lambda ((tbin nbin) (fprintf dataport "~A,~A~%" tbin nbin))) (reverse event-bins))
	   (close-output-port dataport))
	 
	 (plot:init plot-format output-filename)
	 
	 (plot:arg "-cm" )
	 (plot:arg "-pagesize"   "12,20");;PAPER
	 (plot:arg "-textsize"   "12")
	 (plot:arg "-cpulimit"   "60")
	 (plot:arg "-maxrows"    "700000")
	 (plot:arg "-maxfields"  "1400000")
	 (plot:arg "-maxvector"  "700000")
	 
	 (plot:proc "getdata"
		  `(
;		    ("showdata"   . "yes")
		    ("delim"      . "comma")
		    ("fieldnames" . "xcoord ycoord")
		    ("pathname"   . ,temp-path1)
		    ))
       
	 (plot:proc "areadef"
		  `(("title"     . ,(sprintf "~A (~A Hz)" 
                                             plot-title average-event-frequency))
                    ("titledetails" . "adjust=-0.9,0.2")
		    ("rectangle" . "2 3.5 10 10.5")
;;		    ("rectangle" . "2 5 10 14")
		    ("areacolor" . "white")

		    ("xrange"          . ,xrange)
		    ("xaxis.axisline"  . "no")
		    ("xaxis.tics"      . "no")
;;		    ("xaxis.stubs"     . "inc 50")
;;		    ("xaxis.stubrange" . "0")
;;		    ("xaxis.stubdetails" . "adjust=0,1")

		    ("yrange"          . "0 51")
;;		    ("yaxis.label"     . "Cell #")
		    ("yaxis.axisline"  . "no")
		    ("yaxis.tics"      . "no")
;;		    ("yaxis.stubs"     . "inc 10")
;;		    ("yaxis.stubrange" . "0")
		    )
		  )

       (plot:proc "legendentry"
		  `(("sampletype" .  "color")
		    ("details"    .  "black") 
		    ("tag"        .  "0")
		    ))

       (plot:proc "scatterplot"
		  `(("xfield"    .  "xcoord")
		    ("yfield"    .  "ycoord")
		    ("linelen"   . "0.06")
		    ("linedetails"   . "width=1.2")
		    ("linedir"   . "v")
		    ))

       (plot:proc "getdata"
		  `(
;;	            ("showdata"   . "yes")
		    ("delim"      . "comma")
		    ("fieldnames" . "t count")
		    ("pathname"   . ,temp-path2)
		    ))
       
       (plot:proc "areadef"
		  `(;; ("title"     . "Event # per time bin")
		    ("rectangle" . "2 1 10 3")
;;		    ("rectangle" . "2 1 10 4")
		    ("areacolor" . "white")
                    
		    ("xrange"          . ,xrange)
		    ("xaxis.axisline"  . "no")
		    ("xaxis.tics"      . "no")
		    ("xaxis.stubs"     . "inc 50")
		    ("xaxis.stubrange" . "0")
		    ("xaxis.label"     . "Time [ms]")
		    ("xaxis.labeldistance" . "1.25")
;;		    ("xaxis.stubdetails" . "adjust=0,1")
		    ("yrange"      . ,(sprintf "0 ~A" yrange))
;;                  ("yautorange"      . "datafield=count")
;;		    ("yaxis.label"     . "# events")
;;		    ("yaxis.labeldistance" . "1.5")
		    ("yaxis.axisline"  . "no")
		    ("yaxis.tics"      . "no")
		    ("yaxis.stubs"     . ,(sprintf "inc ~A" (/ (string->number yrange) 5)))
		    )
		  )
		    
       (plot:proc "bars"
		  `(("locfield"    .  "t")
		    ("lenfield"    .  "count")
		    ("thinbarline"    .  "color=gray(0.5)")
                    ))
       
       (plot:end)

       ))
))

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define opt-defaults
  `(
    (plot-format . eps)
    ))

(define (defopt x)
  (lookup-def x opt-defaults))



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
                                  (else (error 'plotraster "unrecognized format" x))))))
                          (transformer ,(compose string->symbol string-downcase))
                          (single-char #\p)
                          ))

    (x-range     "colon-separated minimum and maximum of x-axis data field"
                 (value (required FIELD-RANGE)
                        (predicate 
                         ,(lambda (x) 
                            (let ((sl (map (string->number (string-split x ":")))))
                              (cond ((and (pair? sl) (number? (car sl)) (number? (cadr sl))) sl)
                                    (else (error 'plotraster "invalid range" x))))))
                        (transformer ,(lambda (x) 
                                        (let ((sl (map (string->number (string-split x ":")))))
                                          (cons (car sl) (cadr sl)))))
                        ))

    (y-range     "colon-separated minimum and maximum of x-axis data field"
                 (value (required FIELD-RANGE)
                        (predicate 
                         ,(lambda (x) 
                            (let ((sl (map (string->number (string-split x ":")))))
                              (cond ((and (pair? sl) (number? (car sl)) (number? (cadr sl))) sl)
                                    (else (error 'plotraster "invalid range" x))))))
                        (transformer ,(lambda (x) 
                                        (let ((sl (map (string->number (string-split x ":")))))
                                          (cons (car sl) (cadr sl)))))
                        ))


    (help  "Print help"
	    (single-char #\h))
  
  ))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (plotraster:usage)
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (width 45)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))



(define (main options)

  (if (options 'help) (plotraster:usage))

  (if (not (and (opt 'data-filename) (opt 'x-range) (opt 'y-range)))
      (plotraster:usage))

  (let* (
         (plot-format (or (opt 'plot-format) 
                          (defopt 'plot-format)))
         (data-filename (or (opt 'data-filename) 
                            (defopt 'data-filename)))
         (output-filename (or (opt 'output-filename)
                              (pathname-replace-extension 
                               data-filename
                               (sprintf ".~A" output-format))))
         (title (or (opt 'title) (sprintf "Event raster plot ~A" (pathname-file data-filename))))
         (x-range (or (opt 'x-range) "xautorange"))
         (y-range (or (opt 'y-range) "yautorange"))
         )
    
    (plot-raster plot-format data-filename output-filename title x-range y-range)
    ))

(main opt)
