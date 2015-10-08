(require-extension matchable)
(require-library srfi-1 irregex data-structures files posix extras ploticus)
(import
 (only srfi-1 filter list-tabulate)
 (only files make-pathname)
 (only posix glob)
 (only data-structures ->string alist-ref compose)
 (only extras fprintf random)
 (only mathh cosh tanh log10)
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


(define (raster-plot spike-file plot-label xrange yrange)
  (match-let 

   (
    ((data tmax nmax)
     (fold
      (lambda (spike-file ax)
        (match-let (((data tmax nmax)  ax))
          (let ((data1 (map (lambda (line) (map string->number (string-split  line " ")))
                          (filter (lambda (line) (not (irregex-match comment-pat line)))
                                  (read-lines spike-file)))))
            (let ((t1 (fold (lambda (row ax) (max (car row) ax)) tmax data1))
                  (nmax1 (fold (lambda (row ax) (fold max ax (cdr row))) nmax data1)))
              (list (append data1 data) (max tmax t1) nmax1)
              ))
          ))
        '(() 0.0 0)
        (list spike-file)))
    )

   (print "tmax = " tmax)
   (print "nmax = " nmax)

   (let* (

          (nsample 50)
          (bin-size 0.1)

          ;; spike times per node
          (spike-times
           (let ((v (make-vector nmax '())))
             (for-each (match-lambda
                        ((t . ns)
                         (for-each (lambda (n) (vector-set! v (- n 1) (cons t (vector-ref v (- n 1)))))
                                   ns)))
                       (reverse data))
             v))

          (sampled-spike-times (sample nsample spike-times))

          ;; # spikes in 0.1 ms intervals
          
          (spike-bins 
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

          (nspikes (map (lambda (x) (if (null? x) 0 (length (cdr x)))) (vector->list spike-times)))
          (average-rates (map (lambda (x) (* 1000 (/ x tmax))) nspikes))
          
          (average-firing-frequency (round (/ (fold + 0.0 average-rates) (vector-length spike-times))))

          )

     (print "average firing frequency = " average-firing-frequency)
    
  (let-values (
               ((fd1 temp-path1) (file-mkstemp "/tmp/activity-plot.s1.XXXXXX"))
               ((fd2 temp-path2) (file-mkstemp "/tmp/activity-plot.s2.XXXXXX"))
	       )
	 (file-close fd1)
	 (file-close fd2)

	 (let ((dataport (open-output-file temp-path1)))
           (fold (lambda (ts i) (for-each (lambda (t) (fprintf dataport "~A,~A~%" t i)) ts) (+ 1 i)) 1 sampled-spike-times)
	   (close-output-port dataport))
	 
	 (let ((dataport (open-output-file temp-path2)))
	   (for-each (match-lambda ((tbin nbin) (fprintf dataport "~A,~A~%" tbin nbin))) (reverse spike-bins))
	   (close-output-port dataport))
	 
	 (plot:init 'eps (make-pathname
                          "." 
                          (sprintf "~A_activity.eps" 
                                   (pathname-strip-directory
                                    (pathname-strip-extension spike-file )))))
	 
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
                                             plot-label average-firing-frequency))
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
		  `(;; ("title"     . "Spike # per time bin")
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
;;		    ("yaxis.label"     . "# spikes")
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

(apply raster-plot (command-line-arguments))
