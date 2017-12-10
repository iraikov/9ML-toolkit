(require-extension matchable getopt-long statistics)
(require-library srfi-1 srfi-13 irregex data-structures files posix extras)
(import
 (only srfi-1 filter list-tabulate)
 (only srfi-13 string-prefix?)
 (only files make-pathname)
 (only data-structures ->string alist-ref compose)
 (only extras fprintf printf)
 )

(define (diffs xs)
  (if (null? xs) '()
      (reverse
       (cadr
        (fold (match-lambda* ((x (prev lst)) (list x (cons (- x prev) lst))))
              (list (car xs) '()) (cdr xs))
        ))
      ))

(define (sum xs) (fold + 0.0 xs))

(define (mean xs)
  (if (null? xs) 0.0
      (/ (sum xs) (length xs))))

(define (square x) (* x x))

(define (variance xs)
  (if (< (length xs) 2)
      (error "variance: sequence must contain at least two elements")
      (let ((mean1 (mean xs)))
        (/ (sum (map (lambda (x) (square (- x mean1))) xs))
           (- (length xs) 1)))))

(define (standard-deviation xs)
  (sqrt (variance xs)))

(define (coefficient-of-variation m s)
  (if (> m 0.0) (/ s m) 0.0))


(define (event-stats data-file output-file)
  (match-let 

   (
    ((data tmin tmax nmax)
     (fold
      (lambda (data-file ax)
        (match-let (((data tmin tmax nmax)  ax))
                   (let ((data1 (filter-map
                                 (lambda (line)
                                   (and (not (string-prefix? "#" line))
                                        (map string->number (string-split  line " "))))
                                 (read-lines data-file))))
                     (let ((tmax1 (fold (lambda (row ax) (max (car row) ax)) tmax data1))
                           (tmin1 (fold (lambda (row ax) (min (car row) ax)) tmin data1))
                           (nmax1 (fold (lambda (row ax) (fold max ax (cdr row))) nmax data1)))
                       (list (append data1 data) (min tmin tmin1) (max tmax tmax1) nmax1)
                       ))
                   ))
      '(() +inf.0 0.0 0)
      (list data-file)))
    )

   (let* (


          ;; event times per node
          (event-times
           (let ((v (make-vector (+ nmax 1) '())))
             (for-each (match-lambda
                        ((t . ns)
                         (for-each (lambda (n)
				     (vector-set! v n (cons t (vector-ref v n ))))
                                   ns)))
                       (reverse data))
             v))

          (event-times-lst (vector->list event-times))
          (event-intervals (map diffs (filter pair? event-times-lst)))
          (all-event-intervals (concatenate event-intervals))
          (mean-event-interval (mean all-event-intervals))
          (stdev-event-interval (standard-deviation all-event-intervals))
          (cv-event-interval (coefficient-of-variation
                              mean-event-interval
                              stdev-event-interval))
          (nevents (filter-map (lambda (x) (and (not (null? x)) (length (cdr x)))) event-times-lst))
          (mean-rates (map (lambda (x) (* 1000 (/ x (- tmax tmin)))) nevents))
          (mean-event-rate (mean mean-rates))
          (mean-event-frequency (/ 1000.0 mean-event-interval))

          )

     (with-output-to-file output-file
       (lambda ()
         (printf "nmax: ~A~%" nmax)
         (printf "t min: ~A~%" tmin)
         (printf "t max: ~A~%" tmax)
         (printf "mean number of events: ~A~%" (mean nevents))
         (printf "mean event frequency: ~A~%" mean-event-rate)
         (printf "mean event interval: ~A~%" mean-event-interval)	 
         (printf "mean event frequency as function of interval: ~A~%" mean-event-frequency)
         (printf "stdev event interval: ~A~%" stdev-event-interval)
         (printf "cv event interval: ~A~%" cv-event-interval)
         ))
    
     ))
  )




(define opt-grammar
  `(

    (data-filename "name of file containing the data"
                   (value (required FILENAME)
                          (single-char #\d)
                          ))

    (output-filename "name of file containing the output (default is {INPUT-FILENAME}.simstats)"
                   (value (required FILENAME)
                          (single-char #\o)
                          ))


    (help  "Print help"
	    (single-char #\h))
  
  ))

(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define opt-defaults
  `(
    ))

(define (defopt x)
  (lookup-def x opt-defaults))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (simstats:usage)
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (width 45)
  (print (parameterize ((indent 5)) (usage opt-grammar)))
  (exit 1))



(define (main options)

  (if (options 'help) (simstats:usage))

  (if (not (opt 'data-filename))
      (simstats:usage))

  (let* (
         (data-filename (or (opt 'data-filename) 
                            (defopt 'data-filename)))
         (output-filename (or (opt 'output-filename)
                              (pathname-replace-extension 
                               data-filename
                               ".simstats")))
         )
    
    (event-stats data-filename output-filename)
    ))

(main opt)
