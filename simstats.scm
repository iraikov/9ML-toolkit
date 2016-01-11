(require-extension matchable getopt-long)
(require-library srfi-1 irregex data-structures files posix extras)
(import
 (only srfi-1 filter list-tabulate)
 (only files make-pathname)
 (only data-structures ->string alist-ref compose)
 (only extras fprintf printf)
 )


(define comment-pat (string->irregex "^#.*"))

(define (diffs xs)
  (fold (match-lambda* ((x (prev lst)) (list x (cons (- x prev) lst))))
        0.0 xs))

(define (sum xs) (fold + 0.0 xs))

(define (mean xs)
  (if (null? xs) 0.0
      (/ (sum xs) (length xs))))

(define (square x) (* x x))

(define (variance xs)
  (if (< (length xs) 2)
      (error "variance: sequence must contain at least two elements")
      (let ((mean1 (mean xs)))
        (/ (sum (map (lambda (x) (square (- mean1 x))) xs))
           (- (length xs) 1)))))


(define (event-stats data-file output-file)
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

   (let* (


          ;; event times per node
          (event-times
           (let ((v (make-vector nmax '())))
             (for-each (match-lambda
                        ((t . ns)
                         (for-each (lambda (n) (vector-set! v (- n 1) (cons t (vector-ref v (- n 1)))))
                                   ns)))
                       (reverse data))
             v))

          (event-intervals (map diffs (vector->list event-times)))
          (mean-event-intervals (map mean event-intervals))
          (mean-event-interval (mean mean-event-intervals))
          (stdev-event-interval (sqrt (variance mean-event-intervals)))

          (nevents (map (lambda (x) (if (null? x) 0 (length (cdr x)))) (vector->list event-times)))
          (mean-rates (map (lambda (x) (* 1000 (/ x tmax))) nevents))
          (mean-event-frequency (round (mean mean-rates)))

          )

     (with-output-to-file output-file
       (lambda ()
         (printf "nmax: ~A" nmax)
         (printf "t max: ~A" tmax)
         (printf "mean event frequency: ~A " mean-event-frequency)
         (printf "mean event interval: ~A " mean-event-frequency)
         (printf "mean event interval: ~A " mean-event-interval)
         (printf "stdev event interval: ~A " stdev-event-interval)
         ))
    
     ))
  )


(define opts    (getopt-long (command-line-arguments) opt-grammar))
(define opt     (make-option-dispatch opts opt-grammar))


(define opt-defaults
  `(
    ))

(define (defopt x)
  (lookup-def x opt-defaults))



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
      (plotraster:usage))

  (let* (
         (data-filename (or (opt 'data-filename) 
                            (defopt 'data-filename)))
         (output-filename (or (opt 'output-filename)
                              (pathname-replace-extension 
                               data-filename
                               (sprintf ".~A" simstats))))
         )
    
    (event-stats data-filename output-filename)
    ))

(main opt)
