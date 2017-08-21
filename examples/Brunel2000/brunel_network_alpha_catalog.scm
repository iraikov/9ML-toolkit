;;
;; A variant of the network model described in:
;;
;;   Brunel, N (2000) Dynamics of Sparsely Connected Networks of Excitatory and Inhibitory Spiking Neurons.
;;   Journal of Computational Neuroscience 8(3):183--208. doi:10.1023/A:1008925309027.
;;
;; This implementation uses current-based synapses with an
;; alpha-function dynamics, rather than voltage-based delta synapses as
;; in the original paper.
;;


(use srfi-1 utils extras ssax sxpath sxpath-lolevel statistics)
(require-library sxml-transforms)
(import (prefix sxml-transforms sxml:))
(define shared-dir (make-pathname (chicken-home) "9ML"))

(load (make-pathname shared-dir "stx-engine.scm"))
(load (make-pathname shared-dir "SXML-to-XML.scm"))

;; Calculate the height of the EPSP for a synaptic current with peak amplitude 1 nA.
(define (psp-height tau_m R_m tau_syn)
  (let* ((a    (/ tau_m tau_syn))
         (b    (- (/ 1.0 tau_syn) (/ 1.0 tau_m)))
         (x0   (lambert-Wm1 (/ (- (exp (/ -1.0 a))) a)))
         (t_max (* (/ 1.0 b) (- (- x0) (/ 1.0 a)))))
    (let ((x1 (/ 1.0 (* tau_syn tau_m (/ b R_m))))
          (x2 (/ (- (exp (/ (- t_max) tau_m)) (exp (/ (- t_max) tau_syn))) b))
          (x3 (* t_max (exp (/ (- t_max) tau_syn)))))
      (* x1 (- x2 x3)))
    ))

(define (model-name s)
  (let ((cs (string->list (->string s))))
    (let loop ((lst (list)) (cs cs))
      (cond ((null? cs) (list->string (reverse lst)))
	    ((null? (cdr cs))
	     (let ((c (car cs)))
	       (if (or (char-alphabetic? c) (char-numeric? c))
		   (loop (cons c lst) (cdr cs))
		   (loop (append (reverse (string->list (->string (gensym 't)))) lst) (cdr cs))
		   )))
	    (else
	     (let* ((c (car cs))
		    (c1 (cond ((or (char-alphabetic? c) (char-numeric? c) 
				   (char=? c #\_) (char=? c #\#)) c)
			      (else #\_))))
	       (loop (cons c1 lst) (cdr cs))))))))
			    


;; based on SRV:send-reply by Oleg Kiselyov
(define (print-fragments b #!key (out (current-output-port)))
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
       (display (car fragments) out)
       (loop (cdr fragments) #t)))))


(define (Prelude . content)
  `(NineML
    (@ (xmlns "http://nineml.net/9ML/1.0"))
    
    (Dimension (@ (name "time") (t "1")))
    (Dimension (@ (name "frequency") (t "-1") ))
    (Dimension (@ (name "current") (i "1") (k "0") (j "0") (m "0") (l "0") (n "0") (t "0")))
    (Dimension (@ (name "capacitance") (i "2") (l "-2") (m "-1") (t "4" )))
    (Dimension (@ (name "voltage") (i "-1") (t "-3") (m "1") (l "2" )))
    (Dimension (@ (name "resistance") (t "-3") (m "1") (l "2") (i "-2")))

    (Unit (@ (symbol "mV") (dimension "voltage") (power "-3")))
    (Unit (@ (symbol "uF") (dimension "capacitance") (power "-6")))
    (Unit (@ (symbol "Hz") (dimension "frequency") (power "0")))
    (Unit (@ (symbol "nA") (dimension "current") (power "-9")))
    (Unit (@ (symbol "ms") (dimension "time") (power "-3") ))
    (Unit (@ (symbol "Mohm") (dimension "resistance") (power "6")))

    ,@content))


(define (BrunelNetworkAlpha order J g eta)
  (let* (
         (NE       (* 4 order))
         (NI       (* 1 order))
         (epsilon  0.1)
         (theta    20.0)
         (tau      20.0)
         (tau-syn  0.1)
         (tau-rp   2.0)
         (R        1.5)
         (del      1.5)
         (J        J)
         (CE       (* epsilon NE))
         (CI       (* epsilon NI))
         )

    (let* ((scale-factor (psp-height tau R tau-syn))
           (w (/ J scale-factor)) 
           (nu-thr (/ theta (* w CE R tau-syn)))
           (nu-ext (* eta nu-thr))
           (input-rate  (* 1000.0 nu-ext CE)) ;; mean input spiking rate
           )

      `(
        (Selection
         (@ (name "All"))
         (Concatenate
          (Item (@ (index "0")) (Reference "Exc"))
          (Item (@ (index "1")) (Reference "Inh"))))

     (Population
       (@ (name "Exc"))
       (Size ,NE)
       (Cell
         (Component
           (@ (name "nrn"))
           (Definition
             (@ (url "../../neuron/LeakyIntegrateAndFire.xml"))
             "LeakyIntegrateAndFire")
           (Property (@ (units "Mohm") (name "R")) (SingleValue "1.5"))
           (Property
             (@ (units "ms") (name "refractory_period"))
             (SingleValue "2.0"))
           (Property
             (@ (units "ms") (name "tau"))
             (SingleValue "20.0"))
           (Property
             (@ (units "mV") (name "v_reset"))
             (SingleValue "10.0"))
           (Property
             (@ (units "mV") (name "v_threshold"))
             (SingleValue "20.0"))
           (Initial
             (@ (units "ms") (name "refractory_end"))
             (SingleValue "0.0"))
           (Initial
             (@ (units "mV") (name "v"))
             (Component
               (@ (name "uniform_rest_to_threshold"))
               (Definition
                 (@ (url "../../randomdistribution/Uniform.xml"))
                 "UniformDistribution")
               (Property
                 (@ (units "unitless") (name "maximum"))
                 (SingleValue "20.0"))
               (Property
                 (@ (units "unitless") (name "minimum"))
                 (SingleValue "0.0")))))))
     (Projection
       (@ (name "Excitation"))
       (Source (Reference "Exc"))
       (Destination
         (Reference "All")
         (FromResponse
           (@ (send_port "i_synaptic") (receive_port "i_synaptic"))))
       (Connectivity
         (Component
           (@ (name "RandomExc"))
           (Definition
             (@ (url "../../connectionrule/RandomFanIn.xml"))
             "RandomFanIn")
           (Property
             (@ (units "unitless") (name "number"))
             (SingleValue ,CE))))
       (Response
         (Component
           (@ (name "syn"))
           (Definition
             (@ (url "../../postsynapticresponse/Alpha.xml"))
             "Alpha")
           (Property (@ (units "ms") (name "tau")) (SingleValue "0.1"))
           (Initial (@ (units "nA") (name "a")) (SingleValue "0.0"))
           (Initial (@ (units "nA") (name "b")) (SingleValue "0.0")))
         (FromPlasticity
           (@ (send_port "fixed_weight") (receive_port "weight"))))
       (Plasticity
         (Component
           (@ (name "ExcitatoryPlasticity"))
           (Definition (@ (url "../../plasticity/Static.xml")) "Static")
           (Property
             (@ (units "nA") (name "weight"))
             (SingleValue ,w))))
       (Delay (@ (units "ms")) (SingleValue "1.5")))
     (Population
       (@ (name "Ext"))
       (Size ,(+ NI NE))
       (Cell
         (Component
           (@ (name "stim"))
           (Definition (@ (url "../../input/Poisson.xml")) "Poisson")
           (Property
             (@ (units "Hz") (name "rate"))
             (SingleValue ,input-rate))
           (Initial
             (@ (units "ms") (name "t_next"))
             (SingleValue "5.0")))))
     (Projection
       (@ (name "Inhibition"))
       (Source (Reference "Inh"))
       (Destination
         (Reference "All")
         (FromResponse
           (@ (send_port "i_synaptic") (receive_port "i_synaptic"))))
       (Connectivity
         (Component
           (@ (name "RandomInh"))
           (Definition
             (@ (url "../../connectionrule/RandomFanIn.xml"))
             "RandomFanIn")
           (Property
             (@ (units "unitless") (name "number"))
             (SingleValue ,CI))))
       (Response
         (Component
           (@ (name "syn"))
           (Definition
             (@ (url "../../postsynapticresponse/Alpha.xml"))
             "Alpha")
           (Property (@ (units "ms") (name "tau")) (SingleValue "0.1"))
           (Initial (@ (units "nA") (name "a")) (SingleValue "0.0"))
           (Initial (@ (units "nA") (name "b")) (SingleValue "0.0")))
         (FromPlasticity
           (@ (send_port "fixed_weight") (receive_port "weight"))))
       (Plasticity
         (Component
           (@ (name "InhibitoryPlasticity"))
           (Definition (@ (url "../../plasticity/Static.xml")) "Static")
           (Property
             (@ (units "nA") (name "weight"))
             (SingleValue ,(- (* g w))))))
       (Delay (@ (units "ms")) (SingleValue "1.5")))
     (Population
       (@ (name "Inh"))
       (Size ,NI)
       (Cell
         (Component
           (@ (name "nrn"))
           (Definition
             (@ (url "../../neuron/LeakyIntegrateAndFire.xml"))
             "LeakyIntegrateAndFire")
           (Property (@ (units "Mohm") (name "R")) (SingleValue "1.5"))
           (Property
             (@ (units "ms") (name "refractory_period"))
             (SingleValue "2.0"))
           (Property
             (@ (units "ms") (name "tau"))
             (SingleValue "20.0"))
           (Property
             (@ (units "mV") (name "v_reset"))
             (SingleValue "10.0"))
           (Property
             (@ (units "mV") (name "v_threshold"))
             (SingleValue "20.0"))
           (Initial
             (@ (units "ms") (name "refractory_end"))
             (SingleValue "0.0"))
           (Initial
             (@ (units "mV") (name "v"))
             (Component
               (@ (name "uniform_rest_to_threshold"))
               (Definition
                 (@ (url "../../randomdistribution/Uniform.xml"))
                 "UniformDistribution")
               (Property
                 (@ (units "unitless") (name "maximum"))
                 (SingleValue "20.0"))
               (Property
                 (@ (units "unitless") (name "minimum"))
                 (SingleValue "0.0")))))))
     (Projection
       (@ (name "External"))
       (Source (Reference "Ext"))
       (Destination
         (Reference "All")
         (FromResponse
           (@ (send_port "i_synaptic") (receive_port "i_synaptic"))))
       (Connectivity
         (Component
           (@ (name "OneToOne"))
           (Definition
             (@ (url "../../connectionrule/OneToOne.xml"))
             "OneToOne")))
       (Response
         (Component
           (@ (name "syn"))
           (Definition
             (@ (url "../../postsynapticresponse/Alpha.xml"))
             "Alpha")
           (Property (@ (units "ms") (name "tau")) (SingleValue "0.1"))
           (Initial (@ (units "nA") (name "a")) (SingleValue "0.0"))
           (Initial (@ (units "nA") (name "b")) (SingleValue "0.0")))
         (FromPlasticity
           (@ (send_port "fixed_weight") (receive_port "weight"))))
       (Plasticity
         (Component
           (@ (name "ExternalPlasticity"))
           (Definition (@ (url "../../plasticity/Static.xml")) "Static")
           (Property
             (@ (units "nA") (name "weight"))
             (SingleValue ,w))))
       (Delay (@ (units "ms")) (SingleValue "1.5")))
     (Unit (@ (symbol "ms") (power "-3") (dimension "time")))
     (Dimension (@ (t "1") (name "time")))
     (Unit (@ (symbol "Mohm") (power "6") (dimension "resistance")))
     (Dimension (@ (name "dimensionless")))
     (Unit
      (@ (symbol "unitless") (power "0") (dimension "dimensionless"))))))
  )

(define range-g (list-tabulate 16 (lambda (x) (+ 0.5 (* 0.5 x)))))
(define range-eta (list 1.0 2.0 4.0))

(define parameter-sweep
  (concatenate
   (map
    (lambda (g)
      (map
       (lambda (eta)
         `(,(model-name (sprintf "brunel_network_alpha_g~A_eta~A" g eta)) .
           ,(generate-XML
             (Prelude 
              (BrunelNetworkAlpha 10000 0.2 g eta)))))
       range-eta
       ))
    range-g
    ))
  )

(for-each
 (lambda (item)
   (call-with-output-file 
       (string-append (car item) ".xml")
     (lambda  (output)
       (print-fragments (cdr item) out: output))
     ))
 parameter-sweep)

(define variants
  '(
    (SIslow (g . 4.5) (eta . 0.9))
    (AI (g . 5.0) (eta . 2.0))
    (SIfast (g . 6.0) (eta . 4.0))
    (SR (g . 3.0) (eta . 2.0))
    ))
    

(for-each
 (lambda (var)
   (let ((label (car var))
         (g (alist-ref 'g (cdr var)))
         (eta (alist-ref 'eta (cdr var))))
     (call-with-output-file 
         (string-append (model-name (sprintf "brunel_network_alpha_~A" label)) ".xml")
       (lambda  (output)
         (print-fragments
          (generate-XML
           (Prelude 
            (BrunelNetworkAlpha 2500 0.1 g eta)))
          out: output)))
     ))
 variants)
