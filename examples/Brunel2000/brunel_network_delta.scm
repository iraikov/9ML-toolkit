;;
;; A variant of the network model described in:
;;
;;   Brunel, N (2000) Dynamics of Sparsely Connected Networks of Excitatory and Inhibitory Spiking Neurons.
;;   Journal of Computational Neuroscience 8(3):183--208. doi:10.1023/A:1008925309027.
;;
;; This implementation uses current-based delta synapses.
;;

(use srfi-1 utils extras ssax sxpath sxpath-lolevel )
(require-library sxml-transforms sxml-serializer)
(import (prefix sxml-transforms sxml:)
        (prefix sxml-serializer sxml:))
(define shared-dir (make-pathname (chicken-home) "9ML"))


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
			    



(define (Prelude . content)
  `(*TOP* 
    (NineML
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
     
     . ,content))
  )


(define (BrunelNetworkDelta g eta)
  (let* ((order    2500)
         (NE       (* 4 order))
         (NI       (* 1 order))
         (epsilon  0.1)
         (theta    20.0)
         (tau      20.0)
         (tau-rp   2.0)
         (R        1.5)
         (del      1.5)
         (J        0.1)
         (JE       (/ J R))
         (JI       (* (- g) JE))
         (CE       (* epsilon NE))
         (CI       (* epsilon NI))
         (nu-thr   (/ theta (* J CE tau)))
         (nu-ext   (* eta nu-thr))
         (p-rate   (* 1000.0 nu-ext CE))
         )
    `(
      (Population
       (@ (name "Exc"))
       (Size ,NE)
       (Cell
        (Component
         (@ (name "nrn"))
         (Definition (@ (url "BrunelIaFdelta.xml")) "BrunelIaFdelta")
         (Property (@ (units "Mohm") (name "R")) (SingleValue ,R))
         (Property (@ (units "mV") (name "Vreset")) (SingleValue "10.0"))
         (Property (@ (units "ms") (name "tau")) (SingleValue ,tau))
         (Property (@ (units "ms") (name "tau_rp")) (SingleValue ,tau-rp))
         (Property (@ (units "mV") (name "theta")) (SingleValue ,theta))
         (Initial (@ (units "mV") (name "V")) (SingleValue "0.0"))
         (Initial
          (@ (units "ms") (name "t_rpend"))
          (SingleValue "0.0")))))
      (Projection
       (@ (name "Excitation"))
       (Source (Reference "Exc"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "fixed_weight") (receive_port "q"))))
       (Connectivity
        (Component
         (@ (name "RandomExc"))
         (Definition (@ (url "RandomFanIn.xml")) "RandomFanIn")
         (Property (@ (name "number")) (SingleValue ,(inexact->exact CE)))))
       (Plasticity
        (Component
         (@ (name "ExcitatoryPlasticity"))
         (Definition (@ (url "Static.xml")) "Static")
         (Property
          (@ (units "nA") (name "weight"))
          (SingleValue ,JE))))
       (Delay (@ (units "ms")) (SingleValue ,del)))
      (Selection
       (@ (name "All neurons"))
       (Concatenate
        (Item (@ (index "0")) (Reference "Exc"))
        (Item (@ (index "1")) (Reference "Inh"))))
      (Population
       (@ (name "Ext"))
       (Size ,(+ NE NI))
       (Cell
        (Component
         (@ (name "stim"))
         (Definition (@ (url "Poisson.xml")) "Poisson")
         (Property
          (@ (units "Hz") (name "rate"))
          (SingleValue ,p-rate))
         (Initial
          (@ (units "ms") (name "t_next"))
          (Component
           (@ (name "uniform_t_next"))
           (Definition
             (@ (url "RandomUniform.xml"))
             "UniformDistribution")
           (Property
            (@ (units "unitless") (name "maximum"))
            (SingleValue "5.0"))
           (Property
            (@ (units "unitless") (name "minimum"))
            (SingleValue "0.1")))
          ))
        ))
      (Projection
       (@ (name "Inhibition"))
       (Source (Reference "Inh"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "fixed_weight") (receive_port "q"))))
       (Connectivity
        (Component
         (@ (name "RandomInh"))
         (Definition (@ (url "RandomFanIn.xml")) "RandomFanIn")
         (Property (@ (name "number")) (SingleValue ,(inexact->exact CI)))))
       (Plasticity
        (Component
         (@ (name "InhibitoryPlasticity"))
         (Definition (@ (url "Static.xml")) "Static")
         (Property
          (@ (units "nA") (name "weight"))
          (SingleValue ,JI))))
       (Delay (@ (units "ms")) (SingleValue ,del)))
      (Population
       (@ (name "Inh"))
       (Size ,NI)
       (Cell
        (Component
         (@ (name "nrn"))
         (Definition (@ (url "BrunelIaFdelta.xml")) "BrunelIaFdelta")
         (Property (@ (units "Mohm") (name "R")) (SingleValue ,R))
         (Property (@ (units "mV") (name "Vreset")) (SingleValue "10.0"))
         (Property (@ (units "ms") (name "tau")) (SingleValue ,tau))
         (Property (@ (units "ms") (name "tau_rp")) (SingleValue ,tau-rp))
         (Property (@ (units "mV") (name "theta")) (SingleValue ,theta))
         (Initial (@ (units "mV") (name "V")) (SingleValue "0.0"))
         (Initial
          (@ (units "ms") (name "t_rpend"))
          (SingleValue "0.0")))))
      (Projection
       (@ (name "External"))
       (Source (Reference "Ext"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "fixed_weight") (receive_port "q"))))
       (Connectivity
        (Component
         (@ (name "RandomExt"))
         (Definition (@ (url "RandomFanIn.xml")) "RandomFanIn")
         (Property (@ (name "number")) (SingleValue ,(inexact->exact CE)))))
       (Plasticity
        (Component
         (@ (name "ExternalPlasticity"))
         (Definition (@ (url "Static.xml")) "Static")
         (Property
          (@ (units "nA") (name "weight"))
          (SingleValue ,JE))))
       (Delay (@ (units "ms")) (SingleValue ,del))
       ))
    ))


(define variants
  '(
    (SI (g . 4.5) (eta . 0.9))
    (AI (g . 5.0) (eta . 2.0))
    (AR (g . 6.0) (eta . 4.0))
    (SR (g . 3.0) (eta . 2.0))
    ))
    

(for-each
 (lambda (var)
   (let ((label (car var))
         (g (alist-ref 'g (cdr var)))
         (eta (alist-ref 'eta (cdr var))))
     (call-with-output-file 
         (string-append (model-name (sprintf "brunel_network_delta_~A" label)) ".xml")
       (lambda  (output)
         (sxml:serialize-sxml
          (Prelude 
           (BrunelNetworkDelta g eta))
          output: output)))
     ))
 variants)
