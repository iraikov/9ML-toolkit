(use srfi-1 utils extras ssax sxpath sxpath-lolevel )
(require-library sxml-transforms)
(import (prefix sxml-transforms sxml:))
(define shared-dir (make-pathname (chicken-home) "9ML"))

(load (make-pathname shared-dir "stx-engine.scm"))
(load (make-pathname shared-dir "SXML-to-XML.scm"))

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
    (Dimension (@ (name="voltage") (i "-1") (t "-3") (m "1") (l "2" )))
    (Dimension (@ (name "resistance") (t "-3") (m "1") (l "2") (i "-2")))

    (Unit (@ (symbol "mV") (dimension "voltage") (power "-3")))
    (Unit (@ (symbol "uF") (dimension "capacitance") (power "-6")))
    (Unit (@ (symbol "Hz") (dimension "frequency") (power "0")))
    (Unit (@ (symbol "nA") (dimension "current") (power "-9")))
    (Unit (@ (symbol "ms") (dimension "time") (power "-3") ))
    (Unit (@ (symbol "Mohm") (dimension "resistance") (power "6")))

    ,@content))


(define (BrunelNetworkAlpha g eta)
  (let ((w 13.77) (rate 9682.0))
    `(
      (Population
       (@ (name "Exc"))
       (Number "10000")
       (Cell
        (Component
         (@ (name "nrn"))
         (Definition (@ (url "BrunelIaF.xml")) "BrunelIaF")
         (Property (@ (units "Mohm") (name "R")) (SingleValue "1.5"))
         (Property (@ (units "mV") (name "Vreset")) (SingleValue "10.0"))
         (Property (@ (units "ms") (name "tau")) (SingleValue "20.0"))
         (Property (@ (units "ms") (name "tau_rp")) (SingleValue "2.0"))
         (Property (@ (units "mV") (name "theta")) (SingleValue "20.0"))
         (Initial (@ (units "mV") (name "V")) (SingleValue "0.0"))
         (Initial
          (@ (units "ms") (name "t_rpend"))
          (SingleValue "0.0")))))
      (Projection
       (@ (name "Excitation"))
       (Source (Reference "Exc"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "Isyn") (receive_port "Isyn"))))
       (Connectivity
        (Component
         (@ (name "RandomExc"))
         (Definition (@ (url "RandomFanIn.xml")) "RandomFanIn")
         (Property (@ (name "number")) (SingleValue "1000"))))
       (Response
        (Component
         (@ (name "syn"))
         (Definition (@ (url "AlphaPSR.xml")) "AlphaPSR")
         (Property (@ (units "ms") (name "tau_syn")) (SingleValue "0.1"))
         (Initial (@ (units "nA") (name "A")) (SingleValue "0.0"))
         (Initial (@ (units "nA") (name "B")) (SingleValue "0.0")))
        (FromPlasticity (@ (send_port "weight") (receive_port "q"))))
       (Plasticity
        (Component
         (@ (name "ExcitatoryPlasticity"))
         (Definition (@ (url "StaticConnection.xml")) "StaticConnection")
         (Initial
          (@ (units "nA") (name "weight"))
          (SingleValue ,w))))
       (Delay (@ (units "ms")) (SingleValue "1.5")))
      (Selection
       (@ (name "All neurons"))
       (Concatenate
        (Item (@ (index "0")) (Reference "Exc"))
        (Item (@ (index "1")) (Reference "Inh"))))
      (Population
       (@ (name "Ext"))
       (Number "12500")
       (Cell
        (Component
         (@ (name "stim"))
         (Definition (@ (url "Poisson.xml")) "Poisson")
         (Property
          (@ (units "Hz") (name "rate"))
          (SingleValue ,(* eta rate)))
         (Initial
          (@ (units "ms") (name "t_next"))
          (SingleValue "5.0")))))
      (Projection
       (@ (name "Inhibition"))
       (Source (Reference "Inh"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "Isyn") (receive_port "Isyn"))))
       (Connectivity
        (Component
         (@ (name "RandomInh"))
         (Definition (@ (url "RandomFanIn.xml")) "RandomFanIn")
         (Property (@ (name "number")) (SingleValue "250"))))
       (Response
        (Component
         (@ (name "syn"))
         (Definition (@ (url "AlphaPSR.xml")) "AlphaPSR")
         (Property (@ (units "ms") (name "tau_syn")) (SingleValue "0.1"))
         (Initial (@ (units "nA") (name "A")) (SingleValue "0.0"))
         (Initial (@ (units "nA") (name "B")) (SingleValue "0.0")))
        (FromPlasticity (@ (send_port "weight") (receive_port "q"))))
       (Plasticity
        (Component
         (@ (name "InhibitoryPlasticity"))
         (Definition (@ (url "StaticConnection.xml")) "StaticConnection")
         (Initial
          (@ (units "nA") (name "weight"))
          (SingleValue ,(* g w)))))
       (Delay (@ (units "ms")) (SingleValue "1.5")))
      (Population
       (@ (name "Inh"))
       (Number "2500")
       (Cell
        (Component
         (@ (name "nrn"))
         (Definition (@ (url "BrunelIaF.xml")) "BrunelIaF")
         (Property (@ (units "Mohm") (name "R")) (SingleValue "1.5"))
         (Property (@ (units "mV") (name "Vreset")) (SingleValue "10.0"))
         (Property (@ (units "ms") (name "tau")) (SingleValue "20.0"))
         (Property (@ (units "ms") (name "tau_rp")) (SingleValue "2.0"))
         (Property (@ (units "mV") (name "theta")) (SingleValue "20.0"))
         (Initial (@ (units "mV") (name "V")) (SingleValue "0.0"))
         (Initial
          (@ (units "ms") (name "t_rpend"))
          (SingleValue "0.0")))))
      (Projection
       (@ (name "External"))
       (Source (Reference "Ext"))
       (Destination
        (Reference "All neurons")
        (FromResponse (@ (send_port "Isyn") (receive_port "Isyn"))))
       (Connectivity
        (Component
         (@ (name "OneToOne"))
         (Definition (@ (url "OneToOne.xml")) "OneToOne")))
       (Response
        (Component
         (@ (name "syn"))
         (Definition (@ (url "AlphaPSR.xml")) "AlphaPSR")
         (Property (@ (units "ms") (name "tau_syn")) (SingleValue "0.1"))
         (Initial (@ (units "nA") (name "A")) (SingleValue "0.0"))
         (Initial (@ (units "nA") (name "B")) (SingleValue "0.0")))
        (FromPlasticity (@ (send_port "weight") (receive_port "q"))))
       (Plasticity
        (Component
         (@ (name "ExternalPlasticity"))
         (Definition (@ (url "StaticConnection.xml")) "StaticConnection")
         (Initial
          (@ (units "nA") (name "weight"))
          (SingleValue ,w))))
       (Delay (@ (units "ms")) (SingleValue "1.5"))
       ))
    ))

(define range-g (list-tabulate 8 (lambda (x) (+ 0.5 (* 0.5 x)))))
(define range-eta (list-tabulate 8 (lambda (x) (+ 1.0 (* 1.0 x)))))

(for-each
 (lambda (g)
   (for-each
    (lambda (eta)
      (with-output-to-file 
          (sprintf "Brunel_network_alpha_g~A_eta~A.xml" g eta)
        (lambda ()
          (print-fragments 
           (generate-XML
            (Prelude 
             (BrunelNetworkAlpha (- g) eta)))))
        ))
    range-eta
    ))
 range-g
 )
