(use srfi-1 utils extras ssax sxpath sxpath-lolevel )
(require-library sxml-transforms sxml-serializer)
(import (prefix sxml-transforms sxml:)
        (prefix sxml-serializer sxml:))
(define shared-dir (make-pathname (chicken-home) "9ML"))


(define (Prelude . content)
  `(NineML
    (@ (xmlns "http://nineml.net/9ML/1.0"))
    
    (Dimension (@ (name "current") (i "1") (k "0") (j "0") (m "0") (l "0") (n "0") (t "0")))
    (Dimension (@ (name "capacitance") (i "2") (l "-2") (m "-1") (t "4" )))
    (Dimension (@ (name="voltage") (i "-1") (t "-3") (m "1") (l "2" )))
    (Unit (@ (symbol "mV") (dimension "voltage") (power "-3")))
    (Unit (@ (symbol "uF") (dimension "capacitance") (power "-6")))

    ,@content))

  

(define AEIFdef
  `(ComponentClass
    (@ (name "AEIF")) 

    (Parameter (@ (name "C_m") (dimension "capacitance"))) 
    (Parameter (@ (name "g_L") (dimension "conductance"))) 
    (Parameter (@ (name "E_L") (dimension "voltage"))) 
    (Parameter (@ (name "V_T") (dimension "voltage"))) 
    (Parameter (@ (name "V_R") (dimension "voltage"))) 
    (Parameter (@ (name "V_peak") (dimension "voltage"))) 
    (Parameter (@ (name "Delta") (dimension "dimensionless"))) 
    (Parameter (@ (name "theta") (dimension "voltage"))) 
    (Parameter (@ (name "tau_w") (dimension "time"))) 
    (Parameter (@ (name "tau_rp") (dimension "time"))) 
    (Parameter (@ (name "a") (dimension "dimensionless"))) 
    (Parameter (@ (name "b") (dimension "dimensionless"))) 
    (Parameter (@ (name "Iext") (dimension "current"))) 

    (AnalogReducePort (@ (name "Isyn") (dimension "current"))) 
    (AnalogSendPort (@ (name "V") (dimension "voltage")))
    (AnalogSendPort (@ (name "W") (dimension "current"))) 
    (AnalogSendPort (@ (name "t_rpend") (dimension "time")))
    (EventPort (@ (name "refractoryEnd") (mode "send"))) 
    (EventPort (@ (name "spikeOutput") (mode "send"))) 

    (Dynamics 
     (Constant (@ (units "ms") (name "one_ms")) 1.0) 
     (Constant (@ (units "mV") (name "one_mV")) 1.0) 
     (Constant (@ (units "megaohm") (name "one_megaohm")) 1.0) 
     (StateVariable (@ (name "V") (dimension voltage))) 
     (StateVariable (@ (name "U") (dimension current))) 
     (Regime (@ (name "subVb")) 
                 (OnCondition (@ (target_regime "subthreshold")) 
                                  (Trigger (MathInline "V > Vb"))) 
                 (TimeDerivative (@ (variable U)) 
                                     (MathInline "(a * -U) / one_ms")) 
                 (TimeDerivative (@ (variable V)) 
                                     (MathInline "(((k * (V - Vr) * (V - Vt) / one_mV) + (((- U) + Iext) * one_megaohm)) / Cm) / one_megaohm"))) 
     (Regime (@ (name "subthreshold")) 
                 (OnCondition (@ (target_regime "subVb")) 
                                  (Trigger (MathInline "V > Vpeak")) 
                                  (OutputEvent (@ (port "spikeOutput"))) 
                                  (StateAssignment (@ (variable "V")) (MathInline "c")))
                 (TimeDerivative 
                  (@ (variable "U")) 
                  (MathInline "(a*((b*(((V - Vb) / one_mV)^3)) - U)) / one_ms"))
                 (TimeDerivative
                  (@ (variable "V")) 
                  (MathInline "(((k * (V - Vr) * (V - Vt) / one_mV) + (((- U) + Iext) * one_megaohm)) / Cm) / one_megaohm"))
                 )
     ))
  )


(define (AEIF name #!key (a 0.2) (b 0.025) (c 45.0)
                       (k 1.0) (Vpeak 25.0) (Vt -55.0) (Vr -40.0) (Vb -55.0)
                       (Cm 20.0) (Iext 0.0) (Isyn 0.0)
                       )
  `(Component 
    (@ (name ,name)) 
    (Definition "AEIF") 
    (Property (@ (name "a")) (SingleValue ,a)) 
    (Property (@ (units "nA") (name "b")) (SingleValue ,b)) 
    (Property (@ (units "mV") (name "c")) (SingleValue ,c)) 
    (Property (@ (name "k")) (SingleValue ,k)) 
    (Property (@ (units "mV") (name "Vpeak")) (SingleValue ,Vpeak))
    (Property (@ (units "mV") (name "Vt")) (SingleValue ,Vt))
    (Property (@ (units "mV") (name "Vr")) (SingleValue ,Vr))
    (Property (@ (units "mV") (name "Vb")) (SingleValue ,Vb))
    (Property (@ (units "uF") (name "Cm")) (SingleValue ,Cm)) 
    (Property (@ (units "nA") (name "Iext")) (SingleValue ,Iext)) 
    (Property (@ (units "nA") (name "Isyn")) (SingleValue ,Isyn)) 
    
    (Initial (@ (units "mV") (name "V")) (SingleValue -65.0)) 
    (Initial (@ (units "nA") (name "U")) (SingleValue -1.625)))
  
  )


(call-with-output-file 
    "IzhikevichFS.xml" 
  (lambda (output)
    (sxml:serialize-sxml
     (Prelude 
      IzhikevichFSdef
      (IzhikevichFS "IzhikevichFS_Iext100" Iext: 100.0)
      (IzhikevichFS "IzhikevichFS_Iext200" Iext: 200.0)
      (IzhikevichFS "IzhikevichFS_Iext400" Iext: 400.0)
      )
     output: output)
     ))

