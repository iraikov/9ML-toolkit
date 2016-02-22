(use srfi-1 utils extras ssax sxpath sxpath-lolevel )
(require-library sxml-transforms sxml-serializer)
(import (prefix sxml-transforms sxml:)
        (prefix sxml-serializer sxml:))
(define shared-dir (make-pathname (chicken-home) "9ML"))


(define (Prelude . content)
  `(NineML
    (@ (xmlns "http://nineml.net/9ML/1.0"))
    
    (Dimension (@ (name "current") (i "1")))
    (Dimension (@ (name "time") (i "1")))
    (Dimension (@ (name "capacitance") (i "2") (l "-2") (m "-1") (t "4" )))
    (Dimension (@ (name "conductance") (i "2") (l "-2") (m "-1") (t "3" )))
    (Dimension (@ (name "voltage") (i "-1") (t "-3") (m "1") (l "2" )))
    (Unit (@ (symbol "mV") (dimension "voltage") (power "-3")))
    (Unit (@ (symbol "uF") (dimension "capacitance") (power "-6")))
    (Unit (@ (symbol "pA") (dimension "current") (power "-12")))
    (Unit (@ (symbol "nS") (dimension "conductance") (power "-9")))
    (Unit (@ (symbol "ms") (dimension "time") (power "-3")))

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
    (Parameter (@ (name "Delta") (dimension "voltage"))) 
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
     (Constant (@ (units "pA") (name "one_pA")) 1.0) 

     (StateVariable (@ (name "V") (dimension "voltage"))) 
     (StateVariable (@ (name "W") (dimension "dimensionless"))) 
     (StateVariable (@ (name "t_rpend") (dimension "time"))) 

     (Regime (@ (name "subthresholdRegime")) 
             (OnCondition (@ (target_regime "refractoryRegime"))
                          (Trigger (MathInline "V > theta")) 
                          (OutputEvent (@ (port "spikeOutput"))) 
                          (StateAssignment (@ (variable "V")) (MathInline "V_R"))
                          (StateAssignment (@ (variable "W")) (MathInline "W + b"))
                          (StateAssignment (@ (variable "t_rpend")) (MathInline "t + tau_rp"))
                          )
             (TimeDerivative 
              (@ (variable "V")) 
              (MathInline "(- g_L * (V - E_L) + (g_L * Delta * exp ((V - V_T) / Delta)) - W*one_pA + Isyn + Iext) / C_m"))
             (TimeDerivative
              (@ (variable "W")) 
              (MathInline "(a * (V - E_L) - W*one_mV) / tau_w"))
             )
     
     (Regime (@ (name "refractoryRegime"))
             (OnCondition (@ (target_regime "subthresholdRegime")) 
                          (Trigger (MathInline "t > t_rpend")) 
                          (OutputEvent (@ (port "refractoryEnd"))) 
                          )
             )
             
  ))
)


(define (AEIF name #!key
              (C_m    200.0)
              (g_L     10.0)
              (E_L    -58.0)
              (Delta    2.0)
              (V_T    -50.0)
              (theta  -45.0)
              (V_R    -46.0)
              (V_peak  20.0)
              (tau_w  120.0)
              (tau_rp  0.25)
              (a       2.0)
              (b      100.0)
              (Iext   0.0)
              )

  `(Component 
    (@ (name ,name)) 
    (Definition "AEIF") 
    (Property (@ (units "uF") (name "C_m")) (SingleValue ,C_m)) 
    (Property (@ (units "nS") (name "g_L")) (SingleValue ,g_L)) 
    (Property (@ (units "mV") (name "E_L")) (SingleValue ,E_L))
    (Property (@ (units "mV") (name "V_T")) (SingleValue ,V_T))
    (Property (@ (units "mV") (name "V_R")) (SingleValue ,V_R))
    (Property (@ (units "mV") (name "V_peak")) (SingleValue ,V_peak))
    (Property (@ (units "mV") (name "theta")) (SingleValue ,theta))
    (Property (@ (units "ms") (name "tau_w")) (SingleValue ,tau_w))
    (Property (@ (units "ms") (name "tau_rp")) (SingleValue ,tau_rp))
    (Property (@ (units "mV") (name "Delta")) (SingleValue ,Delta))
    (Property (@ (name "a")) (SingleValue ,a))
    (Property (@ (name "b")) (SingleValue ,b))

    (Property (@ (units "pA") (name "Iext")) (SingleValue ,Iext)) 
    
    (Initial (@ (units "mV") (name "V")) (SingleValue -65.0))
    (Initial (@ (name "W"))  (SingleValue 0.0))
    (Initial (@ (units "ms") (name "t_rpend")) (SingleValue 0.0))
  
  ))


(call-with-output-file 
    "AEIF.xml" 
  (lambda (output)
    (sxml:serialize-sxml
     (Prelude 
      AEIFdef
      (AEIF "TestAdEx1" Iext: 210.0)
      )
     output: output)
     ))

