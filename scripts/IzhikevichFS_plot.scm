
(use ploticus)


(for-each
 (lambda (label)
   
   (init 'eps (sprintf "~A.eps" label))

   (arg "-maxrows"     "800020")
   (arg "-maxfields"   "4500000")
   (arg "-maxvector"   "4500000")
   
   (proc "getdata"
	 `(("file"            . ,(sprintf "~A.dat" label))
	   ("fieldnames"      . "time spike tspike V U")
	   ))

   (proc "areadef"
	 `(("title"           . "Izhikevich 2007 (FS)")
	   ("titledetails"    . "size=14  align=C")
	   ("rectangle"       . "1 1 8 4")
	   ("xrange"          . "0 85")
	   ("yrange"          . "-80 40")
	   ))


   (proc "yaxis"
	 `(("stubs"     . "inc 10")
	   ("gridskip"  . "min")
	   ("label"     . "Membrane potential [mV]")
	   ))

   (proc "xaxis"
	 `(("stubs"     . "inc 10")
	   ("gridskip"  . "min")
	   ("label"     . "Time [ms]")
	   ))
   
   (proc "lineplot"
	 `(("xfield"      . "time")
	   ("yfield"      . "V")
	   ("linedetails" . "color=red width=.5")
	   ("legendlabel" . "V")
;	   ("maxinpoints" . 300000)
	   ))
#|
   (proc "curvefit"
	 `(("xfield"      . "time")
	   ("yfield"      . "U")
	   ("linedetails" . "color=blue width=.5")
	   ("legendlabel" . "U")
	   ("maxinpoints" . 300000)
	   ))
|#
   
   (proc "getdata"
	 `(("file"            . ,(sprintf "~A.dat" label))
	   ("fieldnames"      . "time spike tspike V U")
	   ("select"          . "@spike > 0")
	   ))
#|   
   (proc "scatterplot"
	 `(
	   ("xfield"  . "time")
	   ("yfield"  . "spike")
	   ("symbol"  . "shape=nicecircle fillcolor=yellow radius=0.02")
	   ))
|#
   
   (proc "legend"
	 `(("location" . "max-1 max")
	   ("seglen"   . "0.2")
	   ))
   
   (end))

 (list "IzhikevichFS_Iext400"
       "IzhikevichFS_Iext200"
       "IzhikevichFS_Iext100")
 )

