
(use ploticus)


(for-each
 (lambda (label)
   
   (init 'eps (sprintf "~A.eps" label))

   (arg "-maxrows"       "1000020")
   (arg "-maxfields"     "4500000")
   (arg "-maxvector"     "4500000")
   (arg "-cpulimit"      "90")
   (arg "-textsize"      "12")
   
   (proc "getdata"
	 `(("file"            . ,(sprintf "~A.dat" label))
	   ("fieldnames"      . "time U V")
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
	   ("labeldistance"     . "0.6")
	   ))

   (proc "xaxis"
	 `(("stubs"     . "inc 10")
	   ("gridskip"  . "min")
	   ("label"     . "Time [ms]")
	   ("labeldistance"     . "0.6")
	   ))

   (proc "lineplot"
	 `(("xfield"      . "time")
	   ("yfield"      . "V")
	   ("linedetails" . "color=red width=.5")
	   ("legendlabel" . "V (NineML native interpreter)")
	   ))

   (proc "getdata"
	 `(("file"            . ,(sprintf "~A_PyDSTool.dat" label))
	   ("fieldnames"      . "time U V")
	   ))

   (proc "lineplot"
	 `(("xfield"      . "time")
	   ("yfield"      . "V")
	   ("linedetails" . "color=blue width=.5")
	   ("legendlabel" . "V (PyDSTool + lib9ml)")
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

 
   (proc "legend"
	 `(("location" . "max-1 max+0.2")
	   ("seglen"   . "0.2")
	   ("textdetails"   . "size=12")
	   ))
   
   (end))

 (list "IzhikevichFS_Iext400"
       "IzhikevichFS_Iext200"
       "IzhikevichFS_Iext100")
 )

