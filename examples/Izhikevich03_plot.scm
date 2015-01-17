
(use ploticus)

(init 'eps "Izhikevich03_RS.eps")

(arg "-maxrows"     "300000")
(arg "-maxfields"   "1500000")
(arg "-maxvector"   "1500000")

(proc "getdata"
      `(("file"            . "Izhikevich03_RS.dat")
	("fieldnames"      . "time spike tspike V U")
	))

(proc "areadef"
      `(("title"           . "Izhikevich 2003 (RS)")
	("titledetails"    . "size=14  align=C")
	("rectangle"       . "1 1 8 4")
	("xrange"          . "0 500")
	("yrange"          . "-80 40")
	))


(proc "yaxis"
      `(("stubs"     . "inc 10")
	("gridskip"  . "min")
	))

(proc "curvefit"
      `(("xfield"      . "time")
	("yfield"      . "V")
	("linedetails" . "color=red width=.5")
	("legendlabel" . "V")
	("maxinpoints" . 300000)
	))

(proc "curvefit"
      `(("xfield"      . "time")
	("yfield"      . "U")
	("linedetails" . "color=blue width=.5")
	("legendlabel" . "U")
	("maxinpoints" . 300000)
	))

(proc "getdata"
      `(("file"            . "Izhikevich03_RS.dat")
	("fieldnames"      . "time spike tspike V U")
	("select"          . "@spike > 0")
	))

(proc "scatterplot"
      `(
	("xfield"  . "time")
	("yfield"  . "spike")
	("symbol"  . "shape=nicecircle fillcolor=yellow radius=0.02")
	))
 
(proc "legend"
      `(("location" . "max-1 max")
	("seglen"   . "0.2")
	))

(end)
