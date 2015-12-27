#!/bin/sh

plotvar.scm -t "Izhikevich 2007 (FS)" -f time,spike,tspike,V,U -x time -y V --x-range=0:85 --y-range=-80:40 --data-filename=IzhikevichFS_Iext100.dat
plotvar.scm -t "Izhikevich 2007 (FS)" -f time,spike,tspike,V,U -x time -y V --x-range=0:85 --y-range=-80:40 --data-filename=IzhikevichFS_Iext200.dat
plotvar.scm -t "Izhikevich 2007 (FS)" -f time,spike,tspike,V,U -x time -y V --x-range=0:85 --y-range=-80:40 --data-filename=IzhikevichFS_Iext400.dat


