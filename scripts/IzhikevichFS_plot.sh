#!/bin/sh

mv IzhikevichFS_UL_TestIzhikevichFS_Iext100ivp118.log IzhikevichFS_Iext100.dat
mv IzhikevichFS_UL_TestIzhikevichFS_Iext200ivp141.log IzhikevichFS_Iext200.dat
mv IzhikevichFS_UL_TestIzhikevichFS_Iext400ivp164.log IzhikevichFS_Iext400.dat

~/bin/chicken/bin/csi -s IzhikevichFS_plot.scm

