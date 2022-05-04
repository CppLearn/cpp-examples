#!/usr/bin/gnuplot

set samples 500
set grid
set object rectangle from screen 0,0 to screen 1,1 behind fillcolor rgb "#F5F5DC" fillstyle solid noborder
plot "clisp-scatterplot.dat" using 1:2 with points pointtype 5
pause -1

