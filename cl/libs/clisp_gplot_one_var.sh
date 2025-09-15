#!/usr/bin/gnuplot

set samples 500
set grid
set object rectangle from screen 0,0 to screen 1,1 behind fillcolor rgb "#F5F5DC" fillstyle solid noborder
set style line 1 lc rgb '#0060ad' lt 1 lw 2 pt 7 pi -1 ps 1.0
plot "clisp-one-var-plot.dat" with linespoints ls 1
pause -1




