#!/usr/bin/gnuplot

set term qt persist
set xrange [-5:5]
set yrange [-5:5]
set grid
plot '< examples/02-randomwalk.raku' title '' with lines
