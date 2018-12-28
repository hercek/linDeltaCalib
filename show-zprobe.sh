#!/usr/bin/sh
d=`cat $1 | awk '/Z-probe/{++i}; END{print int(0.5+sqrt(i))}'`
grep Z-probe $1 | \
  cut -c16- | \
  sed -e 's/Z-probe://' -e 's/ X:/ /' -e 's/ Y:/ /' | \
  sort -nk3 | \
  sort -snk2 > /tmp/gnuplot.data
gnuplot -e "set dgrid3d $d,$d; set hidden3d; set xlabel \"X\"; splot \"/tmp/gnuplot.data\" using 2:3:1 with lines; pause -1"
