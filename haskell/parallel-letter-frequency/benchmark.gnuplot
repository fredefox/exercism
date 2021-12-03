set datafile separator ','

plot 'parallel.csv' using 2 with points title "Parallel"
set style line 5 lt rgb "cyan" lw 3 pt 6
replot 'sequential.csv' using 2 with points title "Sequential"

set term png
set output "benchmark.png"
replot