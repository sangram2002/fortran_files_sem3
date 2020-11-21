set xlabel "x"
set ylabel "y"
set title ARG1
set xrange [-8:8]
set yrange [-8:8]
set terminal png size 800,800
set output ARG2
set label "vec 1" at 2.2,-3 
set label "vec 2" at -3,0.3
set label "vec 3" at -3,-3
set label "vec 4" at 4,1.6


plot ARG3 using 1:2:3:4 title "transformed vectors" with vectors filled head lw 3,'initial_vector.dat'  using 1:2:3:4 title " initial vectors" with vectors filled head lw 3

