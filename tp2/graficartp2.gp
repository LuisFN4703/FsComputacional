set terminal pngcairo size 800,600 enhanced font 'Times New Roman,11'
set grid
set output 'graficos/Graficotp2.png'



set xlabel "Tiempo (s)" font 'Times New Roman,14 bold'
set ylabel "I(t), Q(t)" font 'Times New Roman,14 bold'

set key inside left top box spacing 1.8 font 'Times New Roman,12'

plot ARG1 u 1:2 w lp lw 2 pt 5 ps 1 lc "red" title "Corriente I(t)", \
	 ARG1 u 1:3 w lp lw 2 pt 7 ps 1 lc "blue" title "Carga Q(t)", \
