# Configuración de salida
set terminal pngcairo size 1280,960 enhanced font 'Verdana,10'
set grid
set key outside right top
set xlabel "x"

# ==========================================================
# GRÁFICO 1: FUNCIONES (Analítica, Interpolaciones e Integrales)
# ==========================================================
set output 'images/grafico_1_funciones.png'
set title "Comparativa de Funciones: Analítica, Interpolaciones e Integrales (h=0.2 y h=0.04)"
set ylabel "y(x)"

# Limitamos el eje Y para que el fenómeno de Runge (Lagrange/Newton) 
# no invisibilice a los Splines y a la Exacta.
set yrange [-2:12]

plot 'outputs/interpolacion_h02.out' u 1:5 w l lw 4 lc rgb "black" title "Exacta (Analítica)", \
     'outputs/interpolacion_h02.out' u 1:2 w l dt 2 lc rgb "red"   title "Lagrange h=0.2", \
     'outputs/interpolacion_h004.out' u 1:2 w l dt 3 lc rgb "red"   title "Lagrange h=0.04", \
     'outputs/interpolacion_h02.out' u 1:3 w p pt 6 ps 0.5 lc rgb "red" title "Newton h=0.2", \
     'outputs/interpolacion_h004.out' u 1:3 w p pt 4 ps 0.4 lc rgb "dark-red" title "Newton h=0.04", \
     'outputs/interpolacion_h02.out' u 1:4 w l lw 2 lc rgb "blue"  title "Splines h=0.2", \
     'outputs/interpolacion_h004.out' u 1:4 w l lw 2 lc rgb "cyan"  title "Splines h=0.04", \
     'outputs/integracion_h02.out' u 1:2 w l lw 2 lc rgb "orange" title "Integral (de Deriv. Central) h=0.2", \
     'outputs/integracion_h004.out' u 1:2 w l lw 2 lc rgb "brown"  title "Integral (de Deriv. Central) h=0.04"

# ==========================================================
# GRÁFICO 2: DERIVADAS (De cada interpolador con ambos h)
# ==========================================================
set output 'images/grafico_2_derivadas.png'
set title "Comparativa de Derivadas Numéricas (h=0.2 y h=0.04)"
set ylabel "dy/dx"
set yrange [*:*] # Escala automática para derivadas

plot 'outputs/derivadas_h02.out' u 1:5 w l lw 4 lc rgb "black" title "Derivada Exacta", \
     'outputs/derivadas_h02.out' u 1:4 w l lw 2 lc rgb "green" title "Central h=0.2", \
     'outputs/derivadas_h004.out' u 1:4 w l lw 2 lc rgb "spring-green" title "Central h=0.04", \
     'outputs/derivadas_h02.out' u 1:2 w l dt 2 lc rgb "magenta" title "Adelante h=0.2", \
     'outputs/derivadas_h004.out' u 1:2 w l dt 3 lc rgb "purple"  title "Adelante h=0.04", \
     'outputs/derivadas_h02.out' u 1:3 w l dt 4 lc rgb "red"     title "Atrás h=0.2", \
	 'outputs/derivadas_h004.out' u 1:3 w l dt 5 lc rgb "brown"     title "Atrás h=0.04"
