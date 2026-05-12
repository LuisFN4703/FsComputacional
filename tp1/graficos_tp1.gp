# Configuración general
set terminal pngcairo size 1024,768 enhanced font 'Verdana,12'
set grid
set key outside right top

# --- GRÁFICO 1: COMPARACIÓN DE INTERPOLACIÓN (h = 0.2) ---
set output 'grafico_interpolacion_h02.png'
set title "Interpolación de la Función (h = 0.2)"
set xlabel "x"
set ylabel "y(x)"
set yrange [-2:12] # Para que las oscilaciones de Lagrange no rompan la escala

plot 'interpolacion_h02.out' u 1:5 w l lw 3 lc rgb "black" title "Exacta", \
     'interpolacion_h02.out' u 1:4 w l lw 2 lc rgb "blue"  title "Splines", \
     'interpolacion_h02.out' u 1:2 w l dt 2 lc rgb "red"   title "Lagrange"

# --- GRÁFICO 2: DERIVADAS (h = 0.2) ---
set output 'grafico_derivadas_h02.png'
set title "Derivada Numérica vs Exacta (h = 0.2)"
set xlabel "x"
set ylabel "dy/dx"

plot 'derivadas_h02.out' u 1:5 w l lw 3 lc rgb "black" title "Exacta", \
     'derivadas_h02.out' u 1:4 w l lw 2 lc rgb "green" title "Central (O2)", \
     'derivadas_h02.out' u 1:2 w p pt 7 ps 0.5 title "Adelante (O1)"

# --- GRÁFICO 3: INTEGRACIÓN - RECONSTRUCCIÓN (h = 0.2) ---
set output 'grafico_integracion_h02.png'
set title "Reconstrucción de la Función (h = 0.2)"
set xlabel "x"
set ylabel "y(x)"

plot 'integracion_h02.out' u 1:3 w l lw 3 lc rgb "black" title "Original", \
     'integracion_h02.out' u 1:2 w l lw 2 lc rgb "orange" title "Recuperada"
