# Configuración general de la terminal para exportar a PNG
set terminal pngcairo size 800,600 enhanced font 'Arial,12'
set grid
set xlabel "Temperatura reducida (T)"

# Mejoramos el aspecto de la leyenda para informes académicos
set key box opaque spacing 1.2

# Definimos la temperatura crítica exacta (Onsager)
Tc = 2.0 / log(1.0 + sqrt(2.0))

# Definimos la línea vertical punteada en X = Tc (front asegura que pase por encima de la grilla)
set arrow 1 from Tc, graph 0 to Tc, graph 1 nohead dt 2 lc rgb "gray" lw 1.5 front

# Función de Onsager para la magnetización teórica por sitio
M_teo(x) = (x < Tc) ? (1.0 - (sinh(2.0/x))**(-4))**0.125 : 0.0
N_total = 10000.0

# 1. Magnetización vs Temperatura
set output "graficos/magnetizacion_vs_T.png"
set ylabel "Magnetización media <M>"
set title "Transición de Fase: Magnetización vs Temperatura"
plot "outputs/resultados_vs_T.dat" using 1:3 with linespoints pt 7 lw 2 lc rgb "blue" title "Simulación MC", \
     N_total * M_teo(x) with lines lw 3 lc rgb "black" dt 3 title "Solución de Onsager", \
     NaN with lines dt 2 lc rgb "gray" lw 1.5 title "T_c Teórica (2.269)"

# 2. Energía vs Temperatura
set output "graficos/energia_vs_T.png"
set ylabel "Energía media <E>"
set title "Energía Interna vs Temperatura"
plot "outputs/resultados_vs_T.dat" using 1:2 with linespoints pt 7 lw 2 lc rgb "dark-red" title "<E>", \
     NaN with lines dt 2 lc rgb "gray" lw 1.5 title "T_c Teórica"

# 3. Calor Específico vs Temperatura
set output "graficos/cv_vs_T.png"
set ylabel "Calor Específico (C_v)"
set title "Divergencia del Calor Específico"
plot "outputs/resultados_vs_T.dat" using 1:4 with linespoints pt 7 lw 2 lc rgb "forest-green" title "C_v", \
     NaN with lines dt 2 lc rgb "gray" lw 1.5 title "T_c Teórica"

# 4. Susceptibilidad vs Temperatura
set output "graficos/susceptibilidad_vs_T.png"
set ylabel "Susceptibilidad magnética ({/Symbol c})"
set title "Divergencia de la Susceptibilidad"
plot "outputs/resultados_vs_T.dat" using 1:5 with linespoints pt 7 lw 2 lc rgb "purple" title "{/Symbol c}", \
     NaN with lines dt 2 lc rgb "gray" lw 1.5 title "T_c Teórica"

# 5. Tasa de Aceptación vs Temperatura
set output "graficos/aceptacion_vs_T.png"
set ylabel "Fracción de pasos aceptados (%)"
set title "Tasa de Aceptación de Metropolis"
# Ubicamos la leyenda abajo a la derecha para que no tape los datos (que crecen con T)
set key bottom right
plot "outputs/resultados_vs_T.dat" using 1:6 with linespoints pt 7 lw 2 lc rgb "orange" title "Aceptación", \
     NaN with lines dt 2 lc rgb "gray" lw 1.5 title "T_c Teórica"
# Restauramos la posición de la leyenda para futuros gráficos si los hubiera
set key top right

print "¡Gráficos generados con calidad académica y leyendas actualizadas!"
