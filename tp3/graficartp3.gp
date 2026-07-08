# Configuración general de la terminal para exportar a PNG
set terminal pngcairo size 800,600 enhanced font 'Arial,12'
set grid
set xlabel "Temperatura reducida (T)"

# 1. Magnetización vs Temperatura
set output "graficos/magnetizacion_vs_T.png"
set ylabel "Magnetización media <M>"
set title "Transición de Fase: Magnetización vs Temperatura"
plot "outputs/resultados_vs_T.dat" using 1:3 with linespoints pt 7 lw 2 lc rgb "blue" title "<M>"

# 2. Energía vs Temperatura
set output "graficos/energia_vs_T.png"
set ylabel "Energía media <E>"
set title "Energía Interna vs Temperatura"
plot "outputs/resultados_vs_T.dat" using 1:2 with linespoints pt 7 lw 2 lc rgb "red" title "<E>"

# 3. Calor Específico vs Temperatura
set output "graficos/cv_vs_T.png"
set ylabel "Calor Específico (C_v)"
set title "Divergencia del Calor Específico"
plot "outputs/resultados_vs_T.dat" using 1:4 with linespoints pt 7 lw 2 lc rgb "forest-green" title "C_v"

# 4. Susceptibilidad vs Temperatura
set output "graficos/susceptibilidad_vs_T.png"
set ylabel "Susceptibilidad magnética ({/Symbol c})"
set title "Divergencia de la Susceptibilidad"
plot "outputs/resultados_vs_T.dat" using 1:5 with linespoints pt 7 lw 2 lc rgb "purple" title "{/Symbol c}"

# 5. Tasa de Aceptación vs Temperatura
set output "graficos/aceptacion_vs_T.png"
set ylabel "Fracción de pasos aceptados (%)"
set title "Tasa de Aceptación de Metropolis"
plot "outputs/resultados_vs_T.dat" using 1:6 with linespoints pt 7 lw 2 lc rgb "orange" title "Aceptación"

print "¡Gráficos generados exitosamente en la carpeta 'graficos/'!"
