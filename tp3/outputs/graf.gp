# Configurar la salida para que abra una ventana interactiva
set terminal qt size 800,600

# Títulos de los ejes y del gráfico
set title "Gráfico de X vs Y2"
set xlabel "Eje X"
set ylabel "Eje Y2"

# Mostrar la grilla de fondo
set grid

# Graficar la columna 1 y la 3
plot "termalizacion.dat" u 1:3 title "Datos Y2" with lines linewidth 2

# Pausar el script para que la ventana no se cierre de inmediato
bind "Close" "exit"
pause -1 "Presiona Enter en la terminal o cierra la ventana para salir..."
