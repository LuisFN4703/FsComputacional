# graficar_histogramas.gp
# Usamos 'png' estandar para asegurar maxima compatibilidad
set terminal png size 1600, 800 enhanced font 'Helvetica,12'
set output 'graficos/histogramas_M.png'

# Configuración del estilo del histograma
set style fill solid 0.7 noborder
set boxwidth 0.9 relative

# Definición de los intervalos (bins)
bw = 0.02
bin(x,width) = width*floor(x/width) + width/2.0

set key off
set xrange [-1.1:1.1]
set yrange [0:*]
set xlabel "Magnetizacion por espin (m)"
set ylabel "Frecuencia"

set multiplot layout 2,4 title "Evolucion de los Histogramas de Magnetizacion" font ",16"

# Cambiamos el array por una cadena de texto (funciona en cualquier version de Gnuplot)
temps = "1.5 2.0 2.2 2.269 2.4 2.6 3.0 3.5"

do for [t in temps] {
    temp = real(t)
    set title sprintf("T = %.3f", temp) font ",14"
    
    # Filtrado por temperatura y graficado
    plot 'outputs/histogramas.dat' using (abs($1 - temp) < 1e-3 ? bin($3, bw) : NaN):(1.0) \
         smooth freq with boxes lc rgb "royalblue"
}

unset multiplot
