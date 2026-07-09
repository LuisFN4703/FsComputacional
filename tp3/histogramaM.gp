# graficar_histogramas.gp
set terminal pngcairo size 1600, 800 enhanced font 'Helvetica,12'
set output 'graficos/histogramas_M.png'

set style fill solid 0.7 noborder
set boxwidth 0.9 relative

# Bins
bw = 0.02
bin(x,width) = width*floor(x/width) + width/2.0

set key off
set yrange [0:*]
set xlabel "Magnetizacion por espin (m)"
set ylabel "Frecuencia"

set multiplot layout 2,4 title "Evolucion de los Histogramas de Magnetizacion" font ",16"

# Listas separadas por espacios
archivos = "outputs/hist_1.dat outputs/hist_2.dat outputs/hist_3.dat outputs/hist_4.dat outputs/hist_5.dat outputs/hist_6.dat outputs/hist_7.dat outputs/hist_8.dat"
titulos = "1.500 2.000 2.200 2.269 2.400 2.600 3.000 3.500"

do for [i=1:8] {
    # Extraemos el archivo y titulo correspondiente de la lista
    archivo = word(archivos, i)
    t = word(titulos, i)
    
    set title sprintf("T = %s", t) font ",14"

	# RESTRICCIÓN DINÁMICA DE INTERVALOS PARA M
    if (i == 1) { set xrange [0.97:1.00] }      # T = 1.5 (Mucho zoom en +1 o -1)
    if (i == 2) { set xrange [-0.96:-0.84] }      # T = 2.0
    if (i == 3) { set xrange [-0.95:-0.60] }      # T = 2.2
    if (i == 4) { set xrange [-0.9:0.9] }     # T = 2.269 (Fase crítica: fluctúa un montón)
    if (i == 5) { set xrange [-0.40:0.40] }     # T = 2.4
    if (i == 6) { set xrange [-0.20:0.20] }     # T = 2.6
    if (i == 7) { set xrange [-0.15:0.15] }     # T = 3.0
    if (i == 8) { set xrange [-0.1:0.1] }     # T = 3.5 (Centrada en 0)
    
    # Graficamos directamente la columna 2
    plot archivo using (bin($2, bw)):(1.0) smooth freq with boxes lc rgb "royalblue"
}

unset multiplot
