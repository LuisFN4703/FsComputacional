# graficar_histogramas_E.gp
set terminal pngcairo size 1600, 800 enhanced font 'Helvetica,12'
set output 'graficos/histogramas_E.png'

set style fill solid 0.7 noborder
set boxwidth 0.9 relative

# Bins para la energía
bw = 0.02
bin(x,width) = width*floor(x/width) + width/2.0

set key off

# Rangos adaptados para la energía por espín

set yrange [0:*]
set xlabel "Energia por espin (e)"
set ylabel "Frecuencia"

set multiplot layout 2,4 title "Evolucion de los Histogramas de Energia" font ",16"

# Lista de tus 8 archivos y sus títulos correspondientes
archivos = "outputs/hist_1.dat outputs/hist_2.dat outputs/hist_3.dat outputs/hist_4.dat outputs/hist_5.dat outputs/hist_6.dat outputs/hist_7.dat outputs/hist_8.dat"
titulos = "1.500 2.000 2.200 2.269 2.400 2.600 3.000 3.500"

do for [i=1:8] {
    archivo = word(archivos, i)
    t = word(titulos, i)
    
    set title sprintf("T = %s", t) font ",14"

	# RESTRICCIÓN DINÁMICA DE INTERVALOS PARA E
    if (i == 1) { set xrange [-3.457:-3.323] }      # T = 1.5 
    if (i == 2) { set xrange [-1.64:-1.51] }      # T = 2.0
    if (i == 3) { set xrange [-1.8:-1.5] }      # T = 2.2
    if (i == 4) { set xrange [-1.1:-0.71] }     # T = 2.269 (Fase crítica: fluctúa un montón)
    if (i == 5) { set xrange [-1.95:-1.75] }     # T = 2.4
    if (i == 6) { set xrange [-2.2:-2] }     # T = 2.6
    if (i == 7) { set xrange [-1.4:-1.25] }     # T = 3.0
    if (i == 8) { set xrange [-0.05:0.1] }     # T = 3.5 
    
    # Grafica directo la columna 1 pasando por la funcion bin
    plot archivo using (bin($1, bw)):(1.0) smooth freq with boxes lc rgb "royalblue"
}

unset multiplot
