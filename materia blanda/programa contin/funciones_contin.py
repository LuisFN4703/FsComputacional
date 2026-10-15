from tkinter import filedialog as fd
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Button
from scipy.signal import argrelextrema
from matplotlib.widgets import SpanSelector
import tkinter as tk
from tkinter import messagebox

#-------------------Funciones Marcos-------------------#
def conv(x):
		return float(x.decode('utf-8').replace(',', '.'))

def leer_archivo(archivo):    #lee el archivo con numpy y devuelve las columnas de tiempo y presion, el archivo tal cual lo da la balanza
	#np.set_printoptions(precision=n_decimales)
	#comento la línea porque Hernán M le sacó los encabezados y todo a sus archivos. Algunos tienen 2 columnas, otros 3, otros 4.
	#además, tiene comas en vez de puntos
	#t1, Bpos, Bspd, A, Mma, P1, P2, T, SP, pH, AD  = np.loadtxt(archivo, delimiter='\t', skiprows = 1,unpack=True)
	#a, t, P1, b, c  = np.loadtxt(archivo, delimiter='\t', unpack=True) # converters={1: conv, 2: conv, 3: conv, 4: conv}) #convertir 3 columnas converters={0: conv, 1: conv, 2: conv})
	t1, P1 = np.loadtxt(archivo, delimiter='\t', skiprows = 1,unpack=True)
	return t1,P1
	
def shiftear_tiempo(t,P1):
	t_inicial = t[0]   #suponemos que los datos están recortados a partir del momento que la balanza llegó al área (tomamos en general como el máximo)
	t = t - t_inicial	#restamos el tiempo inicial
	return(t,P1)

def normalizar_P(P1):	
	max_P1 = np.max(P1) #para normalizar
	P1 = P1/max_P1
	return(P1)

def generar_t_intervalos(delta_t_0,puntos_por_intervalo,t_finales): #genero los tiempos que quiero por intervalo
	#delta_t_0: es la diferencia de tiempo entre el primer y segundo punto, no podemos arrancar desde 0 porque contín no lo soporta
	#puntos_por_intervalo: cuantos puntos va a tener los intervalos, es un array, por ejemplo [49, 201] si son dos grupos con 49 y 201 puntos respectivamente 
	#t_t_finales: son los tiempos finales de cada intervalo
	n_grupos = len(puntos_por_intervalo)
	t_deseados = []
	if n_grupos == len(t_finales): #chequeo tener la misma cantidad de tiempos finales que de intervalos
		for i in range(0,n_grupos):
			if i != 0:
				t_deseados.append(np.linspace(t_finales[i-1],t_finales[i],puntos_por_intervalo[i]))
			else:
				t_deseados.append(np.linspace(delta_t_0,t_finales[i],puntos_por_intervalo[i]))
		return(t_deseados)
	else:
		print("Error en generar_intervalos(delta_t_0,puntos_por_intervalo,t_finales)")

def generar_NSTEND(puntos_por_intervalo,delta_t_0,t_finales):  #Genera la línea de NSTEND, es decir, los grupos de datos.
    if len(puntos_por_intervalo) == len(t_finales) and len(puntos_por_intervalo) > 1:
        n_intervalos_t = len(puntos_por_intervalo)
        #print("número de intervalos:",n_intervalos_t)
        texto = " NSTEND  "+ str(puntos_por_intervalo[0])+ "     {:.2E}".format(delta_t_0)+"    {:.2E}\n".format(t_finales[0])
        for i in range(0,n_intervalos_t-1):
            #print("trabajando en el intervalo",i)
            texto = texto + " NSTEND  "+ str(puntos_por_intervalo[i+1])+ "     {:.2E}".format(t_finales[i]) + "    {:.2E}\n".format(t_finales[i+1])
        return texto
    else:
       print("Error en NSTEND")
       return "error en NSTEND"

def generar_NINTT(puntos_por_intervalo): #Me baso solo en este array, se supone que de haber un error en parámetros, surgirá en la función generar_NSTEND()
    texto = " NINTT                   " + str(len(puntos_por_intervalo)) + '.\n'
    return texto
    
def obt_indices_de_tiempos_deseados(t,t_deseados): #obtiene las posiciones en el array de tiempo de los tiempos más cercanos a los tiempos deseados equiespaciados
    indices = []                                #ATENCIÓN: me devuelve una LISTA de ARRAYS tiempos deseados para cada grupo
    for tiempos in t_deseados:
        #print(type(t),t)
        i_cercana_t_deseado = np.searchsorted(t,tiempos)  #esta función siempre obtiene el tiempo más cercano por encima, es decir, si tenemos entre los tiempos
        #t = [0.1, 0.2, 0.3, ...] y buscamos 0.21, obtendremos 2, es decir la tercera posición, no redondea 0.21 a 0.2 sino que nos da 0.3 directamente.
        indices.append(i_cercana_t_deseado)
    for indices_i in indices:
        if indices_i[-1] == indices_i[-2]: #si ponemos demasiados números, podemos repetir algún índice
            print("ATENCION, ERROR EN LOS ÍNDICES, TIEMPOS REPETIDOS",indices_i)
            print(t[indices_i])
            #raise ValueError("ESPACIAR TIEMPOS")
    #else:
    return indices

def interpolar_presiones(t_deseados,indices_t_deseados,tiempos,presiones): #interpola porque CONTIN necesita intervalos regulares, y el equipo no da intervalos regulares sino con pequeñas variaciones
    P_interpoladas = []     #Como en general voy a tener varias listas de tiempos deseados, genero un set de presiones interpoladas para cada set de tiempos equiespaciados
    i = 0
    for indices in indices_t_deseados:
        P_der = presiones[indices]  #como voy a interpolar linealmente, obtengo la presión a la derecha y a la izquierda, los índices siempre corresponden al valor inmediato superior ¡¡OJO que los t deseados no sean mayores a los del archivo!
        P_izq = presiones[indices-1] #(ver comentario en función obtener índices)
        t_der = tiempos[indices]
        t_izq = tiempos[indices-1]
        pendientes = (P_der - P_izq)/(t_der - t_izq)
        deltas_t = t_deseados[i] - t_izq
        presion_interpolada = P_izq + deltas_t*pendientes
        P_interpoladas.append(presion_interpolada)
        i = i + 1
        print(P_interpoladas)
    return P_interpoladas

def generar_columnas_datos(listas_presiones,n_columnas,decimales): #tengo que generar un bloque de texto con todos los datos en el formato IFORMY
    
    texto = ""
    i = 0
    P1_reducidas = []
    for presiones in listas_presiones:
        for presion in presiones:
            resto = i % n_columnas 
            if resto != n_columnas-1:
                texto = texto + " {:.{}f}".format(presion,decimales)
                P1_reducidas.append(presion)
            else:
                texto = texto + " {:.{}f}\n".format(presion,decimales)
                P1_reducidas.append(presion)
            i = i + 1
    P1_reducidas = np.array(P1_reducidas)
    return (texto,P1_reducidas)

def generar_IFORMY(n_columnas,decimales):
    #EZE: (10F7.4) 10 columnas, F formato decimal, 7 espacios por columna, 4 espacios despues de la coma
    texto = ' IFORMY\n (' + str(n_columnas)+ 'F' + str(decimales + 3) + '.' + str(decimales) + ')\n'
    return texto

def generar_frecuencias(t_min,t_max):
	log_t_min = np.emath.logn(10,t_min)
	log_t_max = np.emath.logn(10,t_max)
	log_t_min = np.floor(log_t_min)
	log_t_max = np.ceil(log_t_max)
	primera_cifra_f_max = '1' #f"{(1/t_min):.0e}"[0]  #saco la primera cifra redondeada de la frec máxima
	primera_cifra_f_min = '1' #f"{(1/t_max):.0e}"[0]
	#print("primera cifra:",primera_cifra_f_max)
	#exp_f_max = int(-log_t_min)  
	#exp_f_min = int(-log_t_max)
	exp_f_max = -1 
	exp_f_min = -3
	f_min = (10**exp_f_min) 
	f_max = (10**exp_f_max)
	return f_min,f_max,primera_cifra_f_min,primera_cifra_f_max,exp_f_min,exp_f_max

def generar_GMNMX(primera_cifra_f_min,primera_cifra_f_max,exp_f_min,exp_f_max):
    texto = " GMNMX     1          "+ primera_cifra_f_min + ".E" + str(exp_f_min) + "\n GMNMX     2          "+ primera_cifra_f_max + ".E" + str(exp_f_max) + '\n' 
    return texto

def generar_texto_completo(archivo_a_abrir,n_frecuencias,min_max_freq,n_int,iformy,nstend,columnas_presiones):
    encabezado_0 = """ TEST DATA SET 1             
 LAST                    1.  *Ultimo juego de datos: -1=false 1=true
"""
    encabezado_1 = """ IWT                     5.  *Modo en el que se van a analizar el peso de los datos 
 IQUAD                   1.  *La solucion ya se ha multiplicado por el valor de los momentos de cada pico
 NERFIT                  0.  *Ningun dato se deja suelto para un mejor ajuste
"""
    encabezado_2 = " NLINF                   1.  *Existe UN unico termino de de Fondo (termino de backgrround constante)\n"
    encabezado_3 = """ PRWT                   -1.  *No mostrar las raices cuadradas de los minimos cuadrados de la funcion de peso
 PRY                    -1.  *No mostrar los arreglos de T y Y y las raices cuadradas de wk
 IPRINT    1             0.  *Muestra la grafica del ajuste de la solucion
 IPRINT    2             4.  *Muestra la grafica del ajuste de la solucion
 IPLRES    2             3.  *muestra los residuos del ajuste de la solucion
 IPLFIT    1             0.  *No muestra la grafica de los datos antes de llegar a la solucion
 IPLFIT    2             3.  *Muestra la grafica de los datos de la solucion
 END                         *Termina los parametros   
"""
    texto_completo_armado = encabezado_0 + min_max_freq + encabezado_1 + n_int + encabezado_2 + iformy  + " NG                    " + str(n_frecuencias)  + '.\n'+ encabezado_3 + nstend + columnas_presiones
    return texto_completo_armado
    
def generar_archivo(archivo_a_abrir,contenido_archivo,ruta):
    
    archivo_a_abrir = archivo_a_abrir.replace(".txt","")
    nombre_archivo_a_escribir = archivo_a_abrir + "_in_contin.txt"
    print('Ruta:',ruta)
    print('Nombre:',nombre_archivo_a_escribir)
    ruta_completa = os.path.join(ruta, nombre_archivo_a_escribir)
    with open(ruta_completa, "w") as archivo:
        archivo.write(contenido_archivo)
    print(nombre_archivo_a_escribir, "terminado")
    return nombre_archivo_a_escribir

def ejecutar_contin(archivo_a_abrir,archivo_generado,ruta):
	archivo_a_abrir = archivo_a_abrir.replace(".txt","")
	nombre_salida_contin = archivo_a_abrir + "_out_contin.txt"
	ruta_completa_in = os.path.join(ruta, archivo_generado)
	ruta_completa_out = os.path.join(ruta, nombre_salida_contin)
	orden_al_sistema = "contin-windows.exe < " +  ruta_completa_in + " > " + ruta_completa_out
	print("ejecutande contin")
	os.system(orden_al_sistema)
	print("terminado")
	return nombre_salida_contin

def obtener_maximos_locales(t,H):
	t = np.array(t)
	H = np.array(H)
	maximos_locales = argrelextrema(H, np.greater)
	tiempos_maximos = t[maximos_locales[0]]
	print(f"Índices de máximos locales: {maximos_locales[0]}")
	#print(tiempos_maximos)
	return tiempos_maximos

#-------------------Funciones Santi-------------------#

def contar_soluciones(lineas):
    inicios_soluciones = []
    finales_soluciones = []

    # Inicializar variables
    num_soluciones = 0
    inicio_solucion = None
    posiciones_fiteo = []
    # Recorrer las líneas del archivo
    for i, linea in enumerate(lineas):
        if "0PLOT OF DATA (O) AND FIT TO DATA (X).  ORDINATES LISTED ARE FIT VALUES." in linea:
            # Encontramos el inicio de una solución
            inicio_solucion = i
        elif "      ALPHA    ALPHA/S(1)     OBJ. FCTN.       VARIANCE      STD. DEV.    DEG FREEDOM    PROB1 TO REJECT    PROB2 TO REJECT" in linea:
            if inicio_solucion is not None:
                posiciones_fiteo.append(i)
        elif "0LINEAR COEFFICIENTS" in linea:
            # Encontramos el final de una solución
            if inicio_solucion is not None:
                num_soluciones += 1
                print(f"Solución {num_soluciones}: Líneas {inicio_solucion + 1} - {i + 1}")
                inicios_soluciones.append(inicio_solucion)
                finales_soluciones.append(i)
                inicio_solucion = None
    inicios_soluciones = np.array(inicios_soluciones)
    posiciones_fiteo = np.array(posiciones_fiteo)
    posiciones_fiteo = posiciones_fiteo - inicios_soluciones
    print(f"Total de soluciones encontradas: {num_soluciones}")
    print(f"Posiciones: {posiciones_fiteo}")
    return(inicios_soluciones,finales_soluciones,posiciones_fiteo)

def abrir_archivo(archivo):
    with open(archivo, 'r') as f:
        lineas = f.readlines()
    return lineas

def cargar_solucion(archivo_completo,linea_inicial, linea_final):
	solucion_cargada = archivo_completo[linea_inicial:linea_final]
	return solucion_cargada

def normalizar_H(frec,H):
	t = 1/frec
	ln_t = np.log(t)
	delta_ln_t = np.diff(ln_t)
	prom_d_ln_t = np.average(delta_ln_t)
	H_integrado = (H[0] + H[-1])/2 + np.sum(H[1:-1])*-prom_d_ln_t
	H_norm = H/H_integrado
	H_int = []
	sumatoria = 0
	print("la integral de H da: ",H_integrado)
	return H_norm
	
def integrar_H(frec,H):
	t = 1/frec
	ln_t = np.log(t)
	delta_ln_t = np.diff(ln_t)
	prom_d_ln_t = np.average(delta_ln_t)
	for i in range(len(H)):
		sumatoria = sumatoria + H_norm[i]
		H_int.append(sumatoria)
	H_int = np.array(H_int)*-prom_d_ln_t
	plt.plot(frec,H_norm,color='blue')
	plt.plot(frec,H_int,color='red')
	plt.xscale('log')
	plt.show()
	
def obtener_Es(frec,H,w_min,w_max,n_puntos,theta,delta_P0):
	ln_w_max,ln_w_min = np.log(w_max) , np.log(w_min)
	ln_omegas = np.linspace(ln_w_min,ln_w_max,n_puntos)
	omegas = np.exp(ln_omegas)
	t = 1/frec
	ln_t = np.log(t)
	delta_ln_t = np.diff(ln_t)
	prom_d_ln_t = np.average(delta_ln_t)
	
	#print(np.average(delta_ln_t),np.std(delta_ln_t),np.min(delta_ln_t),np.max(delta_ln_t))
	E_prima = []
	E_segunda = []
	H_integral = []
	for omega in omegas:
		wt = omega*t
		intE_seg = H*wt/(1 + wt*wt)
		intE_pri = intE_seg*wt
		E_pri = (intE_pri[0] + intE_pri[-1])/2 + np.sum(intE_pri[1:-1])
		E_seg = (intE_seg[0] + intE_seg[-1])/2 + np.sum(intE_seg[1:-1])
		int_H = (H[0] + H[-1])/2 + np.sum(H[1:-1])
		E_prima.append(E_pri)
		E_segunda.append(E_seg)
		H_integral.append(int_H)
	E_prima = np.array(E_prima)*delta_P0*-prom_d_ln_t/theta
	E_segunda = np.array(E_segunda)*delta_P0*-prom_d_ln_t/theta
	H_int = np.array(H_integral)*-prom_d_ln_t
	'''
	#plt.plot(omegas,E_prima,color='blue')
	#plt.plot(omegas,E_segunda,color='red')
	#plt.plot(omegas,H)
	plt.plot(omegas,H_int)
	plt.xscale('log')
	plt.show()
	'''
	return(omegas,E_prima,E_segunda)

def graficar_soluciones(x_data,y_data,x_fit,y_fit,x_dist,y_dist,alphas,w,integrales1,integrales2,error_fit):
	# Función para actualizar el gráfico
	num_sets = len(alphas)
	current_set = 0
	def update_plot_plus(event):
		nonlocal current_set
		current_set = (current_set + 1) % num_sets
		line1.set_ydata(y_fit[current_set])
		line3.set_ydata(error_fit[current_set])
		line2.set_ydata(y_dist[current_set])
		line4.set_ydata(integrales1[current_set])
		line5.set_ydata(integrales2[current_set])
		
		t_max = obtener_maximos_locales(x_dist[current_set],y_dist[current_set])
		print(t_max)	
		ax1.set_xlabel('τ [s]')
		ax1.set_ylabel('Π [mN/m]')
		ax2.set_xlabel('w [Hz]')
		ax2.set_ylabel('H [w]')
		ax4.set_xlabel('w [Hz]')
		ax4.set_ylabel('E\' , E\'\' [mN/m]')
		ax1.set_ylim(bottom=min(y_fit[current_set])-max(y_fit[current_set])*0.1,top=1.1*max(y_fit[current_set]))
		#ax1.set_ylim(ymin=-0.3, ymax=1.1)
		ax2.set_ylim(bottom=-0.1*min(y_dist[current_set]),top=1.1*max(y_dist[current_set]))
		ax3.set_ylim(bottom=1.1*min(error_fit[current_set]),top=1.1*max(error_fit[current_set]))

		if max(integrales1[current_set]) >= max(integrales2[current_set]):
			ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales1[current_set]))
		else:
			ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales2[current_set]))
		fig.suptitle(f'Número de soluciones: {len(alphas)} \n Solución nº:{(current_set+1)} \n Alpha={alphas[current_set]:.2e}')
		fig.canvas.draw()

	def update_plot_minus(event):
		nonlocal current_set
		current_set = (current_set - 1) % num_sets
		line1.set_ydata(y_fit[current_set])
		line3.set_ydata(error_fit[current_set])
		line2.set_ydata(y_dist[current_set])
		line4.set_ydata(integrales1[current_set])
		line5.set_ydata(integrales2[current_set])
		
		t_max = obtener_maximos_locales(x_dist[current_set],y_dist[current_set])
		print(t_max)	
		ax1.set_xlabel('τ [s]')
		ax1.set_ylabel('Π [mN/m]')
		ax2.set_xlabel('w [Hz]')
		ax2.set_ylabel('H [w]')
		ax4.set_xlabel('w [Hz]')
		ax4.set_ylabel('E\' , E\'\' [mN/m]')
		ax1.set_ylim(bottom=min(y_fit[current_set])-max(y_fit[current_set])*0.1,top=1.1*max(y_fit[current_set]))
		#ax1.set_ylim(ymin=-0.3, ymax=1.1)
		ax2.set_ylim(bottom=-0.1*min(y_dist[current_set]),top=1.1*max(y_dist[current_set]))
		ax3.set_ylim(bottom=1.1*min(error_fit[current_set]),top=1.1*max(error_fit[current_set]))

		if max(integrales1[current_set]) >= max(integrales2[current_set]):
			ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales1[current_set]))
		else:
			ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales2[current_set]))
		fig.suptitle(f'Número de soluciones: {len(alphas)} \n Solución nº:{(current_set+1)} \n Alpha={alphas[current_set]:.2e}')
		fig.canvas.draw()
    
    # Crear la figura y los subplots
	fig, ((ax1, ax2), (ax3,ax4)) = plt.subplots(2, 2, figsize=(10, 8))
	fig.suptitle(f'Número de soluciones: {len(alphas)} \n Solución nº:{(current_set+1)} \n Alpha={alphas[current_set]:.2e}')
	scatter, = ax1.plot(x_data, y_data,color='red',marker='o',linestyle='',mfc='none')
	line1, = ax1.plot(x_fit[current_set], y_fit[current_set], color='#1f77b4',label=f"Ajuste {current_set + 1}")
	line2, = ax2.plot(x_dist[current_set], y_dist[current_set], label=f"Distribución {current_set + 1}")
	line3, = ax3.plot(x_fit[current_set], error_fit[current_set], color='#1f77b4',label=f"Ajuste {current_set + 1}")
	line4, = ax4.plot(w[current_set], integrales1[current_set])
	line5, = ax4.plot(w[current_set], integrales2[current_set], label=f"Modulos de viscoelasticidad {current_set + 1}")
	t_max = obtener_maximos_locales(x_dist[current_set],y_dist[current_set])
	print(t_max)
	ax1.set_xlabel('τ [s]')
	ax1.set_ylabel('Π [mN/m]')
	ax2.set_xlabel('w [Hz]')
	ax2.set_ylabel('H [τ]')
	ax3.set_xlabel('τ [s]')
	ax3.set_ylabel('Residuals [mN/m]')
	ax4.set_xlabel('w [Hz]')
	ax4.set_ylabel('E\' , E\'\' [mN/m]')
	ax1.semilogx()
	ax2.semilogx()
	ax3.semilogx()
	ax3.axhline(y=0, color='black')
	ax4.semilogx()
	ax1.set_ylim(bottom=min(y_fit[current_set])-max(y_fit[current_set])*0.1,top=1.1*max(y_fit[current_set]))
	ax2.set_ylim(bottom=-0.1*min(y_dist[current_set]),top=1.1*max(y_dist[current_set]))
	ax3.set_ylim(bottom=1.1*min(error_fit[current_set]),top=1.1*max(error_fit[current_set]))
      
	if max(integrales1[current_set]) >= max(integrales2[current_set]):
		ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales1[current_set]))
	else:
		print("integrales1 y 2:",(integrales1[current_set]),(integrales2[current_set]))
		ax4.set_ylim(bottom=-0.1*min(integrales1[current_set]),top=1.1*max(integrales2[current_set]))  #editado marcos integrales1 -> integrales2
	# Crear el botón
	ax_button_plus = plt.axes([0.1, 0.01, 0.10, 0.04])
	button_plus = Button(ax_button_plus, "Siguiente")
	button_plus.on_clicked(update_plot_plus)
	# Crear el botón
	ax_button_minus = plt.axes([0.01, 0.01, 0.10, 0.04])
	button_minus = Button(ax_button_minus, "Anterior")
	button_minus.on_clicked(update_plot_minus)
      
	plt.show()

def error_ajuste(datos,ajuste):
	# Calcular el error cuadrático medio
    errores_cuadrados = datos - ajuste
    return errores_cuadrados

def extraer_datos(datos_P1,numero_de_soluciones,contenido_arhivo,frecuencias_min_y_max):
    contador_alpha=0
    encuentra_alpha=False
    alphas=[]
    lineas_a_omitir = 3
    lineas_omitidas = 0
    datos_error_dist = []
    max_caracteres = 8
    encuentra_fit = True
    datos_finales_fit_x = []
    datos_finales_fit_y = []
    datos_finales_dist_x = []
    datos_finales_dist_y = []
    datos_finales_errores = []
    filas = 4
    integrales1 = []
    integrales2 = []
    w = []
    error = []
    for i in range(0,len(numero_de_soluciones[0])):
        bloque_en_uso = cargar_solucion(contenido_arhivo,numero_de_soluciones[0][i],numero_de_soluciones[1][i])
        datos_x_fit	= []
        datos_y_fit	= []
        datos_x_dist = []
        datos_y_dist = []
        datos_error_dist = []

        if i == len(numero_de_soluciones[0])-1:
            filas = filas+1
        for j, linea in enumerate(bloque_en_uso):
            if j == 0:
                encuentra_fit = True
            if j >= 3 and j<= numero_de_soluciones[2][i]-filas and encuentra_fit:
                dato_x_a_extraer = linea.strip().split()[1][:max_caracteres]
                dato_y_a_extraer = linea.strip().split()[0]
                try:
                    datos_x_fit.append(float(dato_x_a_extraer))
                    datos_y_fit.append(float(dato_y_a_extraer))
                except ValueError:
                    print("ERROR: no se puede convertir la línea",linea)
                    pass
            if j == numero_de_soluciones[2][i]-4 and encuentra_fit:
                encuentra_fit = False
            if encuentra_alpha:
                encuentra_alpha = False
                linea = linea.replace('*',' ')
                alphas.append(float(linea.split()[0]))
            if "ALPHA" in linea:
                encuentra_alpha = True
                contador_alpha += 1
            if j >= numero_de_soluciones[2][i]+5 and encuentra_fit == False:
                datos_x_dist.append(float(linea.strip().split()[2][:max_caracteres]))
                datos_y_dist.append(float(linea.strip().split()[0]))
                datos_error_dist.append(float(linea.strip().split()[1].replace('D','E')))
        datos_x_dist = np.array(datos_x_dist)
        datos_y_dist = np.array(datos_y_dist)
        #normalizo la distribución
        datos_y_dist = normalizar_H(datos_x_dist,datos_y_dist)
        w_min,w_max = frecuencias_min_y_max[0]/10,frecuencias_min_y_max[1]*10
        Es_obtenidos = obtener_Es(datos_x_dist,datos_y_dist,w_min,w_max,300,1,1)
        w.append(Es_obtenidos[0]) 
        integrales1.append(Es_obtenidos[1]) #frec,H,w_min,w_max,n_puntos,theta
        integrales2.append(Es_obtenidos[2])
        error.append(error_ajuste(datos_P1,datos_y_fit))
        datos_finales_fit_x.append(datos_x_fit)
        datos_finales_fit_y.append(datos_y_fit)
        datos_finales_dist_x.append(datos_x_dist)
        datos_finales_dist_y.append(datos_y_dist)
        datos_finales_errores.append(datos_error_dist)
        
    print(contador_alpha,alphas)
    print('La solución elegida es:', alphas[-1])

    datos_finales_dist_x=np.array(datos_finales_dist_x)

    return (datos_finales_dist_x,datos_finales_dist_y,datos_finales_fit_x,datos_finales_fit_y,alphas,w,integrales1,integrales2,error)

def selector_datos(x,y):
    def extraer_intervalos():
        global datos_seleccionados
        if intervalos:
            datos_seleccionados = []
            for inicio, fin in intervalos:
                datos_seleccionados.extend(y[(x >= inicio) & (x <= fin)])
            messagebox.showinfo("Selector de Datos por Intervalos","Datos Seleccionados correctamente")
            #messagebox.showinfo("Datos Seleccionados", f"Datos en los intervalos {intervalos}:\n{datos_seleccionados}")
        else:
            messagebox.showwarning("Advertencia", "No se ha seleccionado ningún intervalo.")

    def restablecer_grafico():
        global intervalos
        global datos_seleccionados
        intervalos = []
        datos_seleccionados = []
        ax.clear()
        ax.plot(x, y, '-o')
        ax.semilogx()
        ax.set_title("Selecciona intervalos")
        plt.draw()

    def verificar_solapamiento(nuevo_inicio, nuevo_fin):
        for inicio, fin in intervalos:
            if not (nuevo_fin <= inicio or nuevo_inicio >= fin):
                return True
        return False

    # Crear la ventana principal de tkinter
    root = tk.Tk()
    root.title("Selector de Datos por Intervalos")
    root.geometry("400x200")

    # Crear la figura de matplotlib y el selector de intervalos
    fig, ax = plt.subplots()
    ax.plot(x, y, '-o')
    ax.set_title("Selecciona intervalos")
    ax.semilogx()
    def onselect(xmin, xmax):
        if verificar_solapamiento(xmin, xmax):
            messagebox.showwarning("Advertencia", "El intervalo seleccionado se solapa con uno existente.")
        else:
            intervalos.append((xmin, xmax))
            ax.axvspan(xmin, xmax, color='red', alpha=0.5)
            plt.draw()
            print(f"Intervalo seleccionado: [{xmin}, {xmax}]")

    span = SpanSelector(ax, onselect, 'horizontal', useblit=True, interactive=True, props=dict(alpha=0.5, facecolor='red'))

    # Crear los botones en tkinter para extraer y restablecer los intervalos
    boton_extraer = tk.Button(root, text="Extraer Intervalos", command=extraer_intervalos)
    boton_extraer.pack(pady=10)

    boton_restablecer = tk.Button(root, text="Restablecer Gráfico", command=restablecer_grafico)
    boton_restablecer.pack(pady=10)

    # Mostrar la figura de matplotlib y ejecutar el bucle principal de tkinter
    plt.show()
    root.mainloop()
    return intervalos,datos_seleccionados
