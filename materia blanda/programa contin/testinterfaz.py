import tkinter as tk
from tkinter import filedialog, messagebox
from tkinter import filedialog as fd
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.widgets import Button
from scipy.signal import argrelextrema
from matplotlib.widgets import SpanSelector
from tkinter import messagebox
import funciones_contin as mf

intervalos = []
t = []
P1 = []
archivo_a_abrir = None
ruta = None
t_finales = []
t_inicial = []

def funcion_1():
    global root
    global intervalos
    global t
    global P1
    global archivo_a_abrir
    global t_finales 
    global ruta
    
    delta_t_0 = t[1]-t[0]		#es el menor delta de tiempo entre el primer punto y el segundo (no podemos meter t = 0 s al contin)
    print("minma dif de t:",delta_t_0)
    #sección particular para medidas equiespaciadas en 10s ******************
    ultimo_t = t[-1]
    print("Tiempo total de la medida:",ultimo_t,"segundos")
    n_puntos_de_a_10s = int(ultimo_t/10)
    print("cantidad de puntos si el espaciado son 10s:",n_puntos_de_a_10s)
    ## santi
    n_puntos_de_a_10s = len(t)
    puntos_por_grupo = int(n_puntos_de_a_10s)
    	#t[-2] #para evitar que se haga lío con el último punto por redondeo o algo
    t_finales = t_finales
    print("tiempos finales:",t_finales)
    cantidad_de_puntos = len(t)

    puntos_por_intervalo = [int(n_puntos_de_a_10s/4),int(n_puntos_de_a_10s/4),int(n_puntos_de_a_10s/4),int(n_puntos_de_a_10s/4)]
    print("p por intervalo:",puntos_por_intervalo)
    t_deseados = mf.generar_t_intervalos(delta_t_0,puntos_por_intervalo,t_finales)
    #print("AHHHHHHH",t_finales,type(t_finales),type(t_finales[0]))
    print("tiempos deseados",t_deseados)
    indices = mf.obt_indices_de_tiempos_deseados(t,t_deseados)
    #print(indices)
    p_interpoladas = mf.interpolar_presiones(t_deseados,indices,t,P1)
    nstend = mf.generar_NSTEND(puntos_por_intervalo,delta_t_0,t_finales)
    print("NSTEND:",nstend)
    n_int = mf.generar_NINTT(puntos_por_intervalo)
    print(n_int)
    columnas,decimales = 10,4
    iformy = mf.generar_IFORMY(columnas,decimales)
    print("IFORMY:",iformy)
    columnas_presiones, P1_reducidas = mf.generar_columnas_datos(p_interpoladas,columnas,decimales)
    #print(columnas_presiones)
    frecuencias_min_y_max = mf.generar_frecuencias(t[1],t[-1])
    print("frec min y max:",frecuencias_min_y_max)
    min_max_freq = mf.generar_GMNMX(frecuencias_min_y_max[2],frecuencias_min_y_max[3],frecuencias_min_y_max[4],frecuencias_min_y_max[5])
    n_frecuencias = 250
    #print(n_puntos)
    archivo_entrada_completo = mf.generar_texto_completo(archivo_a_abrir,n_frecuencias,min_max_freq,n_int,iformy,nstend,columnas_presiones)
    #print("\n\n TEXTO FINALIZADO:\n",archivo_entrada_completo)
        
    #t,P1 = leer_archivo(archivo_a_abrir)
    archivo_generado = mf.generar_archivo(archivo_a_abrir,archivo_entrada_completo,ruta)
    salida_contin = mf.ejecutar_contin(archivo_a_abrir,archivo_generado,ruta)

    #-------------------MAIN Santi-------------------#
    # Ejemplo de uso
    archivo_texto = salida_contin  # Reemplaza con la ruta de tu archivo
    contenido_arhivo = mf.abrir_archivo(archivo_texto)
    inicios_y_finales = mf.contar_soluciones(contenido_arhivo)

    (datos_finales_dist_x,datos_finales_dist_y,datos_finales_fit_x,datos_finales_fit_y,alphas,w,integrales1,integrales2,error) = mf.extraer_datos(P1_reducidas,inicios_y_finales,contenido_arhivo,frecuencias_min_y_max)

    #sección para guardar los archivos
    nombre_archivo_H = archivo_a_abrir + "_H.txt"
    nombre_archivo_modulos = archivo_a_abrir + "_modulos.txt"
    encabezado_H = ['w [Hz]','H(t)']
    encabezado_modulos = ['w [Hz]','Elastic moduli (E \')','Loss moduli (E\'\')']

    encabezados = ['w [Hz]','H(t)','w [Hz]','Elastic moduli (E \')','Loss moduli (E\'\')']
    matriz_H = np.column_stack((datos_finales_dist_x[-1], datos_finales_dist_y[-1]))
    matriz_modulos = np.column_stack((w[-1],integrales1[-1],integrales2[-1]))

    matriz_H = np.vstack((encabezado_H, matriz_H))
    matriz_modulos = np.vstack((encabezado_modulos, matriz_modulos))

    # Guarda la matriz con encabezados en un archivo de texto
    np.savetxt(os.path.join(ruta,nombre_archivo_H), matriz_H, delimiter='\t',fmt='%s')
    #np.savetxt(os.path.join(ruta,nombre_archivo_modulos), matriz_modulos, delimiter='\t',fmt='%s')
    nombre_archivo_H_completo = archivo_a_abrir + "_H_todos.txt"
    matriz_todos_H = np.column_stack((datos_finales_dist_x[0], datos_finales_dist_y[0]))

    cantidad_de_espectros = len(datos_finales_dist_y)
    for i in range(1,cantidad_de_espectros):
        matriz_todos_H = np.column_stack((matriz_todos_H, datos_finales_dist_y[i]))
        
    nombres_columnas =  "w (Hz)"
    for i in range(cantidad_de_espectros):
        nombres_columnas = nombres_columnas + '\tH_' + str(i+1)
    np.savetxt(os.path.join(ruta,nombre_archivo_H_completo),matriz_todos_H,delimiter='\t',fmt='%s',header=nombres_columnas, comments='')

    matriz_todos_los_modulos = np.column_stack((w[0],integrales1[0]))
    #print("primera columna stackeada")
    matriz_todos_los_modulos = np.column_stack((matriz_todos_los_modulos,integrales2[0]))
    for i in range(1,cantidad_de_espectros):
        matriz_todos_los_modulos = np.column_stack((matriz_todos_los_modulos,integrales1[i]))
        #print(i,"columna 1 stackeada")
        matriz_todos_los_modulos = np.column_stack((matriz_todos_los_modulos,integrales2[i]))
        #print(i,"columna 2 stackeada")

    nombres_columnas =  "w (Hz)"
    for i in range(cantidad_de_espectros):
        nombres_columnas = nombres_columnas + '\tE\'_' + str(i+1) + '\tE\'\'_' + str(i+1)
    nombre_archivo_H_completo = archivo_a_abrir + "_E_todes.txt"
    np.savetxt(os.path.join(ruta,nombre_archivo_H_completo),matriz_todos_los_modulos,delimiter='\t',fmt='%s',header=nombres_columnas, comments='')

    # Configuración inicial
    mf.graficar_soluciones(t,P1,datos_finales_fit_x,datos_finales_fit_y,datos_finales_dist_x,datos_finales_dist_y,alphas,w,integrales1,integrales2,error)

    # Combina los arrays en una matriz
    print("datos_finales_dist_x[-1]: ",len(datos_finales_dist_x[-1]))
    print("datos_finales_dist_y[-1]",len(datos_finales_dist_y[-1]))
    print("integrales1[-1]",len(integrales1[-1]))
    print("integrales2[-1]",len(integrales2[-1]))

def funcion_2():
    global root
    global etiqueta_intervalo
    global intervalos
    global t
    global P1
    global t_finales
    global ruta
       # Lista para almacenar los intervalos seleccionados

    intervalos = []
    datos_seleccionados = []
    t_finales = []
    def selector_datos(x,y):
        def extraer_intervalos():
            nonlocal datos_seleccionados
            global t_finales
            if intervalos:
                datos_seleccionados = []
                for inicio, fin in intervalos:
                    datos_seleccionados.extend(y[(x >= inicio) & (x <= fin)])
                    t_finales.append(fin)
                t_finales = sorted(t_finales)
                messagebox.showinfo("Selector de Datos por Intervalos","Datos Seleccionados correctamente")
                plt.close('all')
                root2.destroy()
                t_finales_formateados = [f"{num:.2f}" for num in t_finales]
                etiqueta_t_finales.config(text=f"Nuevos tiempos finales: {t_finales_formateados}")
                #messagebox.showinfo("Definir intervalos", "Ejecutando función 2")
                return intervalos
                #messagebox.showinfo("Datos Seleccionados", f"Datos en los intervalos {intervalos}:\n{datos_seleccionados}")
            else:
                messagebox.showwarning("Advertencia", "No se ha seleccionado ningún intervalo.")

        def restablecer_grafico():
            global intervalos
            global t_finales
            nonlocal datos_seleccionados
            intervalos = []
            datos_seleccionados = []
            t_finales = []
            ax.clear()
            ax.plot(x, y, '-o')
            ax.semilogx()
            ax.set_title("Selecciona intervalos")
            plt.draw()

        def cancelar():
            plt.close('all')
            root2.destroy()

        def verificar_solapamiento(nuevo_inicio, nuevo_fin):
            for inicio, fin in intervalos:
                if not (nuevo_fin <= inicio or nuevo_inicio >= fin):
                    return True
            return False

        # Crear la ventana principal de tkinter
        root2 = tk.Tk()
        root2.title("Selector de Datos por Intervalos")
        root2.geometry("300x150")

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
        boton_extraer = tk.Button(root2, text="Extraer Intervalos", command=extraer_intervalos)
        boton_extraer.pack(pady=10)

        boton_restablecer = tk.Button(root2, text="Restablecer Gráfico", command=restablecer_grafico)
        boton_restablecer.pack(pady=10)

        boton_cancelar = tk.Button(root2, text="Cancelar", command=cancelar)
        boton_cancelar.pack(pady=10)
        # Mostrar la figura de matplotlib y ejecutar el bucle principal de tkinter
        plt.show()
        root2.mainloop()

    selector_datos(t,P1)
    

def cargar_archivo():
    global t
    global P1
    global archivo_a_abrir
    global intervalos
    global t_finales
    global ruta
    archivo_a_abrir = fd.askopenfilename(title= "Seleccionar archivo con tiempos y presiones")
    print("ruta:",archivo_a_abrir)
    ruta = os.path.dirname(archivo_a_abrir)
    t,P1 = mf.leer_archivo(archivo_a_abrir)  #hay que darle la precisión que va a tener por el número de decimales
    t,P1 = mf.shiftear_tiempo(t,P1)
    P1 = mf.normalizar_P(P1)
    #t_finales = [t[-1]/4,t[-1]/2,t[-2]*3/4,t[-1]]
    t_finales = [10,100,1000,2810]
    t_finales_formateados = [f"{num:.2f}" for num in t_finales]
    etiqueta_archivo.config(text=f"Archivo cargado: {archivo_a_abrir.split('/')[-1]}")
    boton_funcion_1.config(state=tk.NORMAL, fg='black')
    boton_funcion_2.config(state=tk.NORMAL, fg='black')
    
    etiqueta_t_finales.config(text=f"Tiempos finales por default: {t_finales_formateados}")
# Crear la ventana principal
root = tk.Tk()
root.title("Interfaz Gráfica")
root.geometry("400x300")

# Botón para cargar el archivo
boton_cargar = tk.Button(root, text="Cargar Archivo", command=cargar_archivo)
boton_cargar.pack(pady=10)


# Botones de procesamiento (inicialmente deshabilitados)
boton_funcion_1 = tk.Button(root, text="Ejecutar CONTIN", command=funcion_1, state=tk.DISABLED, fg='grey')
boton_funcion_1.pack(pady=10)

boton_funcion_2 = tk.Button(root, text="Cambiar intervalos", command=funcion_2, state=tk.DISABLED, fg='grey')
boton_funcion_2.pack(pady=10)

# Etiqueta para mostrar el nombre del archivo cargado
etiqueta_archivo = tk.Label(root, text="No se ha cargado ningún archivo")
etiqueta_archivo.pack(pady=10)

etiqueta_t_finales = tk.Label(root, text=f"")
etiqueta_t_finales.pack(pady=10)
# Ejecutar el bucle principal de tkinter
root.mainloop()
