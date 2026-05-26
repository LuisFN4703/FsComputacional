program main
use metodos_numericos
use funcion
implicit none
	integer :: m, n, i
	real(dp) :: h, t0, tn
	real(dp), allocatable :: t(:), y0(:), y(:,:)

	call read_input(m, h, t0, tn)

	!dimension de los vectores y cantidad de puntos
	n = nint(abs(tn - t0)/h)

	!Determinacion de la dimension de los vectores tiempo, condiciones 
	!iniciales y variables dependientes
	allocate(t(0:n), y0(m), y(0:n, m))

	!Guardado de condiciones iniciales
	call cond_ini(m, y0)
	call rk4_sist(t0, y0, n, h, m, t, y, f)	
	call savedata(n, m, t, y, 'outputs/resultados.out')

	!Liberacion de memoria
	deallocate(t, y0, y)
	
	!graficacion
	call execute_command_line("gnuplot -c graficartp2.gp resultados.out")
	print*, "Programa finalizado, resultados guardados en 'resultados.out' y graficados en 'graficos/Graficotp2.png'"

end program main


