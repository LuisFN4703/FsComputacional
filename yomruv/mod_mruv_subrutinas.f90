module mod_mruv_subrutinas
implicit none

contains !contenido: aca van a estar todas las subrutinas que voy a llamar en mi programa principal
!--------------------------------------------------------------------------------------------------------------------------------
! Lee los datos de un archivo .dat y los ubica en dos vectores h y t
	subroutine leer_datos(h, t)
	real, intent(out) :: h(3), t(3)
	integer :: i

	open(20, file='data.dat', status='old')
	do i=1, 3 
		read(20, *) h(i), t(i)
	enddo
	close(20)
	
	return
	end subroutine leer_datos
!--------------------------------------------------------------------------------------------------------------------------------


! calcula la gravedad para cada h y t 
!--------------------------------------------------------------------------------------------------------------------------------
	subroutine calc_g(h, t, g)
		real, intent(in) :: h(3), t(3) !estos son intent(out) pq entran pero no los modifica la subrutina
		real, intent(out) :: g(3) !este, en cambio, lo crea y modifica la subrutina
		integer :: i

		do i=1, 3
			g(i)=2.0*(h(i)/100)/t(i)**2
		enddo
		
	return
	end subroutine calc_g
!--------------------------------------------------------------------------------------------------------------------------------


! calcula el promedio de un vector
!--------------------------------------------------------------------------------------------------------------------------------
	subroutine prom_g(n, g, avg)
	!declaracion de variables
		integer, intent(in) :: n
		real, intent(in) :: g(n)
		real, intent(out) :: avg
		integer i

		avg = 0.0
		
		!calculo del promedio
		do i= 1, n
			avg=avg + g(i)
		enddo
		
		avg = avg/n
		
	return
	end subroutine prom_g
!--------------------------------------------------------------------------------------------------------------------------------
	
end module mod_mruv_subrutinas
