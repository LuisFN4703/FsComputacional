subroutine read_input(m, h, t0, tn)
use mis_subrutinas, only: dp
implicit none
	integer, intent(out) :: m
	real(dp), intent(out) :: h, t0, tn

	m = 2 ! Orden de la EDO a resolver
	print*, "Introduzca el paso h:"
	read*, h
	print*, "Introduzca los limites de integracion t0 y tn separados por un espacio"
	read*, t0, tn
	
end subroutine read_input
