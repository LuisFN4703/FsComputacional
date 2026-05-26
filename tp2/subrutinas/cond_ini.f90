subroutine cond_ini(m, y0)
use mis_subrutinas, only: dp
implicit none
	integer, intent(in) :: m
 	real(dp), intent(inout) :: y0(m)

 	print*, "Introduzca las condiciones iniciales I(0) y Q(0) separadas por un espacio (recordar I = dQ/dt)"
	read*, y0(1), y0(2) 	

end subroutine cond_ini
