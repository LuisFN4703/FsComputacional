subroutine desviacion_max(n, o, h1, h2, max)
use mis_subrutinas, only: desv_max
implicit none
	integer, intent(in) :: n
	real(8), intent(in) :: o(n), h1(n), h2(n)
	real(8), intent(out) :: max(3)
	real(8) :: ref
	
	ref = 0.0
	call desv_max(o, n, ref, max(1))
	call desv_max(h1, n, ref, max(2))
	call desv_max(h2, n, ref, max(3))
	

end subroutine desviacion_max
