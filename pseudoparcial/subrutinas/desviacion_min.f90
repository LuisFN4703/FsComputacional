subroutine desviacion_min(n, o, h1, h2, min)
use mis_subrutinas, only: desv_min
implicit none
	integer, intent(in) :: n
	real(8), intent(in) :: o(n), h1(n), h2(n)
	real(8), intent(out) :: min(3)
	real(8) :: ref
		
	ref = 0.0
	call desv_min(o, n, ref, min(1))
	call desv_min(h1, n, ref, min(2))
	call desv_min(h2, n, ref, min(3))
		
end subroutine desviacion_min
