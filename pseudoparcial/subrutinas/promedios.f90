subroutine promedios(n, o, h1, h2, prom)
use mis_subrutinas, only: prom_vector
implicit none
	integer, intent(in) :: n
	real(8), intent(in) :: o(n), h1(n), h2(n)
	real(8), intent(out) :: prom(3)

	call prom_vector(o, n, prom(1))
	call prom_vector(h1, n, prom(2))
	call prom_vector(h2, n, prom(3))

end subroutine promedios
