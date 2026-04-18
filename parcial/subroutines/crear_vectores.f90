subroutine crear_vect(nf, nc, m, v1, v2, v3, v4, v5)
use mis_subrutinas
implicit none
	integer, intent(in) :: nf, nc
	integer, parameter :: dp = selected_real_kind(15, 307)
	real(dp), intent(in) :: m(nf, nc)
	real(dp), intent(out) :: v1(nc), v2(nc), v3(nc), v4(nc), v5(nc)
	integer :: i, j

	i=1
	call extraer_fila(nf, nc, m, i, v1)
	i=2
	call extraer_fila(nf, nc, m, i, v2)
	i=3
	call extraer_fila(nf, nc, m, i, v3)
	i=4
	call extraer_fila(nf, nc, m, i, v4)		
	i=5
	call extraer_fila(nf, nc, m, i, v5)
	
end subroutine crear_vect
