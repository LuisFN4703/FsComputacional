subroutine crear_vect(nrow, ncol, m, v1, v2, v3, v4, v5)
!esta subrutina agarra las filas de una matriz y las convierte en vectores
use subrutinas_parcial
implicit none
	integer, intent(in) :: nrow, ncol
!	integer, parameter :: dp = selected_real_kind(15, 307)
	real(dp), intent(in) :: m(nrow, ncol)
	real(dp), intent(out) :: v1(ncol), v2(ncol), v3(ncol), v4(ncol), v5(ncol)
	integer :: i, j

	i=1
	call extraer_fila(nrow, ncol, m, i, v1)
	i=2
	call extraer_fila(nrow, ncol, m, i, v2)
	i=3
	call extraer_fila(nrow, ncol, m, i, v3)
	i=4
	call extraer_fila(nrow, ncol, m, i, v4)		
	i=5
	call extraer_fila(nrow, ncol, m, i, v5)
	
end subroutine crear_vect
