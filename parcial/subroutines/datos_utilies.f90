subroutine datos_utiles(nrow, ncol, m)
use subrutinas_parcial
implicit none
	integer, intent(in) :: nrow, ncol
	!integer, parameter :: dp = selected_real_kind(15, 307)
	real(dp), intent(out) :: m(nrow, ncol)


	call matriz_util("input/bahia2011.dat", nrow, ncol, m)
	
end subroutine datos_utiles
