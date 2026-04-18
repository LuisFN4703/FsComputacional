subroutine datos_utiles(nf, nc, m)
use mis_subrutinas
implicit none
integer, intent(in) :: nf, nc
integer, parameter :: dp = selected_real_kind(15, 307)
real(dp), intent(out) :: m(nc, nf)


	call matriz_util("input/bahia2011.dat", nf, nc, m)
	
end subroutine datos_utiles
