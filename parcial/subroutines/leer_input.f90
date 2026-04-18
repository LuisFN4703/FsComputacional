subroutine leer_input(nrow, ncol)
use subrutinas_parcial
implicit none
	integer, intent(out) :: nrow, ncol

	call contar_dimensiones(trim("input/bahia2011.dat"), nrow, ncol)
	nrow = nrow - 1
	ncol = ncol - 1

end subroutine leer_input
