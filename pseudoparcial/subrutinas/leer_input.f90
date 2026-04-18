subroutine leer_input(n)
use modbk_leer_input
implicit none
	integer, intent(out) :: n
	call readMatrix(trim("parcial/inputs/nose.dat"), n, ncol)

end subroutine leer_input
