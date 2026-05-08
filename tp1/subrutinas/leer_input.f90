subroutine leer_input(n, x, y)
use mis_subrutinas
implicit none
	integer, intent(out) :: n
	real(dp), intent(out) :: x(n), y(n)

	call contarfilas(trim("input/funcion.dat"), n)
	call crear_vectores(trim("input/funcion.dat"), n, x, y)

end subroutine leer_input
