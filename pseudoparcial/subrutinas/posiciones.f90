subroutine posiciones(n, o, h1, h2)
use mis_subrutinas, only: crear_vectores
implicit none
	integer, intent(in) :: n
	real(8), intent(out) :: o(n), h1(n), h2(n)
	
	call crear_vectores(trim("parcial/inputs/nose.dat"), n, o, h1, h2)

end subroutine posiciones
