program main
use mis_subrutinas
use mod_lectura  
implicit none
	integer :: n
    real(dp), allocatable :: x(:), y(:)
    real(8) :: h

    call leer_input(n, x, y)
    call inciso_1(n, x, y, 0.20, "interpolacion.out")

    deallocate(x, y)
end program main
