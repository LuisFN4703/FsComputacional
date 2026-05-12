program main
use mis_subrutinas
use mod_lectura  
implicit none
	integer :: n
    real(dp), allocatable :: x(:), y(:)
    real(8) :: h

    call leer_input(n, x, y)
    call inciso_1(n, x, y, 0.2_dp, "interpolacion.out")
    call inciso_2(n, x, y, 0.2_dp)

    deallocate(x, y)
end program main
