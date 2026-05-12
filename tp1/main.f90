program main
use mis_subrutinas
use mod_lectura  
implicit none
	integer :: n, n_out
    real(dp), allocatable :: x(:), y(:)
    real(dp), allocatable :: x_int(:), y_spl_int(:), dy_central(:)
    real(dp) :: h1

	h1 = 0.2_dp
    call leer_input(n, x, y)

	n_out = int((x(n) - x(1)) / h1) + 1
    allocate(x_int(n_out), y_spl_int(n_out), dy_central(n_out))
    
    call inciso_1(n, x, y, h1, "interpolacion.out", n_out, x_int, y_spl_int)
    call inciso_2(n, x, y, h1)
	call inciso_3(n_out, x_int, y_spl_int, "derivadas_h02.out")
	call inciso_4(n_out, x_int, dy_central, y(1), "integracion_h02.out")

    deallocate(x, y, x_int, y_spl_int, dy_central)
end program main
