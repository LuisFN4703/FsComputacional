subroutine leer_input(n, x, y)
use mis_subrutinas
implicit none    
    integer, intent(out) :: n
    real(dp), allocatable, intent(out) :: x(:), y(:)

    call contarfilas(trim("input/funcion.dat"), n)
    allocate(x(n), y(n))
    
    call crear_vectores(trim("input/funcion.dat"), n, x, y)

end subroutine leer_input
