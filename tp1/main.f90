program main
    use mis_subrutinas
    implicit none

    integer :: n
    real(dp), allocatable :: x(:), y(:) 

    call leer_input(n, x, y)

    print *, "Lectura finalizada. Cantidad de puntos: ", n
    print *, "Primer punto: x =", x(1), ", y =", y(1)
    

    deallocate(x, y)

end program main
