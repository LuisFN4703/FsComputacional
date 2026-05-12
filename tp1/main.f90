program main
    use mis_subrutinas
    use mod_lectura
    use mod_tp1
    implicit none

    integer :: n, n_int
    real(dp), allocatable :: x(:), y(:)
    real(dp), allocatable :: x_int(:), y_int(:), dy_int(:)
    real(dp) :: h_paso

    call leer_input(n, x, y)

    ! ==========================================
    ! PASO h = 0.2
    ! ==========================================
    h_paso = 0.2_dp
    n_int = int((x(n) - x(1)) / h_paso) + 1
    allocate(x_int(n_int), y_int(n_int), dy_int(n_int))

    call inciso_1(n, x, y, h_paso, "outputs/interpolacion_h02.out", n_int, x_int, y_int)
    call inciso_2(n, x, y, h_paso, "outputs/errores_h02.out")
    call inciso_3(n_int, x_int, y_int, "outputs/derivadas_h02.out", dy_int)
    call inciso_4(n_int, x_int, dy_int, y(1), "outputs/integracion_h02.out")

    deallocate(x_int, y_int, dy_int)

	print *
    ! ==========================================
    ! PASO h = 0.04
    ! ==========================================
    h_paso = 0.04_dp
    n_int = int((x(n) - x(1)) / h_paso) + 1
    allocate(x_int(n_int), y_int(n_int), dy_int(n_int))

    call inciso_1(n, x, y, h_paso, "outputs/interpolacion_h004.out", n_int, x_int, y_int)
    call inciso_2(n, x, y, h_paso, "outputs/errores_h004.out")
    call inciso_3(n_int, x_int, y_int, "outputs/derivadas_h004.out", dy_int)
    call inciso_4(n_int, x_int, dy_int, y(1), "outputs/integracion_h004.out")

    deallocate(x_int, y_int, dy_int, x, y)

    print *, "TP1: Proceso completado para ambos pasos de h."
end program main
