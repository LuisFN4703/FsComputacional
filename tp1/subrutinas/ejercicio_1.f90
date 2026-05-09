subroutine inciso_1(n, x, y, h, filename)
    use mod_tp1
    use interpolacion
    implicit none
    
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n), y(n)
    real(dp), intent(in) :: h
    character(len=*), intent(in) :: filename

    integer :: i, n_puntos
    real(dp) :: x_actual, y_lag, y_new, y_spl, y_exa
    real(dp) :: dd(n, n) ! Matriz de diferencias divididas

    call generar_tabla_newton(n, x, y, dd)

    n_puntos = int((x(n) - x(1)) / h) + 1
    
    open(unit=1, file=filename, status='replace')
    ! Encabezado para identificar columnas en xmgrace o gnuplot
    write(1, '(A15, 4A22)') "# x", "y_Lagrange", "y_Newton", "y_Spline", "y_Exacta"

    do i = 1, n_puntos
        x_actual = x(1) + dble(i-1) * h
        
        if (i == n_puntos) x_actual = x(n)

        ! --- Llamadas a los interpoladores ---
        y_lag = lagrange(x_actual, n, x, y)
        call evaluar_newton(n, x, dd, x_actual, y_new)
        call splines_cubicos(n, x, y, x_actual, y_spl)

        ! --- Valor analítico ---
        y_exa = y_exacta(x_actual)

        ! 2. Guardado 
        write(1, '(5E22.12)') x_actual, y_lag, y_new, y_spl, y_exa
    end do

    close(1)
    print *, "TP1: Archivo generado: ", trim(filename), " con h =", h

end subroutine inciso_1
