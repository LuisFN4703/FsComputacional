subroutine inciso_1(n, x, y, h, filename, n_out, x_out, y_out)
    use mod_tp1
    use interpolacion
    implicit none
    
    integer, intent(in) :: n, n_out
    real(dp), intent(in) :: x(n), y(n)
    real(dp), intent(in) :: h
    character(len=*), intent(in) :: filename

	real(dp), intent(out) :: x_out(n_out), y_out(n_out)

    integer :: i 
    real(dp) :: x_actual, y_lag, y_new, y_spl, y_exa
    real(dp) :: dd(n, n) ! Matriz de diferencias divididas

    call generar_tabla_newton(n, x, y, dd)
    
    open(unit=18, file=filename, status='replace')
    write(18, '(A15, 4A22)') "# x", "y_Lagrange", "y_Newton", "y_Spline", "y_Exacta"

    do i = 1, n_out
        x_actual = x(1) + dble(i-1) * h
        if (i == n_out) x_actual = x(n)

        ! --- Llamadas a los interpoladores ---
        y_lag = lagrange(x_actual, n, x, y)
        call evaluar_newton(n, x, dd, x_actual, y_new)
        call splines_cubicos(n, x, y, x_actual, y_spl)

        ! --- Valor analítico ---
        y_exa = y_exacta(x_actual)

        ! 2. Guardado 
        write(18, '(5E22.12)') x_actual, y_lag, y_new, y_spl, y_exa
        x_out(i) = x_actual
		y_out(i) = y_spl
    end do

    close(18)
	print '("Interpolación generada en: ", A, " con h = ", F4.2)', trim(filename), h
    
end subroutine inciso_1
