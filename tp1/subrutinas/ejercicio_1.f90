subroutine inciso_1(n, x, y, h, archivo_salida)
    use interpolacion
    use mod_tp1
    implicit none
    
    integer, intent(in) :: n
    real*8, intent(in) :: x(n), y(n)
    real*8, intent(in) :: h
    character(len=*), intent(in) :: archivo_salida
    
    integer :: i, n_puntos_nuevos
    real*8 :: x_min, x_max, xint
    real*8 :: y_lag, y_newton, y_spline, y_real
    
    ! Matriz para las diferencias divididas de Newton
    real*8 :: dd(n, n)
    
	call generar_tabla_newton(n, x, y, dd)
    
    ! 2. Determinamos el dominio
	n_puntos_nuevos = int((x(n) - x(1)) / h) + 1
    
    open(unit=30, file=archivo_salida, status='replace')
    write(30, '(A15, 4A17)') "# x", "y_Lagrange", "y_Newton", "y_Spline", "y_Exacta"
    
    do i = 1, n_puntos_nuevos
        xint = x_min + dble(i-1) * h
        
        ! --- Método 1: Lagrange (Function) ---
        y_lag = lagrange(xint, n, x, y)
        
        ! --- Método 2: Newton (Subroutine) ---
        ! Se le pasa la matriz dd para que la use/llene
        call evaluar_newton(n, x, dd, xint, y_newton)
        
        ! --- Método 3: Splines Cúbicos (Subroutine) ---
        call splines_cubicos(n, x, y, xint, y_spline)
        
        ! Valor exacto para comparación
        y_real = y_exacta(xint)
        
        ! Guardamos todo en columnas
        write(30, '(5E17.8)') xint, y_lag, y_newton, y_spline, y_real
    end do
    
    close(30)
    print *, "TP1: Inciso 1 finalizado. Datos en: ", trim(archivo_salida)

end subroutine inciso_1
