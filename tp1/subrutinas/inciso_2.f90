subroutine inciso_2(n, x, y, h)
    use mod_tp1
    use interpolacion
    implicit none
    
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n), y(n)
    real(dp), intent(in) :: h
    
    integer :: i, n_puntos, m, u_err
    real(dp) :: x_actual, y_int, y_exa, err_rel
    real(dp) :: dd(n, n)
    logical :: en_intervalo(3)
    real(dp) :: inicio_intervalo(3)
    character(len=20) :: metodos(3)

    metodos = [character(len=20) :: "Lagrange", "Newton  ", "Splines "]
    u_err = 15 
    
    call generar_tabla_newton(n, x, y, dd)
    
    n_puntos = int((x(n) - x(1)) / h) + 1
    
    open(unit=u_err, file="errores.out", status='replace')
    
    write(u_err, *) "=== ANALISIS DE INTERVALOS CON ERROR > 10% (h =", h, ") ==="
    write(u_err, *) ""

    do m = 1, 3 ! Iteramos por cada método
        write(u_err, '(A, A)') "Metodo: ", trim(metodos(m))
        en_intervalo(m) = .false.
        
        do i = 1, n_puntos
            x_actual = x(1) + dble(i-1) * h
            if (i == n_puntos) x_actual = x(n)
            
            ! Selección de método
            select case (m)
            case (1); y_int = lagrange(x_actual, n, x, y)
            case (2); call evaluar_newton(n, x, dd, x_actual, y_int)
            case (3); call splines_cubicos(n, x, y, x_actual, y_int)
            end select
            
            y_exa = y_exacta(x_actual)
            
            ! Evitamos división por cero en x=0 (y_exacta(0)=0)
            if (abs(y_exa) > 1.0e-12_dp) then
                err_rel = abs((y_exa - y_int) / y_exa)
            else
                err_rel = 0.0_dp
            end if

            ! Lógica de detección de intervalos críticos
            if (err_rel > 0.10_dp) then
                if (.not. en_intervalo(m)) then
                    inicio_intervalo(m) = x_actual
                    en_intervalo(m) = .true.
                end if
            else
                if (en_intervalo(m)) then
                    write(u_err, '(A, F8.3, A, F8.3, A)') "  -> Intervalo: [", inicio_intervalo(m), " ; ", x_actual, "]"
                    en_intervalo(m) = .false.
                end if
            end if
        end do
        
        ! Si el error sigue siendo alto al final del dominio, cerramos el intervalo
        if (en_intervalo(m)) then
            write(u_err, '(A, F8.3, A, F8.3, A)') "  -> Intervalo: [", inicio_intervalo(m), " ; ", x(n), "]"
        end if
        write(u_err, *) "" ! Espacio entre métodos
    end do

    close(u_err)
    print *, "Analisis de errores completado. Resultados guardados en errores.out"

end subroutine inciso_2
