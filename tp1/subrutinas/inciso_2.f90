subroutine inciso_2(n, x, y, h, filename)
    use mod_tp1
    use interpolacion
    implicit none
    
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n), y(n)
    real(dp), intent(in) :: h
    character(len=*), intent(in) :: filename
    
    integer :: i, n_puntos, m, u_err
    real(dp) :: x_actual, y_int, y_exa, err_rel
    real(dp) :: dd(n, n)
    logical :: en_intervalo
    real(dp) :: inicio_intervalo
    character(len=20) :: metodos(3)

    metodos = [character(len=20) :: "Lagrange", "Newton", "Splines"]
    u_err = 15 
    
    ! 1. Calculamos la tabla de Newton una sola vez para eficiencia
    call generar_tabla_newton(n, x, y, dd)
    
    n_puntos = int((x(n) - x(1)) / h) + 1
    
    ! 2. Abrimos el archivo que nos indicó el main
    open(unit=u_err, file=filename, status='replace')
    
    write(u_err, '(A, F6.3, A)') "=== ANALISIS DE INTERVALOS CON ERROR > 10% (h =", h, ") ==="
    write(u_err, *) ""

    do m = 1, 3 ! Iteramos: 1:Lagrange, 2:Newton, 3:Splines
        write(u_err, '(A, A)') "Metodo: ", trim(metodos(m))
        en_intervalo = .false.
        
        do i = 1, n_puntos
            x_actual = x(1) + dble(i-1) * h
            ! Aseguramos que el último punto sea exactamente x(n)
            if (i == n_puntos) x_actual = x(n)
            
            ! Obtenemos la interpolación según el método actual
            select case (m)
            case (1); y_int = lagrange(x_actual, n, x, y)
            case (2); call evaluar_newton(n, x, dd, x_actual, y_int)
            case (3); call splines_cubicos(n, x, y, x_actual, y_int)
            end select
            
            y_exa = y_exacta(x_actual)
            
            ! Cálculo del error relativo evitando división por cero en nodos nulos
            if (abs(y_exa) > 1.0e-12_dp) then
                err_rel = abs((y_exa - y_int) / y_exa)
            else
                err_rel = 0.0_dp
            end if

            ! Detección lógica de intervalos
            if (err_rel > 0.10_dp) then
                if (.not. en_intervalo) then
                    inicio_intervalo = x_actual
                    en_intervalo = .true.
                end if
            else
                if (en_intervalo) then
                    write(u_err, '(A, F8.3, A, F8.3, A)') "  -> Intervalo: [", inicio_intervalo, " ; ", x_actual, "]"
                    en_intervalo = .false.
                end if
            end if
        end do
        
        ! Cerramos el intervalo si terminó en el borde derecho del dominio
        if (en_intervalo) then
            write(u_err, '(A, F8.3, A, F8.3, A)') "  -> Intervalo: [", inicio_intervalo, " ; ", x(n), "]"
        end if
        write(u_err, *) "" 
    end do

    close(u_err)
    print *, "Analisis de error > 10% guardado en: ", trim(filename)

end subroutine inciso_2
