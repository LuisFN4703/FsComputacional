subroutine inciso_2(n, x, y, h)
    use mod_tp1
    use interpolacion
    implicit none
    
    integer, intent(in) :: n
    real(dp), intent(in) :: x(n), y(n)
    real(dp), intent(in) :: h
    
    integer :: i, n_puntos, m
    real(dp) :: x_actual, y_int, y_exa, err_rel
    real(dp) :: dd(n, n)
    logical :: en_intervalo(3) ! 1:Lagrange, 2:Newton, 3:Splines
    real(dp) :: inicio_intervalo(3)
    character(len=20) :: metodos(3)

    metodos = [character(len=20) :: "Lagrange", "Newton  ", "Splines "]
    en_intervalo = .false.
    
    ! Preparamos Newton una vez
    call generar_tabla_newton(n, x, y, dd)
    
    n_puntos = int((x(n) - x(1)) / h) + 1
    
    print *, ""
    print *, "=== ANALISIS DE INTERVALOS CON ERROR > 10% (h =", h, ") ==="

    do m = 1, 3 ! Iteramos por cada método
        print *, "Metodo: ", metodos(m)
        en_intervalo(m) = .false.
        
        do i = 1, n_puntos
            x_actual = x(1) + dble(i-1) * h
            if (i == n_puntos) x_actual = x(n)
            
            ! Obtener valor interpolado según el método m
            select case (m)
            case (1); y_int = lagrange(x_actual, n, x, y)
            case (2); call evaluar_newton(n, x, dd, x_actual, y_int)
            case (3); call splines_cubicos(n, x, y, x_actual, y_int)
            end select
            
            y_exa = y_exacta(x_actual)
            
            ! Evitar división por cero en x=0
            if (abs(y_exa) > 1.0e-12_dp) then
                err_rel = abs((y_exa - y_int) / y_exa)
            else
                err_rel = 0.0_dp ! En el nodo (0,0) el error es nulo
            end if

            ! Lógica de detección de intervalos
            if (err_rel > 0.10_dp) then
                if (.not. en_intervalo(m)) then
                    inicio_intervalo(m) = x_actual
                    en_intervalo(m) = .true.
                end if
            else
                if (en_intervalo(m)) then
                    print '(A, F8.3, A, F8.3, A)', "  -> Intervalo: [", inicio_intervalo(m), " ; ", x_actual, "]"
                    en_intervalo(m) = .false.
                end if
            end if
        end do
        
        ! Cerrar intervalo si termina en el borde del dominio
        if (en_intervalo(m)) then
            print '(A, F8.3, A, F8.3, A)', "  -> Intervalo: [", inicio_intervalo(m), " ; ", x(n), "]"
        end if
    end do

end subroutine inciso_2
