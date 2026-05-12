subroutine inciso_4(n_int, x_int, dy_int, y_inicial, filename)
    use mod_tp1
    use derivacion_integracion
    implicit none
    
    integer, intent(in) :: n_int
    real(dp), intent(in) :: x_int(n_int), dy_int(n_int)
    real(dp), intent(in) :: y_inicial
    character(len=*), intent(in) :: filename
    
    real(dp) :: y_rec(n_int), y_exa, err_rel, integral_temp
    integer :: i
    
    ! 1. El primer punto es la condición inicial original y(x1)
    y_rec(1) = y_inicial
    
    ! 2. Recuperamos la función integrando la derivada acumuladamente
    ! Usamos la subrutina 'trapecio' de tu módulo
	y_rec(1) = y_inicial
	do i = 1, n_int - 1
		! Calculamos solo el área del trapecio "nuevo"
		h = x_int(i+1) - x_int(i)
		y_rec(i+1) = y_rec(i) + (h / 2.0_dp) * (dy_int(i) + dy_int(i+1))
	end do
    
    ! 3. Guardado de resultados y cálculo de error relativo
    open(unit=14, file=filename, status='replace')
    write(14, '(A15, 3A22)') "# x", "y_Recuperada", "y_Exacta", "Error_Relativo"
    
    do i = 1, n_int
        y_exa = y_exacta(x_int(i))
        
        ! Evitamos división por cero en x=0
        if (abs(y_exa) > 1.0e-12_dp) then
            err_rel = abs((y_exa - y_rec(i)) / y_exa)
        else
            err_rel = 0.0_dp
        end if
        
        write(14, '(4E22.12)') x_int(i), y_rec(i), y_exa, err_rel
    end do
    
    close(14)
    print *, "TP1: Inciso 4 completado. Resultados en: ", trim(filename)
    
end subroutine inciso_4
