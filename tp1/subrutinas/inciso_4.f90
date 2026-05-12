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
    
    y_rec(1) = y_inicial
    
	do i = 2, n_int
    	call trapecio(i, x_int(1:i), dy_int(1:i), integral_temp)
    	y_rec(i) = y_inicial + integral_temp
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
    print *, "Resultados de integración en: ", trim(filename)
    
end subroutine inciso_4
