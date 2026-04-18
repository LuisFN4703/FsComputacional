subroutine save_data(nc, tavg, pmax, max_lluvia, indices, pmin, min_temp, cruces, n_cruces)
use mod_meses
implicit none
    integer, intent(in) :: nc
    real(8), intent(in) :: tavg, max_lluvia, min_temp
    integer, intent(in) :: pmax, pmin, n_cruces
    integer, intent(in) :: indices(nc)
    real(8), intent(in) :: cruces(nc)
    integer :: n_mes
    real(8) :: frac
    
    integer :: i, j

	!abre un archivo data.out, si exite lo remplaza y si no, lo crea
    open(unit=38, file="data.out", status="replace")
    
    write(38, '(A)') "--- RESULTADOS CLIMATOLOGICOS DE BAHÍA BLANCA EN 2011 ---"
    write(38, *)
    
    write(38, '(A)') "El promedio de la temperatura media fue:" 
    write(38, '(F8.2, A)') tavg, " (°C)"
    write(38, *)
    
    write(38, '(A)') "El mes que mas llovió fue:"
    write(38, '(A, A, F6.2, A)') trim(meses(pmax)), " y cayeron: ", max_lluvia, " (mm)"
    write(38, *)
    
    write(38, '(A)') "Los meses con humedad menor a 50% fueron:"

    if (maxval(indices) > 0) then
            do i = 1, nc
                if (indices(i) > 0) then
                    write(38, '(A, A)') "  - Mes: ", trim(meses(i))
                end if
            end do
	else
            write(38, *) "Ningun mes registro humedad menor al 50%"
	end if
    write(38, *)
    
    write(38, '(A)') "El mes con la temperatura minima mas baja fue"
	write(38, '(A, A, F6.2)') trim(meses(pmin)), &
        " y dicha temperatura fue: ", &
        min_temp, " (°C)"
	write(38, *)

	if (n_cruces > 0) then
		write(38, '(A)') "Estimacion de cuando a temperatura alcazó los 20 °C:"
		
			do j = 1, n_cruces
				n_mes = int(cruces(j))
				frac = cruces(j) - real(n_mes, 8)
				
				if ( frac <= 0.3 ) then
					write(38, '(A, A)') "A principios de ", trim(meses(n_mes))
					
				else if ( frac > 0.3 .and. frac <= 0.6 ) then
					write(38, '(A, A)') "A mediados de ", trim(meses(n_mes))
					
				else 
					write(38, '(A, A)') "A fines de ", trim(meses(n_mes))
					
				endif
			enddo
		else
		write(28, '(A)') "Las temperaturas no fueron iguales a 20 °C"
	endif
    
    close(38)

end subroutine save_data
