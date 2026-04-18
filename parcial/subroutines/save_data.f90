subroutine save_data(nc, tavg, pmax, max_lluvia, indices, pmin, min_temp)
    implicit none
    
    integer, intent(in) :: nc
    
    real(8), intent(in) :: tavg, max_lluvia, min_temp
    integer, intent(in) :: pmax, pmin
    integer, intent(in) :: indices(nc)
    
    integer :: i

    ! Usamos status="replace" por si el archivo ya existe y lo volvés a correr
    open(unit=38, file="data.out", status="replace")
    
    write(38, *) "--- RESULTADOS CLIMATOLOGICOS ---"
    
    write(38, '(A, F8.2)') "El promedio de la temperatura media es: ", tavg
    
    write(38, '(A, I2, A, F8.2)') "El mes que mas llovio fue el ", pmax, " y llovio: ", max_lluvia
    
    write(38, *) "Los meses con humedad menor a 50% fueron:"

    if (maxval(indices) > 0) then
            do i = 1, nc
                if (indices(i) > 0) then
                    write(38, '(A, I2)') "  - Mes: ", indices(i)
                end if
            end do
	else
            write(38, *) "  (Ningun mes registro humedad menor al 50%)"
	end if
    
    write(38, '(A, I2, A, F8.2)') "El mes con la temperatura minima mas baja fue el ", pmin, " y dicha temperatura fue: ", min_temp
    
    close(38)

end subroutine save_data
