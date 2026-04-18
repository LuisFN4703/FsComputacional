subroutine tareas(ncol, v1, v2, v3, v4, v5, tavg, max_lluvia, pmax, indices, min_temp, pmin, cruces, n_cruces)
use subrutinas_parcial
implicit none

    integer, intent(in) :: ncol
    !integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp), intent(in) :: v1(ncol), v2(ncol), v3(ncol), v4(ncol), v5(ncol)
    real(dp), intent(out) :: tavg, max_lluvia, min_temp
    integer, intent(out)  :: pmax, pmin
    integer, intent(out)  :: indices(ncol)
    real(dp), intent(out) :: cruces(ncol) 
	integer, intent(out) :: n_cruces
    real(dp) :: ref, N
    real(dp) :: v_filtrado(ncol) 
    integer  :: temp_arr(1), contador 


    indices = 0

    ! 1. Promedio de la temperatura media en el año 
    call prom_vector(v1, ncol, tavg)

    ! 2. Mes más lluvioso y mm caídos 
	max_lluvia = maxval(v5)
	temp_arr = maxloc(v5)
    pmax = temp_arr(1)

    ! 3. Meses con humedad menor a 50% 
    N = 50.0_dp
    ! v_filtrado y contador operan aquí, pero no salen de la subrutina
    call filtrar_menores(v4, ncol, N, v_filtrado, indices, contador)

    ! 4. Mes con temperatura mínima más baja y dicha temperatura (v3)
    min_temp = minval(v3)
    temp_arr = minloc(v3)
    pmin = temp_arr(1)

    ! 5. Estimacion de cuando las temperaturas son iguales a 20 °C
    ref = 20.0_dp
	call buscar_cruces(v1, ncol, ref, cruces, n_cruces)
    

end subroutine tareas
