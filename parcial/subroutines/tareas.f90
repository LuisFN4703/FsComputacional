subroutine tareas(nc, v1, v2, v3, v4, v5, tavg, max_lluvia, pmax, indices, min_temp, pmin)
    use mis_subrutinas
    implicit none

    integer, intent(in) :: nc
    integer, parameter :: dp = selected_real_kind(15, 307)
    
    ! Vectores de entrada
    real(dp), intent(in) :: v1(nc), v2(nc), v3(nc), v4(nc), v5(nc)
    
    ! Salidas generales
    real(dp), intent(out) :: tavg, max_lluvia, min_temp
    integer, intent(out)  :: pmax, pmin
    
    ! Salidas del filtro de humedad (Solo los índices)
    integer, intent(out)  :: indices(nc)

    ! Variables locales
    real(dp) :: ref, N
    real(dp) :: v_filtrado(nc) ! Variable local
    integer  :: temp_arr(1), contador ! Contador ahora es local


    indices = 0

    ! 1. Promedio de la temperatura media en el año (v1)
    call prom_vector(v1, nc, tavg)

    ! 2. Mes más lluvioso y mm caídos (v5)
    ref = 0.0_dp
    call desv_max(v5, nc, ref, max_lluvia, pmax)

    ! 3. Meses con humedad menor a 50% (v4)
    N = 50.0_dp
    ! v_filtrado y contador operan aquí, pero no salen de la subrutina
    call filtrar_menores(v4, nc, N, v_filtrado, indices, contador)

    ! 4. Mes con temperatura mínima más baja y dicha temperatura (v2)
    min_temp = minval(v2)
    temp_arr = minloc(v2)
    pmin = temp_arr(1)

end subroutine tareas
