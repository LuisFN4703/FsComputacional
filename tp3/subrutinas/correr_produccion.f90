subroutine correr_produccion(pasos_prod)
    use mod_ising
    implicit none
    integer, intent(in) :: pasos_prod
    integer :: paso, aceptados
    real(8) :: suma_E, suma_E2, suma_M, suma_M2, suma_acep
    real(8) :: N_total

    ! inicio en 0
    suma_E    = 0.0
    suma_E2   = 0.0
    suma_M    = 0.0
    suma_M2   = 0.0
    suma_acep = 0.0
    N_total   = real(N * N)

    do paso = 1, pasos_prod
        call paso_metropolis(aceptados)

        ! acumulacion de los valores en cada instante
        suma_E    = suma_E + E_tot
        suma_E2   = suma_E2 + (E_tot * E_tot)
        suma_M    = suma_M + M_tot
        suma_M2   = suma_M2 + (M_tot * M_tot)
        suma_acep = suma_acep + (real(aceptados) / N_total)
    end do

    ! valores medios dividiendo por el total de pasos
    E_med  = suma_E / real(pasos_prod)
    E2_med = suma_E2 / real(pasos_prod)
    M_med  = suma_M / real(pasos_prod)
    M2_med = suma_M2 / real(pasos_prod)
    tasa_aceptacion_media = (suma_acep / real(pasos_prod)) * 100.0

    ! Calculamos Calor Específico y Susceptibilidad usando las varianzas
    cv = ((beta**2) / N_total) * (E2_med - (E_med**2))
    susceptibilidad = beta * (M2_med - (M_med**2))

end subroutine correr_produccion
