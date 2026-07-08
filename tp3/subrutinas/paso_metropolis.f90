subroutine paso_metropolis(aceptados)
    use mod_ising
    use ziggurat, only: uni
    implicit none
    integer, intent(out) :: aceptados
    integer :: paso, i, k
    integer :: s_old, suma_vecinos
    real(8) :: delta_E, r

    aceptados = 0

    do paso = 1, N*N
        
        ! 1. elegir un espín al azar (i, k) entre 1 y N
        i = int(uni() * N) + 1
        k = int(uni() * N) + 1

        s_old = spin(i, k)

        ! 2. calcular la diferencia de energía usando solo los 4 primeros vecinos
        suma_vecinos = spin(i+1, k) + spin(i-1, k) + spin(i, k+1) + spin(i, k-1)
        delta_E = 2.0 * J * real(s_old) * real(suma_vecinos)

        ! 3. regla de aceptacin de Metropolis
        ! Si delta_E <= 0, exp(-beta*delta_E) >= 1, por lo que la condicion uni() < exp()
        ! cubre automaticamente los casos donde se minimiza la energía.
        if (delta_E <= 0.0 .or. uni() < exp(-beta * delta_E)) then
            
            ! aceptamos el cambio
            spin(i, k) = -s_old
            E_tot = E_tot + delta_E
            M_tot = M_tot - 2.0 * real(s_old)
            aceptados = aceptados + 1

            ! 4. app de C.P.C. 
            ! Si el espin volteado está en un borde, se actualiza su img fantasma
            if (i == 1) spin(N+1, k) = spin(1, k)
            if (i == N) spin(0, k)   = spin(N, k)
            if (k == 1) spin(i, N+1) = spin(i, 1)
            if (k == N) spin(i, 0)   = spin(i, N)
            
        end if
    end do

end subroutine paso_metropolis
