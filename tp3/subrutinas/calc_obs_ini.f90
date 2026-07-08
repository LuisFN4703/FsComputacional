subroutine calc_obs_ini()
    use mod_ising
    implicit none
    integer :: i, k

    M_tot = 0.0
    E_tot = 0.0
    beta = 1.0 / T  ! uso k_B = 1

    do k = 1, N
        do i = 1, N
            M_tot = M_tot + real(spin(i, k))
            
            ! cada espin interactua con el vecino de la derecha (i+1) y el de arriba (k+1)
            E_tot = E_tot - k * real(spin(i, k)) * real(spin(i+1, k) + spin(i, k+1))
        end do
    end do

end subroutine calc_obs_ini
