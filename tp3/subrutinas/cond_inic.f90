subroutine cond_ini()
    use mod_ising
    use ziggurat
    implicit none
    integer :: i, j
    real :: r

    ! llenado de la red con espines aleatorios usando la funcion uni() de ziggurat
    do j = 1, N
        do i = 1, N
            r = uni() !me genera un nro entre 0 y 1
            if (r < 0.5) then !si r esta entre 0 y 0.5  le asigno 1, sino -1
                spin(i, j) = 1
            else
                spin(i, j) = -1
            end if
        end do
    end do

    ! aplicacion de las c.p.c a los espines fantasmas
    spin(0, 1:N)   = spin(N, 1:N)   
    spin(N+1, 1:N) = spin(1, 1:N)   
    spin(1:N, 0)   = spin(1:N, N)   
    spin(1:N, N+1) = spin(1:N, 1)   

end subroutine cond_ini
