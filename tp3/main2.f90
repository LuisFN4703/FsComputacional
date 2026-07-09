program main2
    use mod_ising
    use ziggurat, only: zigset
    implicit none
    
    integer :: paso, aceptados
    integer :: t_termalizacion, t_produccion
    integer :: iT
    real(8) :: temps(8)
    real(8) :: N_total

    ! 1. Definimos las 8 temperaturas de interés (incluyendo Tc = 2.269)
    temps = [1.5d0, 2.0d0, 2.2d0, 2.269d0, 2.4d0, 2.6d0, 3.0d0, 3.5d0]
    
    N_total = real(N * N)
    call zigset(12345)
    
    ! Abrimos un único archivo para los datos de los histogramas
    open(unit=30, file='outputs/histogramas.dat', status='replace')
    write(30, *) "# T    E_por_espin    M_por_espin"

    t_termalizacion = 20000
    t_produccion = 100000

    print *, "Iniciando simulacion para histogramas (8 temperaturas)..."

    do iT = 1, 8
        T = temps(iT)
        beta = 1.0 / T
        print *, "Termalizando y produciendo para T = ", T

        ! Inicializamos la red de cero para cada T para evitar shock térmico
        call cond_ini()
        call calc_obs_ini()

        ! Etapa de termalización
        do paso = 1, t_termalizacion
            call paso_metropolis(aceptados)
        end do

        ! Etapa de producción (escribimos los observables instantáneos directamente)
        do paso = 1, t_produccion
            call paso_metropolis(aceptados)
            ! Se dividen las variables extensivas por N_total
            write(30, *) T, E_tot / N_total, M_tot / N_total
        end do

    end do

    close(30)
    print *, "Datos para histogramas listos en outputs/histogramas.dat"

end program main2
