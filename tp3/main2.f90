program main2
    use mod_ising
    use ziggurat, only: zigset
    implicit none
    
    integer :: paso, aceptados
    integer :: t_termalizacion, t_produccion
    integer :: iT
    real(8) :: temps(8)
    real(8) :: N_total
    character(len=20) :: arch(8) ! Nombres de los archivos

    temps = [1.5d0, 2.0d0, 2.2d0, 2.269d0, 2.4d0, 2.6d0, 3.0d0, 3.5d0]
    
    ! Nombres de los 8 archivos de salida
    arch(1) = "outputs/hist_1.dat"
    arch(2) = "outputs/hist_2.dat"
    arch(3) = "outputs/hist_3.dat"
    arch(4) = "outputs/hist_4.dat"
    arch(5) = "outputs/hist_5.dat"
    arch(6) = "outputs/hist_6.dat"
    arch(7) = "outputs/hist_7.dat"
    arch(8) = "outputs/hist_8.dat"
    
    N_total = real(N * N)
    call zigset(12345)
    
    t_termalizacion = 20000
    t_produccion = 100000

    print *, "Iniciando simulacion para histogramas independientes..."

    do iT = 1, 8
        T = temps(iT)
        beta = 1.0 / T
        print *, "Termalizando y produciendo para T = ", T

        ! Abrimos un archivo específico para esta T
        open(unit=30, file=trim(arch(iT)), status='replace')
        write(30, *) "# Columna 1: E_por_espin | Columna 2: M_por_espin"

        call cond_ini()
        call calc_obs_ini()

        ! Termalización (no guardamos nada)
        do paso = 1, t_termalizacion
            call paso_metropolis(aceptados)
        end do

        ! Producción
        do paso = 1, t_produccion
            call paso_metropolis(aceptados)
            
            ! MAGIA: Guardamos solo 1 de cada 100 pasos para evitar correlacion
            if (mod(paso, 100) == 0) then
                ! Ya no guardamos la T porque cada archivo es una T distinta
                write(30, *) E_tot / N_total, M_tot / N_total
            end if
        end do

        close(30)
    end do
    print *, "Datos listos. Peso total reducido. Listos para pushear a GitHub."
end program main2
