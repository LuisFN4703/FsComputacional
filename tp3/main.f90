program main
    use mod_ising
    use ziggurat, only: zigset
    implicit none
    
    integer :: paso, aceptados
    integer :: t_termalizacion, t_produccion
    integer :: iT  ! Índice entero para el bucle de temperaturas

    ! 1. inicializacion para el modulo ziggurat
    call zigset(12345)

    ! archivo para guardar los resultados finales
    open(unit=20, file='outputs/resultados_vs_T.dat', status='replace')
    write(20, *) "# T    <E>    <M>    Cv    Susceptibilidad    Aceptacion(%)"

    t_termalizacion = 20000 !con estas dos lineas se separan en los bloque de termalizacion
    t_produccion = 100000 ! y produccion respectivamente

	print *, "Inicializando la red aleatoria para la primera temperatura..."
    ! se genera la cond incial una sola vez, luego se actualiza en el do
    call cond_ini()    

    print *, "Iniciando barrido de temperaturas..."

    ! 2. barrido de temperaturas desde T=1 hasta T=4 con paso dT=0.1
	do iT = 10, 40
                
		! Actualizamos los parámetros físicos en el módulo
		T = real(iT) / 10.0
		beta = 1.0 / T
        
        print *, "Simulando para T = ", T

        call calc_obs_ini()

        ! 3. etapa de termalizacion
        do paso = 1, t_termalizacion
            call paso_metropolis(aceptados)
        end do

        ! 4. etapa de produccion
        call correr_produccion(t_produccion)

        ! 5. guardado de datos
        write(20, *) T, E_med, M_med, cv, susceptibilidad, tasa_aceptacion_media

    end do

    close(20)
    print *, "Simulacion completa. Datos guardados en outputs/resultados_vs_T.dat"

end program main
