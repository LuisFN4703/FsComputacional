program main
    use mod_ising
    use ziggurat
    implicit none

    ! para utilizar el modulo ziggurat
    call zigset(12345) 

    ! generacion de una red de espines aleatorios (N=100)
    call cond_ini()

    print *, "Condición inicial generada con éxito."

end program main
