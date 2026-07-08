module mod_ising
    implicit none
    
    !la dimension que pide el trabajo
    integer, parameter :: N = 100 !la dimension que pide el trabajo

    !defino la matriz de espines pero con dim 102x102 donde los espines 0 y 101 son "fantasmas"
    !en la subrutina aplico las C.P.C
    integer, dimension(0:N+1, 0:N+1) :: spin
    
	! parametros fisicos
    real(8) :: J = 1.0 ! ferromagnetismo
    real(8) :: T = 2.0  
    real(8) :: beta
    
    ! observables instantaneos
    real(8) :: E_tot, M_tot    
	! observables medios (promedios)
    real(8) :: E_med, E2_med
    real(8) :: M_med, M2_med
    real(8) :: cv, susceptibilidad
    real(8) :: tasa_aceptacion_media
    
end module mod_ising
