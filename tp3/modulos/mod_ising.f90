module mod_ising
    implicit none
    
    !la dimension que pide el trabajo
    integer, parameter :: N = 100 !la dimension que pide el trabajo

    !defino la matriz de espines pero con dim 102x102 donde los espines 0 y 101 son "fantasmas"
    !en la subrutina aplico las C.P.C
    integer, dimension(0:N+1, 0:N+1) :: spin
    
end module mod_ising
