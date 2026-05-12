subroutine inciso_3(n_int, x_int, y_int, filename)
    use mod_tp1
    use derivacion_integracion
    implicit none
    integer, intent(in) :: n_int
    real(dp), intent(in) :: x_int(n_int), y_int(n_int)
    character(len=*), intent(in) :: filename
    real(dp) :: dy_fwd(n_int), dy_bwd(n_int), dy_central(n_int), dy_exac
    integer :: i
    
    ! 1. derivadas
    call derivada_adelante(n_int, x_int, y_int, dy_fwd)
    call derivada_atras(n_int, x_int, y_int, dy_bwd)  
    call derivada_central(n_int, x_int, y_int, dy_central)
    
    ! 2. Guardado de resultados
    open(unit=12, file=filename, status='replace')
    write(12, '(A15, 5A22)') "# x", "dy_Adelante(O1)", "dy_Atras(O1)", &
                             "dy_Central(O2)", "dy_Exacta", "Error_Rel_Central"
    
    do i = 1, n_int
        dy_exac = dy_exacta(x_int(i))
        
        ! se evita división por cero si dy_exac es muy pequeño
        if (abs(dy_exac) > 1.0e-10_dp) then
            write(12, '(6E22.12)') x_int(i), dy_fwd(i), dy_bwd(i), dy_central(i), &
                                  dy_exac, abs((dy_exac - dy_central(i))/dy_exac)
        else
            write(12, '(6E22.12)') x_int(i), dy_fwd(i), dy_bwd(i), dy_central(i), &
                                  dy_exac, 0.0_dp
        end if
    end do
    
    close(12) ! Corregido el cierre de la unidad 12
    print *, "TP1: Derivadas calculadas en ", trim(filename)
    
end subroutine inciso_3
