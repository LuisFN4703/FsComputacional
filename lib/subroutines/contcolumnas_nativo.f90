subroutine contarcolumnas_nativo(archivo, ncol)
    implicit none
    character(len=*), intent(in) :: archivo
    integer, intent(out) :: ncol
    
    integer :: iunit, ios, i
    character(len=10000) :: linea  ! Buffer amplio para números de alta precisión
    logical :: en_dato

    ncol = 0
    iunit = 10 

    open(unit=iunit, file=trim(archivo), status='old', action='read', iostat=ios)
    if (ios /= 0) then
        ncol = -1  ! Error al abrir
        return
    end if

    ! Leer la primera línea completa
    read(iunit, '(A)', iostat=ios) linea
    close(iunit)

    if (ios == 0) then
        en_dato = .false.
        ! Recorremos la línea carácter por carácter
        do i = 1, len_trim(linea)
            ! Si el carácter NO es un espacio o una coma (delimitadores comunes)
            if (linea(i:i) /= ' ' .and. linea(i:i) /= ',' .and. linea(i:i) /= char(9)) then
                if (.not. en_dato) then
                    ncol = ncol + 1
                    en_dato = .true.  ! Entramos en un número
                end if
            else
                en_dato = .false.     ! Salimos a un espacio
            end if
        end do
    end if
end subroutine contarcolumnas_nativo
