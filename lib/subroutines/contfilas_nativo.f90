subroutine contarfilas_nativo(file_name, nrow)
    character(len=*), intent(in) :: file_name
    integer, intent(out) :: nrow
    integer :: ier
    
    nrow = 0
    open(unit=10, file=trim(file_name), status='old', action='read')
    do
        read(10, *, iostat=ier)
        if (ier /= 0) exit
        nrow = nrow + 1
    end do
    close(10)
    return
end subroutine
