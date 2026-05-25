subroutine savedata(n, m, t, y, filename)
use metodos_numericos
implicit none
	integer, intent(in) :: n, m
	real(dp), intent(in) :: t(0:n), y(0:n, m)
	character(len=*), intent(in) :: filename
	integer :: i

	open(unit=10, file=filename, status='replace')
	write(10, '(3A15)') "t", "I(t)", "Q(t)"
	do  i = 0, n
		write(10, '(3F15.5)') t(i), y(i, 1), y(i, 2)
	end do
	close(10) 
	
end subroutine savedata
