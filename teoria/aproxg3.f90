program aproxg3
use interpolacion
use mis_subrutinas
implicit none
	character(len=20) :: nombre
	integer :: n, i
	real(8), allocatable :: x(:), a(:)
	real(8) :: xint, yint

	call contarfilas_nativo("inputs/cubicspline.dat", n)
	n = n - 1
	allocate(x(0:n), a(0:n))
	call crear_vectores("inputs/cubicspline.dat", n, x, a)
	print*, "Ingrese el valor donde quiere calcular la aproximacion"
	read*, xint
	call splines_cubicos(n, x, a, xint, yint)
	print*, "La aproximacion es:", yint

 
end program aproxg3
