PROGRAM parcial
implicit none 
integer :: nrow
real(8), allocatable :: o(:), h1(:), h2(:)
real(8) :: avg(3), max(3), min(3)
integer :: i

	call leer_input(nrow)
	
	allocate(o(nrow), h1(nrow), h2(nrow))
	
	call posiciones(nrow, o, h1, h2)
	call promedios(nrow, o, h1, h2, avg)
	call desviacion_max(nrow, o, h1, h2, max)
	call desviacion_min(nrow, o, h1, h2, min)
	call escribir_input(nrow, o, h1, h2)

END PROGRAM parcial
