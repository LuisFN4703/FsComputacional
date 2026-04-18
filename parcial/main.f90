program main
implicit none !comentario
	integer nrow, ncol
	integer, parameter :: dp = selected_real_kind(15, 307)
	real(dp), allocatable :: matriz(:, :)
	real(dp), allocatable :: t(:), tmax(:), tmin(:), hum(:), prec(:)
	real(8) :: tavg, max_lluvia, min_temp
	integer :: pmax, pmin
	integer, allocatable :: indices(:)
	integer :: i

	    
	call leer_input(nrow, ncol)
	allocate(matriz(nrow, ncol))
	allocate(t(ncol), tmax(ncol), tmin(ncol), hum(ncol), prec(ncol))
	allocate(indices(ncol))
	
	
	call datos_utiles(nrow, ncol, matriz)
	call crear_vect(nrow, ncol, matriz, t, tmax, tmin, hum, prec)
	call tareas(ncol, t, tmin, tmax, hum, prec, tavg, max_lluvia, pmax, indices, min_temp, pmin)
	call save_data(ncol, tavg, pmax, max_lluvia, indices, pmin, min_temp)

end program main
