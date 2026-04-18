program main
use modbk
implicit none 
   
	call leer_input(nrow, ncol)
	
	allocate(matriz(nrow, ncol), t(ncol), tmax(ncol), tmin(ncol), hum(ncol), prec(ncol), indices(ncol), cruces(ncol))

	call datos_utiles(nrow, ncol, matriz)
	call crear_vect(nrow, ncol, matriz, t, tmax, tmin, hum, prec)!
	call tareas(ncol, t, tmax, tmin, hum, prec, tavg, max_lluvia, pmax, indices, min_temp, pmin, cruces, n_cruces)!
	call save_data(ncol, tavg, pmax, max_lluvia, indices, pmin, min_temp, cruces, n_cruces)

	deallocate(matriz, t, tmax, tmin, hum, prec, indices, cruces)

end program main
