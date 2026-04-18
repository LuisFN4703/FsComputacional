program mruv 
	use mod_mruv_subrutinas
	implicit none
	real :: h(3), t(3), g(3), avg
	integer :: i
	
	call leer_datos(h, t)
	call calc_g(h, t, g)
 	call prom_g(3, g, avg)
 	print*, 'el valor promedio de g es:', avg
end program mruv

 
