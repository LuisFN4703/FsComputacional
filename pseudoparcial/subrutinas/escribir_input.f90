subroutine escribir_input(n, v1, v2, v3)
implicit none
	integer, intent(in) :: n
	real(8), intent(in) :: v1(n), v2(n), v3(n) 
	integer :: i

	open(unit=20, file="datos.out", status="unknown")
	write(20, *) "Los valores medios temporales de la posicion de cada atomo son :"
	do i = 1, 3
		write(20, *) "Atomo",i,"=", v1(i)
	enddo
	write(20, *) 
	write(20, *) "Las desviaciones máximas para cada atomo son:"
	do i = 1, 3
		write(20, *) "Atomo",i,"=", v2(i)
	enddo	
	write(20, *)
	write(20, *) "Las desviaciones mínimas para cada atomo son:"
	do i = 1, 3
		write(20, *) "Atomo",i,"=", v3(i)
	enddo	
	close(20)

end subroutine escribir_input
