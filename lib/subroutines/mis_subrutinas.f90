module mis_subrutinas
implicit none
integer, parameter :: dp = selected_real_kind(15, 307)

contains

!subrutina para sacar la primera fila (encabezado) y la primera columna
subroutine matriz_util(nombre_archivo, nrow, ncol, matriz)

	!variable local para definir la precicion de vectores y m
	
	!-------------VARIABLES DE ENTRADA -------------------------------
	character(len=*), intent(in) :: nombre_archivo
	integer, intent(in)          :: nrow, ncol

	!--------------VARIABLES DE SALIDA-------------------------------
	real(dp), intent(out)        :: matriz(nrow, ncol)

  	!--------------VARIABLES LOCALES ---------------------------------
	integer :: i
	character(len=256) :: columna_ignorada

	!--------------CODIGO--------------------------------------------
	open(unit=27, file=nombre_archivo, status='old')

	!lee la primera fila pero la ignora
	read(27, *) 

	!lee fila por fila
	do i = 1, nrow
		read(27, *) columna_ignorada, matriz(i, :) !almacena la primera columan en una variable muda
	end do

	close(27)
	
end subroutine matriz_util
!--------------------------------------------------------------------------------------------


!subrutina para crear vectores a partir de las columnas de una matriz nrow x 3
subroutine crear_vectores(nombre, nrow, v1, v2)
    character(len=*), intent(in) :: nombre
    integer, intent(in) :: nrow
    real(8), intent(out) :: v1(0:nrow), v2(0:nrow)
    integer :: i

    open(23, file=trim(nombre), status='old')

	read(23, *)
    do i=0, nrow - 1
        read(23, *) v1(i), v2(i)
    enddo

    close(23)

end subroutine crear_vectores
!-------------------------------------------------------------------------

!subrutina para calacular el promedio de un vector de dimension nrow
subroutine prom_vector(v, dim, avg)
	real(8), intent(in) :: v(dim)
	integer, intent(in) :: dim
	real(8), intent(out) :: avg
	integer :: i
	real(8) :: suma

	suma = 0.0
	do i = 1, dim
		suma = suma + v(i)
	enddo
	avg = suma/dim

end subroutine prom_vector
!--------------------------------------------------------------------------

!subrutina para calcular el valor absoluto de un vector (hace todas sus componentes positivas)
subroutine abs_vector(v, dim, absv)
	integer, intent(in) :: dim
	real(8), intent(in) :: v(dim)
	real(8), intent(out) :: absv(dim)
	integer :: i

	do i = 1, dim
		if (v(i) .LE. 0 ) then
			absv(i) = -v(i)
			else
			absv(i) = v(i)
		endif
	enddo

end subroutine abs_vector
!---------------------------------------------------------------------------

!subrutina para calcular el valor que mas se desvia de un punto de referencia
subroutine desv_max(v, dim, ref, max, pos_max)
	integer, intent(in) :: dim
	real(8), intent(in) :: v(dim), ref
	real(8), intent(out) :: max
	integer, intent(out) :: pos_max
	integer i

	max = 0.0
	pos_max = 1
	do i = 1, dim 
		if (abs(v(i)-ref) .GE. max) then
			max = abs(v(i)-ref)
			pos_max=i
		endif
	enddo

end subroutine desv_max
!-----------------------------------------------------------------------------

!subrutina que calcula el valor que mas cerca esta de un valor de referencia en un vector
subroutine desv_min(v, dim, ref, min, pos_min)
	integer, intent(in) :: dim
	real(8), intent(in) :: v(dim), ref
	real(8), intent(out) :: min
	integer, intent(out) :: pos_min
	integer i
	
	min = abs(v(1)-ref)
	pos_min = 1
	do i = 1, dim 
		if (abs(v(i)-ref) .LE. min) then
			min = abs(v(i)-ref)
			pos_min=i
		endif
	enddo

end subroutine desv_min


subroutine extraer_fila(nrow, ncol, matriz, indice_fila, vector)
    integer, intent(in) :: nrow, ncol, indice_fila
    real(dp), intent(in)  :: matriz(nrow, ncol)
    real(dp), intent(out) :: vector(ncol)
    
    ! Control de seguridad: verificamos que la fila exista
    if (indice_fila >= 1 .and. indice_fila <= nrow) then
        
        ! Array slicing: Agarra la fila "indice_fila" entera (todas sus columnas)
        vector = matriz(indice_fila, :)
        
    else
        print *, "Error: El índice de la fila (", indice_fila, ") está fuera de rango."
        vector = 0.0_dp  ! Llenamos de ceros por seguridad si hay error
    end if
    
end subroutine extraer_fila

subroutine filtrar_menores(v, dim, N, v_filtrado, indices, contador)
    implicit none
    
    ! Entradas
    integer, intent(in)  :: dim
    real(8), intent(in)  :: v(dim), N
    
    ! Salidas dimensionadas al máximo posible
    real(8), intent(out) :: v_filtrado(dim)
    integer, intent(out) :: indices(dim)
    integer, intent(out) :: contador
    
    integer :: i

    contador = 0

    do i = 1, dim
        ! ACÁ ESTÁ EL CAMBIO: estrictamente menor (<)
        if (v(i) < N) then
            contador = contador + 1
            v_filtrado(contador) = v(i)
            indices(contador) = i
        endif
    enddo

end subroutine filtrar_menores

end module
