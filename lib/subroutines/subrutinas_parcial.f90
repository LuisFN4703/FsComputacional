module subrutinas_parcial
implicit none
integer, parameter :: dp = selected_real_kind(15, 307)

contains
subroutine contar_dimensiones(archivo, nrow, ncol)
	!Declaracion de varibles internas, externas y locales
    character(len=*), intent(in) :: archivo
    integer, intent(out)         :: nrow, ncol
    integer :: unit, istat, i, n_en_linea
    character(len=2048) :: line 
    logical :: en_palabra

    nrow = 0
    ncol = 0

    ! Abrir el archivo
    open(newunit=unit, file=archivo, status='old', action='read', iostat=istat)
    
    if (istat /= 0) then
        print *, "Error: No se pudo abrir el archivo ", archivo
        return
    end if

    do
        read(unit, '(A)', iostat=istat) line
        if (istat /= 0) exit ! Fitn del archivo o error

        nrow = nrow + 1
        n_en_linea = 0
        en_palabra = .false.

        ! Algoritmo para contar palabras separadas por espacios/tabs en la línea
        do i = 1, len_trim(line)
            if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then ! char(9) es el tabulador
                if (.not. en_palabra) then
                    n_en_linea = n_en_linea + 1
                    en_palabra = .true.
                end if
            else
                en_palabra = .false.
            end if
        end do

        ! Guardamos el máximo de columnas encontradas (o podrías comparar si varían)
        if (n_en_linea > ncol) ncol = n_en_linea
    end do

    close(unit)

end subroutine contar_dimensiones

!subrutina para sacar la primera fila (encabezado) y la primera columna
subroutine matriz_util(nombre_archivo, nrow, ncol, matriz)
	!Declaracion de varibles internas, externas y locales respectivamente
	character(len=*), intent(in) :: nombre_archivo
	integer, intent(in)          :: nrow, ncol
	real(dp), intent(out)        :: matriz(nrow, ncol)
	integer :: i
	character(len=256) :: columna_ignorada

	open(unit=27, file=nombre_archivo, status='old')

	read(27, *) !lee la primera fila pero la ignora

	do i = 1, nrow
		read(27, *) columna_ignorada, matriz(i, :) !guarda la primera columna en una variable muda y las demas en una matriz
	end do

	close(27)
	
end subroutine matriz_util
!--------------------------------------------------------------------------------------------

!subrutina para hacer de cada fila de una matriz en un vector
subroutine extraer_fila(nrow, ncol, matriz, fila, vector)
    integer, intent(in) :: nrow, ncol, fila
    real(dp), intent(in)  :: matriz(nrow, ncol)
    real(dp), intent(out) :: vector(ncol)
    
    ! Control de seguridad: verificamos que la fila exista
    if (fila >= 1 .and. fila <= nrow) then
        
        ! Array slicing: Agarra la fila "indice_fila" entera (todas sus columnas)
        vector = matriz(fila, :)
        
    else
        print *, "Error: El índice de la fila (", fila, ") está fuera de rango."
        vector = 0.0_dp  ! Llenamos de ceros por seguridad si hay error
    end if
    
end subroutine extraer_fila

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

!subrutina para encontrar los puntos (mediante interpolación lineal) 
subroutine buscar_cruces(v, dim, ref, posiciones_cruce, num_cruces)
    integer, intent(in)  :: dim
    real(dp), intent(in) :: v(dim)
    real(dp), intent(in) :: ref
    real(dp), intent(out) :: posiciones_cruce(dim) 
    integer, intent(out) :: num_cruces
    integer :: i
    real(dp) :: y1, y2
    
    num_cruces = 0
    posiciones_cruce = 0.0_dp
    
    do i = 1, dim - 1
        ! Restamos la referencia para evaluar cruces por el cero
        y1 = v(i) - ref
        y2 = v(i+1) - ref
        
        !si y1*y2 es negativo o cero hubo un cambio de signo 
        if (y1 * y2 <= 0.0_dp) then
            num_cruces = num_cruces + 1
            
            ! Control de seguridad: evitamos división por cero si la curva es plana
            if (v(i+1) == v(i)) then
                posiciones_cruce(num_cruces) = real(i, dp)
            else
                ! Ecuación de la recta (interpolación): x = x1 + (ref - y1)*(x2 - x1)/(y2 - y1)
                ! Como estamos usando índices, (x2 - x1) = (i+1) - i = 1
                posiciones_cruce(num_cruces) = real(i, dp) + (ref - v(i)) / (v(i+1) - v(i))
            end if
        end if
    end do
    
end subroutine buscar_cruces

end module
