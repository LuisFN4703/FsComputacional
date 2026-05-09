module interpolacion
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!Función que crea polinomio de Lagrange !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real*8 function lagrange(xint,n, x, y)
implicit none
integer, intent(in) :: n
real*8, intent(in) :: xint, x(n), y(n)

real*8 :: suma, L  !variables usadas para los polinomios
integer :: i, j


suma = 0
DO i = 1, n
   L = 1
   DO j = 1, n
     IF (i /= j) THEN
        L = L * (xint- x(j))/(x(i) - x(j))
    ENDIF
   
   END DO

   suma = suma + L * y(i)
END DO

lagrange = suma
Return
END


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!Subrutina polinomios de Newton !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine generar_tabla_newton(n, x, y, dd)
	integer, intent(in) :: n
	real*8, intent(in) :: x(n), y(n)
	real*8, intent(out) :: dd(n, n)
	integer :: i, j
            
	dd = 0.0d0
	do i = 1, n
		dd(i, 1) = y(i)
	end do
    
		do j = 2, n
			do i = 1, n - j + 1
			dd(i, j) = (dd(i+1, j-1) - dd(i, j-1)) / (x(j-1+i) - x(i))
			end do
		end do
end subroutine generar_tabla_newton
    
subroutine evaluar_newton(n, x, dd, xint, yint)
	integer, intent(in) :: n
	real*8, intent(in) :: x(n), dd(n, n)
	real*8, intent(in) :: xint
	real*8, intent(out) :: yint
	integer :: j
	real*8 :: xn
            
	yint = dd(1, 1)
	xn = 1.0d0
	do j = 2, n
		xn = xn * (xint - x(j-1))
		yint = yint + dd(1, j) * xn
	end do
end subroutine evaluar_newton

subroutine splines_lineales(n, x, y, xint, yint)
    implicit none
    integer, intent(in) :: n
    real*8, intent(in) :: x(n), y(n)
    real*8, intent(in) :: xint
    real*8, intent(out) :: yint
    
    integer :: i
    
    ! Verificación de que el punto este dentro del rango de datos 
    if (xint < x(1) .or. xint > x(n)) then
        print *, 'Error: el valor de x para interpolar esta fuera de rango.'
        yint = 0.0d0
        return
    end if

    ! Busqueda del intervalo i tal que x(i) <= xint <= x(i+1) 
    do i = 1, n - 1
        if (xint <= x(i+1)) then
            ! Aplicación de la forma de Lagrange para el intervalo i 
            ! f_i(x) = [(x - x_{i+1}) / (x_i - x_{i+1})] * y_i + [(x - x_i) / (x_{i+1} - x_i)] * y_{i+1}
            yint = ((xint - x(i+1)) / (x(i) - x(i+1))) * y(i) + &
                   ((xint - x(i)) / (x(i+1) - x(i))) * y(i+1)
            exit ! Salir del bucle una vez encontrado el intervalo e interpolado 
        end if
    end do
end subroutine splines_lineales


subroutine splines_cubicos(n, x, a, xint, yint)
        integer, intent(in) :: n
        real*8, intent(in)  :: x(n), a(n)
        real*8, intent(in)  :: xint
        real*8, intent(out) :: yint

        integer :: ipiv(n), info, i
        real*8 :: b(n), c(n), d(n), h(n-1)
        real*8 :: matriz(n, n)

        ! Paso 1: construcción de h
        do i = 1, n-1
            h(i) = x(i+1) - x(i)
        end do

        ! Paso 2: construcción del vector de términos independientes
        b = 0.0d0
        do i = 2, n-1
            b(i) = (3.0d0/h(i))*(a(i+1)-a(i)) - (3.0d0/h(i-1))*(a(i)-a(i-1))
        end do

        ! Paso 3: construcción de la matriz tridiagonal
        matriz = 0.0d0
        matriz(1, 1) = 1.0d0
        matriz(n, n) = 1.0d0
        do i = 2, n-1
            matriz(i, i-1) = h(i-1)
            matriz(i, i)   = 2.0d0 * (h(i-1) + h(i))
            matriz(i, i+1) = h(i)
        end do

        ! Paso 4: Resolver sistema con LAPACK
        c = b
        call dgesv(n, 1, matriz, n, ipiv, c, n, info)

        if (info /= 0) then
            print *, "Error en LAPACK: ", info
            return
        end if

        ! Paso 5: Obtener coeficientes finales
        do i = 1, n-1
            b(i) = (a(i+1) - a(i))/h(i) - (h(i)*(c(i+1) + 2.0d0*c(i)))/3.0d0
            d(i) = (c(i+1) - c(i))/(3.0d0*h(i))
        end do
        
        ! Paso 6: Evaluación
        do i = 1, n-1
            if (xint >= x(i) .and. xint <= x(i+1)) then
                yint = a(i) + b(i)*(xint - x(i)) + c(i)*((xint - x(i))**2) + d(i)*((xint - x(i))**3)
                exit
            endif
        end do
    end subroutine splines_cubicos
	
end module
