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
        implicit none
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
                !f[x_j...x_i] = (f[x_j...x_{i+1}] - f[x_{j-1}...x_i]) / (x_f - x_i)
                dd(i, j) = (dd(i+1, j-1) - dd(i, j-1)) / (x(j-1+i) - x(i))
            end do
        end do
    end subroutine generar_tabla_newton

    subroutine evaluar_newton(n, x, dd, xint, yint)
        implicit none
        integer, intent(in) :: n
        real*8, intent(in) :: x(n), dd(n, n)
        real*8, intent(in) :: xint
        real*8, intent(out) :: yint
        
        integer :: j
        real*8 :: xn
        
        yint = dd(1, 1)
        xn = 1.0d0
        
        ! polinomio: a1 + a2(x-x1) + a3(x-x1)(x-x2) + ...
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
implicit none
    ! Argumentos
    integer, intent(in) :: n
    real*8, intent(in)  :: x(0:n), a(0:n)
    real*8, intent(in)  :: xint
    real*8, intent(out) :: yint

    ! Variables para LAPACK
    integer :: ipiv(0:n)
    integer :: info

    ! Arreglos internos del spline
    real*8 :: b(0:n), c(0:n), d(0:n), h(0:n-1)
    real*8 :: matriz(0:n, 0:n)
    integer :: i


!Paso 1: construccion de h
	do i=0, n-1
		h(i) = x(i+1)-x(i)
	enddo

!Paso 2: costruccion de b (vector)
	b(0) = 0
	do i=1, n-1
		b(i) = (3/h(i))*(a(i+1)-a(i)) - (3/h(i-1))*(a(i)-a(i-1))
	enddo
	b(n) = 0

!Paso 3: construccion de la matriz
    matriz = 0.0d0
    matriz(0, 0) = 1.0d0
    matriz(n, n) = 1.0d0

    do i = 1, n-1
        matriz(i, i-1) = h(i-1)                     ! Diagonal inferior
        matriz(i, i)   = 2.0d0 * (h(i-1) + h(i))    ! Diagonal principal
        matriz(i, i+1) = h(i)                       ! Diagonal superior
    end do

	!Paso 4: usar LAPACK para resolver A*x = b
	!Primero asigno los valores de b a c, para que LAPACK los guarde alli para mantener notacion 

	c = b
	call dgesv(n+1, 1, matriz, n+1, ipiv, c, n+1, info)
	! Control de seguridad
	if (info /= 0) then
		print *, "Error al resolver el sistema: DGESV retornó info = ", info
		return
	end if

	!Paso 5: obtener los coeficiente b y d
	do i=0, n-1
		b(i) = (a(i+1) - a(i))/h(i) - (h(i)*(c(i+1)+2*c(i)))/3
		d(i) = (c(i+1) - c(i))/(3*h(i))
	enddo
	
	!Paso 6: costruccion del polinomio
	do i = 0, n-1
		if (xint >= x(i) .and. xint <= x(i+1)) then
			yint = a(i) + b(i)*(xint - x(i)) + c(i)*((xint - x(i))**2) + d(i)*((xint - x(i))**3)
			exit
		endif
	enddo
end subroutine
	
end module
