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

Subroutine Interpol_Newton(n, xint, yint, X, Y, a, dd)  
  integer, intent(in) :: n
  real*8,intent(in)::  X(n),Y(n)
  real*8, intent(out) :: a(n)
  real*8, intent(out) ::  DD(n,n)
  real*8 :: xint, yint, xn
 
!70 format(a2,i3,'   x(i)=',f8.3,'   y(i)=',f8.3,'   d(i,1)=',f8.3) 
  
  ! Cheque si el punto de interpolación es correcto
if ((xint<minval(X)).or. (xint > maxval(x)) ) Then
   print*, 'El valor de x se encuentra fuera del intervalo de los puntos de datos'
   return
else

  ! Genera las diferencias divididas
  do i=1,n
   dd(i,1)= Y(i)
  enddo

  do j=2, n
    do i=1, n-j+1
      DD(i,j) = difdiv(x(j-1+i),x(i),dd(i+1,j-1),dd(i,j-1))
    enddo
  enddo
endif

print*,'--------------------DD(i,j)----------------'
print*,

do i=1,n 
 write(*,'(*(f6.2,3x))') (dd(i,j) ,j=1,n)
enddo


print*,
print*,
     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
!Construyo el polinomio de interpolación y calculo yint
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

yint=dd(1,1)
 xn=1
 do j=2,n
  xn=xn*(xint-x(j-1))
  yint=yint+dd(1,j)*xn
 enddo

return
end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
!!!!!!!!funcion diferencia dividida !!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real*8 function  difdiv(xf, xi, yf,yi)
implicit none
real*8 yf, yi, xf, xi
difdiv=(yf-yi)/(xf-xi)
return
end 

subroutine splines_cubicos(n, xint, yint, x, a)
implicit none
integer, intent(in) :: n
real*8, intent(in) :: x(0:n), a(0:n)
real*8, intent(in) :: xint
real*8, intent(out) :: yint

real*8 :: b(0,n), c(0,n), d(0,n), h(0,n)
integer :: nd, s, inf

real*8 :: l(0,n), dp(0,n), u(0,n) !vectores para cargar la matriz tridiagonal
real
integer :: i, j

!Paso 1
	do i=0, n-1
		h(i) = x(i+1)-x(i)
	enddo

!Paso 2


end module
