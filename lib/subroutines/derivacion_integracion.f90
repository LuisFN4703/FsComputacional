module derivacion_integracion
    implicit none
    integer, parameter :: dp = kind(1.0d0)

contains
!--------------------------------------------------------------------------------
!----------------------- SUBRUTINAS DERIVACION ----------------------------------
!--------------------------------------------------------------------------------

!----------------------- DERIVADA HACIA ADELANTE --------------------------------
    subroutine derivada_adelante(n, x, y, dy)
        integer, intent(in) :: n           ! Numero total de puntos
        real(dp), intent(in) :: x(n)       ! Entrada x
        real(dp), intent(in) :: y(n)       ! Entrada f(x)
        real(dp), intent(out) :: dy(n)     ! derivada f'(x)
        
        integer :: i
        real(dp) :: h
        
        dy = 0.0_dp
        
        do i = 1, n - 1
            h = x(i+1) - x(i)
            
            ! fórmula: f'(x) = [f(x+h) - f(x)] / h
            dy(i) = (y(i+1) - y(i)) / h
        end do
        dy(n) = dy(n-1) 
        
    end subroutine derivada_adelante
    
!----------------------------- DERIVADA HACIA ATRAS -------------------------------------
	subroutine derivada_atras(n, x, y, dy)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n)
        real(dp), intent(in) :: y(n)
        real(dp), intent(out) :: dy(n)
        
        integer :: i
        real(dp) :: h
        
        dy = 0.0_dp
        
        do i = 2, n
            h = x(i) - x(i-1)
            dy(i) = (y(i) - y(i-1)) / h
        end do
        
        dy(1) = dy(2)
    end subroutine derivada_atras

!----------------------- DERIVADA CENTRAL --------------------------------
    subroutine derivada_central(n, x, y, dy)
        integer, intent(in) :: n
        real(dp), intent(in) :: x(n)
        real(dp), intent(in) :: y(n)
        real(dp), intent(out) :: dy(n)
        
        integer :: i
        real(dp) :: h
        
        dy = 0.0_dp
        
        do i = 2, n - 1
            h = (x(i+1) - x(i-1)) / 2.0_dp
            dy(i) = (y(i+1) - y(i-1)) / (2.0_dp * h)
        end do
        
        dy(1) = (y(2) - y(1)) / (x(2) - x(1))
        dy(n) = (y(n) - y(n-1)) / (x(n) - x(n-1))
    end subroutine derivada_central



!-----------------------------------------------------------------------------------------
!----------------------- SUBRUTINAS INTEGRACION ------------------------------------------
!-----------------------------------------------------------------------------------------

!----------------------- INTEGRAL TRAPECIO --------------------------------
	subroutine trapecio(n, x, y, integral)
        integer, intent(in) :: n
        real*8, intent(in) :: x(n), y(n)
        real*8, intent(out) :: integral
        integer :: i
        real*8 :: h
        
        integral = 0.0d0
        do i = 1, n - 1
            h = x(i+1) - x(i)
            integral = integral + (h / 2.0d0) * (y(i) + y(i+1))
        end do
    end subroutine trapecio
    
!----------------------- INTEGRAL SIMPSON 1/3 --------------------------------
    subroutine simpson13(n, x, y, integral)
        integer, intent(in) :: n
        real*8, intent(in) :: x(n), y(n)
        real*8, intent(out) :: integral
        integer :: i, m
        real*8 :: h
        
        m = n - 1 
        if (mod(m, 2) /= 0) then
            print *, "Error: Simpson 1/3 requiere un numero par de intervalos."
            return
        end if
        
        h = (x(n) - x(1)) / dble(m)
        integral = y(1) + y(n)
        
        do i = 2, n - 1
            if (mod(i-1, 2) /= 0) then
                integral = integral + 4.0d0 * y(i)
            else
                integral = integral + 2.0d0 * y(i)
            end if
        end do
        
        integral = (h / 3.0d0) * integral
    end subroutine simpson13

!----------------------- INTEGRAL SIMPSON 3/8 --------------------------------
    subroutine simpson38(n, x, y, integral)
        integer, intent(in) :: n
        real*8, intent(in) :: x(n), y(n)
        real*8, intent(out) :: integral
        integer :: i, m
        real*8 :: h
        
        m = n - 1 
        if (mod(m, 3) /= 0) then
            print *, "Error: Simpson 3/8 requiere que el numero de intervalos sea multiplo de 3."
            return
        end if
        
        h = (x(n) - x(1)) / dble(m)
        integral = y(1) + y(n)
        
        do i = 2, n - 1
            if (mod(i-1, 3) == 0) then
                integral = integral + 2.0d0 * y(i)
            else
                integral = integral + 3.0d0 * y(i)
            end if
        end do
        
        integral = (3.0d0 * h / 8.0d0) * integral
    end subroutine simpson38

end module derivacion_integracion
