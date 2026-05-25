module metodos_numericos
use mis_subrutinas, only: dp
implicit none

contains
!----------------- metodo de euler --------------------------
subroutine euler(x0, y0, n, h, x, y, f)
	real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: n
    real(dp), intent(inout) :: x(0:n), y(0:n)
    integer :: i
    
	interface
		real(dp) function f(x, y)
			real(dp), intent(in) :: x, y
		end function f
	end interface 

    x(0) = x0
    y(0) = y0

    do i = 1, n
        x(i) = x(i-1) + h
        y(i) = y(i-1) + f(x(i-1), y(i-1))*h
    end do
    
end subroutine euler
!------------------------------------------------------------

!------------- metodo de euler modificado -------------------
subroutine euler_mod(x0, y0, n, h, x, y, f)
	real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: n
    real(dp), intent(inout) :: x(0:n), y(0:n)
    integer :: i
	real(dp) :: y_euler
    
	interface
		real(dp) function f(x, y)
			real(dp), intent(in) :: x, y
		end function f
	end interface 

    x(0) = x0
    y(0) = y0

    do i = 1, n
        x(i) = x(i-1) + h
        y_euler = y(i-1) + f(x(i-1), y(i-1))*h
        y(i) = y(i-1) + 0.5_dp * (f(x(i-1), y(i-1)) + f(x(i), y_euler)) * h
    end do
    
end subroutine euler_mod 
!--------------------------------------------------------------


!------------- rk2: Metodo de Heun ----------------------------------
subroutine rk2_heun(x0, y0, n, h, x, y, f)
	real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: n
    real(dp), intent(inout) :: x(0:n), y(0:n)
    integer :: i
	real(dp) :: k1, k2
    
	interface
		real(dp) function f(x, y)
			real(dp), intent(in) :: x, y
		end function 
	end interface

	x(0) = x0
	y(0) = y0

	do i = 1, n
		x(i) = x(i-1) + h
	
		k1 = f(x(i-1), y(i-1))
		k2 = f(x(i-1) + h, y(i-1) + k1*h)

		y(i) = y(i-1) + (k1 + k2) * h * 0.5_dp
	end do

end subroutine rk2_heun
!----------------------------------------------------------------------

!---------------- rk2: Metodo del punto medio -------------------------
subroutine rk2_ptomedio(x0, y0, n, h, x, y, f)
	real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: n
    real(dp), intent(inout) :: x(0:n), y(0:n)
    integer :: i
	real(dp) :: k1, k2
    
	interface
		real(dp) function f(x, y)
			real(dp), intent(in) :: x, y
		end function 
	end interface

	x(0) = x0
	y(0) = y0

	do i = 1, n
		x(i) = x(i-1) + h
	
		k1 = f(x(i-1), y(i-1))
		k2 = f(x(i-1) + h *0.5_dp, y(i-1) + 0.5_dp * k1 * h)

		y(i) = y(i-1) + k2*h
	end do

end subroutine rk2_ptomedio
!---------------------------------------------------------------------------

!----------------- metodo rk4 ------------------------------------------------
subroutine rk4(x0, y0, n, h, x, y, f)
real(dp), intent(in) :: x0, y0, h
    integer, intent(in) :: n
    real(dp), intent(inout) :: x(0:n), y(0:n)
    integer :: i
	real(dp) :: k1, k2, k3, k4
    
	interface
		real(dp) function f(x, y)
			real(dp), intent(in) :: x, y
		end function 
	end interface

	x(0) = x0
	y(0) = y0

	do i = 1, n
		x(i) = x(i-1) + h
	
		k1 = f(x(i-1), y(i-1))
		k2 = f(x(i-1) + 0.5_dp * h, y(i-1) + 0.5_dp * k1 * h)
		k3 = f(x(i-1) + 0.5_dp * h, y(i-1) + 0.5_dp * k2 * h)
		k4 = f(x(i-1) + h, y(i-1) + k3 * h)

		y(i) = y(i-1) + (k1 + 2.0_dp * k2 + 2.0_dp * k3 + k4) * (h/6.0_dp)
	end do

end subroutine rk4 
!-------------------------------------------------------------------------------
end module
