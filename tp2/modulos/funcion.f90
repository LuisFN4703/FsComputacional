module funcion
use mis_subrutinas, only: dp
implicit none
contains

function f(t, y, m)
    integer, intent(in) :: m
    real(dp), intent(in) :: t       
    real(dp), intent(in) :: y(m) 
    real(dp) :: f(m)

	real(dp) :: r0, r1, L, C, V0
	r0 = 200.0_dp
	r1 = 250.0_dp
	L = 15.0_dp
	C = 4.2E-6_dp
	V0 = 1000.0_dp

    f(1) = -(r0/L) * y(1) - (r1/L) * y(1)**3 - y(2)/(L*C) + V0/L

    f(2) = y(1)
end function f


end module
