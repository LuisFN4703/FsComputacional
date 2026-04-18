module modbk
!es para declarar variables
implicit none
	integer nrow, ncol
	integer, parameter :: dp = selected_real_kind(15, 307)
	real(dp), allocatable :: matriz(:, :)
	real(dp), allocatable :: t(:), tmax(:), tmin(:), hum(:), prec(:), cruces(:)
	real(8) :: tavg, max_lluvia, min_temp
	integer :: pmax, pmin, n_cruces
	integer, allocatable :: indices(:)
	integer :: i

end module
