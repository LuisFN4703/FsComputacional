program main
use metodos_numericos
implicit none
	integer :: m, n, i
	real(dp) :: h, t0, tn
	real(dp), allocatable :: t(:), y0(:), y(:,:)
	interface
        function f(t, y, m)
            use mis_subrutinas, only: dp
            implicit none
            integer, intent(in) :: m
            real(dp), intent(in) :: t
            real(dp), intent(in) :: y(m)
            real(dp) :: f(m)
        end function f
    end interface

	!datos del problema
	m = 2 !orden de la EDO
	h = 0.02_dp
	t0 = 0.0_dp
	tn = 0.3_dp

	!dimension de los vectores y cantidad de puntos
	n = nint(abs(tn - t0)/h)

	!Determinacion de la dimension de los vectores tiempo, condiciones 
	!iniciales y variables dependientes
	allocate(t(0:n), y0(m), y(0:n, m))

	!Guardado de condiciones iniciales
	y0(1) = 0.0_dp
	y0(2) = 0.0_dp 

	call rk4_sist(t0, y0, n, h, m, t, y, f)	
	call savedata(n, m, t, y, 'resultados.out')

	!Liberacion de memoria
	deallocate(t, y0, y)
	
	!graficacion
	call execute_command_line("gnuplot -c graficartp2.gp resultados.out")


end program main

function f(t, y, m)
    use mis_subrutinas, only: dp
    implicit none
    integer, intent(in) :: m
    real(dp), intent(in) :: t       
    real(dp), intent(in) :: y(m) 
    real(dp) :: f(m)

	real(dp) :: r0, r1, L, C, V0
	r0 = 200.0_dp
	r1 = 250.0_dp
	L = 15
	C = 4.2E-6_dp
	V0 = 1000

    f(1) = -(r0/L) * y(1) - (r1/L) * y(1)**3 - y(2)/(L*C) + V0/L

    f(2) = y(1)
end function f
