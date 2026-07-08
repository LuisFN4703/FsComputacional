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
			import :: dp
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
			import :: dp
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
			import :: dp
			real(dp), intent(in) :: x, y
		end function f
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
			import :: dp
			real(dp), intent(in) :: x, y
		end function f
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
			import :: dp
			real(dp), intent(in) :: x, y
		end function f
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

!----------------- metodo rk4 para sistemas -------------------
!En gral m =2 porque es para resolver edos de 2do orden. m vendria a ser el orden de la EDO
subroutine rk4_sist(x0, y0, n, h, m, x, y, f)
    implicit none
    integer, intent(in) :: n, m
    real(dp), intent(in) :: x0, h
    
    real(dp), intent(in) :: y0(m)

    real(dp), intent(inout) :: x(0:n)
    real(dp), intent(inout) :: y(0:n, m) 
    
    integer :: i

    real(dp) :: k1(m), k2(m), k3(m), k4(m)
    
    interface
        function f(x, y, m)
            import :: dp
            implicit none
            integer, intent(in) :: m
            real(dp), intent(in) :: x
            real(dp), intent(in) :: y(m)
            real(dp) :: f(m)
        end function f
    end interface

    x(0) = x0
    y(0, :) = y0(:)  
    do i = 1, n
        x(i) = x(i-1) + h
        
        k1 = f(x(i-1), y(i-1, :), m)
        k2 = f(x(i-1) + 0.5_dp * h, y(i-1, :) + 0.5_dp * k1 * h, m)
        k3 = f(x(i-1) + 0.5_dp * h, y(i-1, :) + 0.5_dp * k2 * h, m)
        k4 = f(x(i-1) + h, y(i-1, :) + k3 * h, m)

        y(i, :) = y(i-1, :) + (k1 + 2.0_dp * k2 + 2.0_dp * k3 + k4) * (h / 6.0_dp)
    end do

end subroutine rk4_sist
!--------------------------------------------------------------

!----------------- metodo de disparo (2do orden) -------------------
subroutine disparo(a, b, alpha, beta, n, tol, max_iter, x, y, f)
    use mis_subrutinas, only: dp
    implicit none
    
    real(dp), intent(in) :: a, b, alpha, beta, tol
    integer, intent(in) :: n, max_iter
    real(dp), intent(inout) :: x(0:n), y(0:n)
    
    integer :: i, iter
    real(dp) :: h, w1, w2, w3, y_b1, y_b2
    real(dp) :: y_sist(0:n, 2)
    
    interface
        function f(x_val, y_vec, m)
            import :: dp
            implicit none
            integer, intent(in) :: m
            real(dp), intent(in) :: x_val
            real(dp), intent(in) :: y_vec(m)
            real(dp) :: f(m)
        end function f
    end interface

    h = (b - a) / real(n, dp)
    
    ! Primera estimación de la pendiente W1 [cite: 52, 53]
    w1 = (beta - alpha) / (b - a) 
    call rk4_sist(a, [alpha, w1], n, h, 2, x, y_sist, f)
    y_b1 = y_sist(n, 1) ! Solucion en x=b [cite: 54]
    
    if (abs(y_b1 - beta) <= tol) then
        y(0:n) = y_sist(0:n, 1)
        return
    end if
    
    ! Segunda estimación W2 [cite: 59, 60]
    w2 = w1 + 0.1_dp 
    call rk4_sist(a, [alpha, w2], n, h, 2, x, y_sist, f)
    y_b2 = y_sist(n, 1) ! Solucion en x=b [cite: 61]
    
    ! Proceso iterativo usando interpolación (Secante) [cite: 75, 77, 79]
    do iter = 1, max_iter
        if (abs(y_b2 - beta) <= tol) then
            y(0:n) = y_sist(0:n, 1)
            return
        end if
        
        ! Calcular nueva pendiente W3 [cite: 76, 77]
        w3 = w2 - (y_b2 - beta) * (w2 - w1) / (y_b2 - y_b1)
        
        ! Actualizar variables para la proxima iteracion
        w1 = w2
        y_b1 = y_b2
        w2 = w3
        
        ! Resolver el sistema nuevamente [cite: 78]
        call rk4_sist(a, [alpha, w2], n, h, 2, x, y_sist, f)
        y_b2 = y_sist(n, 1)
    end do
    
    print *, "El metodo de disparo no convergio en iteraciones maximas"
    y(0:n) = y_sist(0:n, 1)
    
end subroutine disparo
!-------------------------------------------------------------------

!----------------- diferencias finitas (2do orden lineal) -------------------
subroutine diferencias_finitas(a, b, alpha, beta, n_int, x, y, p, q, r)
    use mis_subrutinas, only: dp
    implicit none
    
    real(dp), intent(in) :: a, b, alpha, beta
    integer, intent(in) :: n_int ! N subintervalos interiores 
    real(dp), intent(inout) :: x(0:n_int+1), y(0:n_int+1)
    
    integer :: i
    real(dp) :: h, x_val
    real(dp) :: diag(n_int), subdiag(n_int), supdiag(n_int), rhs(n_int)
    
    ! Variables para el algoritmo de Thomas (solucionador tridiagonal)
    real(dp) :: c_star(n_int), d_star(n_int)
    
    interface
        real(dp) function p(x)
            import :: dp
            real(dp), intent(in) :: x
        end function p
        real(dp) function q(x)
            import :: dp
            real(dp), intent(in) :: x
        end function q
        real(dp) function r(x)
            import :: dp
            real(dp), intent(in) :: x
        end function r
    end interface

    ! Tamaño del paso y condiciones de frontera 
    h = (b - a) / real(n_int + 1, dp) 
    x(0) = a
    y(0) = alpha 
    x(n_int+1) = b
    y(n_int+1) = beta 
    
    do i = 1, n_int
        x(i) = a + i * h 
    end do
    
    ! Construir la matriz tridiagonal A 
    do i = 1, n_int
        x_val = x(i)
        
        diag(i) = 2.0_dp + (h**2) * q(x_val)
        subdiag(i) = -1.0_dp - (h / 2.0_dp) * p(x_val) 
        supdiag(i) = -1.0_dp + (h / 2.0_dp) * p(x_val) 
        rhs(i) = -(h**2) * r(x_val) 
    end do
    
    ! Ajustar el lado derecho b por las condiciones de frontera 
    rhs(1) = rhs(1) - subdiag(1) * alpha
    rhs(n_int) = rhs(n_int) - supdiag(n_int) * beta
    
    ! Resolver el sistema A * y = rhs usando algoritmo de Thomas 
    c_star(1) = supdiag(1) / diag(1)
    d_star(1) = rhs(1) / diag(1)
    
    do i = 2, n_int
        if (i < n_int) then
            c_star(i) = supdiag(i) / (diag(i) - subdiag(i) * c_star(i-1))
        end if
        d_star(i) = (rhs(i) - subdiag(i) * d_star(i-1)) / &
                    (diag(i) - subdiag(i) * c_star(i-1))
    end do
    
    y(n_int) = d_star(n_int)
    do i = n_int - 1, 1, -1
        y(i) = d_star(i) - c_star(i) * y(i+1)
    end do
    
end subroutine diferencias_finitas
!----------------------------------------------------------------------------

end module
