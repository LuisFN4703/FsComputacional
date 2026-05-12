module mod_tp1
	use mis_subrutinas, only: dp
    implicit none

contains

    ! Funcion analítica dada en el TP1
    ! y(x) = 10 * exp(-0.05*x^2) * sin(0.9*x)^2 + 0.05*x^2
    real(dp) function y_exacta(x)
        real(dp), intent(in) :: x
        y_exacta = 10.0_dp * exp(-0.05_dp * x**2) * (sin(0.9_dp * x)**2) + 0.05_dp * x**2
    end function y_exacta

    ! Derivada analitica exacta para comparacion
        real(dp) function dy_exacta(x)
            real(dp), intent(in) :: x
            dy_exacta = exp(-0.05_dp * x**2) * &
                        (9.0_dp * sin(1.8_dp * x) - x * (sin(0.9_dp * x)**2)) + &
                        0.1_dp * x
        end function dy_exacta

end module mod_tp1
