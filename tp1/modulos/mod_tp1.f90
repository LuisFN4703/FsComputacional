module mod_tp1
    implicit none
    integer, parameter :: dp = kind(1.0d0)

contains

    ! Función analítica dada en el TP1
    ! y(x) = 10 * exp(-0.05*x^2) * sin(0.9*x)^2 + 0.05*x^2
    real(dp) function y_exacta(x)
        real(dp), intent(in) :: x
        y_exacta = 10.0_dp * exp(-0.05_dp * x**2) * (sin(0.9_dp * x)**2) + 0.05_dp * x**2
    end function y_exacta

end module mod_tp1
