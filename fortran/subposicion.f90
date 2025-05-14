subroutine posicion(t, x0, y0, v0, theta, x, y)
    implicit none
    real, intent(in) :: t, x0, y0, v0, theta
    real, intent(out) :: x, y
    real, parameter :: g = 9.81

    x = x0 + v0 * cos(theta) * t
    y = y0 + v0 * sin(theta) * t - 0.5 * g * t**2
end subroutine posicion
