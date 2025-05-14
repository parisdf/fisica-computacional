subroutine tiempo_de_vuelo(y0, v0, theta, tmax)
    implicit none
    real, intent(in) :: y0, v0, theta
    real, intent(out) :: tmax
    real, parameter :: g = 9.8
    real :: a, b, c, discriminante

    ! Resolver y(t) = 0: 0 = y0 + v0*sin(theta)*t - 0.5*g*t^2
    a = -0.5 * g
    b = v0 * sin(theta)
    c = y0
    discriminante = b**2 - 4*a*c
    if (discriminante < 0) then
        print *, "No hay solución real: el proyectil no cae al suelo"
        stop
    end if
    tmax = (-b + sqrt(discriminante)) / (2*a)
end subroutine tiempo_de_vuelo
