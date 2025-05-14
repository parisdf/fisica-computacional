subroutine cartesianas_a_polares(x, y, r, theta)
        implicit none
        real, intent(in) :: x, y
        real, intent(out) :: r, theta

        r = sqrt(x**2 + y**2)
        theta = atan2(y, x)
end subroutine cartesianas_a_polares

