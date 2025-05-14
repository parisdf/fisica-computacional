subroutine celsius_a_fahrenheit(c, f)
        real, intent(in) :: c
        real, intent(out) :: f
        f = (9.0 / 5.0) * c + 32.0
end subroutine celsius_a_fahrenheit
