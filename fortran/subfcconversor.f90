subroutine fahrenheit_a_celsius(f, c)
        real, intent(in) :: f
        real, intent(out) :: c
        c = (5.0 / 9.0) * (f - 32.0)
end subroutine fahrenheit_a_celsius
